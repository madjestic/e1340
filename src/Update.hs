{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Update
  ( updateKeyboard
  , updateKeyboard'
  , updateMouse
  , updateCameras
  , updateCamera
  , updateApp
  , handleExit
  , centerView
  , appRun
  , appIntro
  ) where

import SDL hiding ( (*^), Event, Mouse )
import FRP.Yampa
import Data.IntMap.Lazy         as IM hiding (keys)
import Data.List.Index          as DL        (indexed)
import Data.Functor                          (($>))
import Control.Lens

import Application
import AppInput
import Solvable
import Object
import App
import Camera
import Controllable
import Keyboard
import Mouse
import Utils

updateKeyboard' :: SF (AppInput, Keyboard) (Keyboard, [Event ()])
updateKeyboard' = undefined

-- | ~inspired by foldrWith mtx0 keys - for every keyInput apply a folding transform to mtx0
updateKeyboard :: Keyboard -> SF (AppInput, Keyboard) (Keyboard, [Event ()])
updateKeyboard kbd0 =
  proc (input, kbd) -> do
        (keys', kevs) <- updateKeys (keys kbd0) -< (input, (keys kbd))
        let
          events = [(catEvents kevs) $> ()]
          kbd' = kbd { keys = keys' }

        returnA -< (kbd', events)

updateKeys :: Keys -> SF (AppInput, Keys) (Keys, [Event ()])
updateKeys keys0 =
  proc (input, _) -> do
    (keyW_, keyWe) <- keyEvent SDL.ScancodeW keyW keys0 -< input
    (keyS_, keySe) <- keyEvent SDL.ScancodeS keyS keys0 -< input
    (keyA_, keyAe) <- keyEvent SDL.ScancodeA keyA keys0 -< input
    (keyD_, keyDe) <- keyEvent SDL.ScancodeD keyD keys0 -< input

    (keyQ_, keyQe) <- keyEvent SDL.ScancodeQ keyQ keys0 -< input
    (keyE_, keyEe) <- keyEvent SDL.ScancodeE keyE keys0 -< input
    (keyZ_, keyZe) <- keyEvent SDL.ScancodeZ keyZ keys0 -< input
    (keyC_, keyCe) <- keyEvent SDL.ScancodeC keyC keys0 -< input
    (keyPageUp_,   keyPageUpE)   <- keyEvent SDL.ScancodePageUp   keyPageUp   keys0 -< input
    (keyPageDown_, keyPageDownE) <- keyEvent SDL.ScancodePageDown keyPageDown keys0 -< input

    (keyLShift_, keyLShiftE) <- keyEvent SDL.ScancodeLShift keyLShift keys0 -< input
    (keyLCtrl_ , keyLCtrlE)  <- keyEvent SDL.ScancodeLCtrl  keyLCtrl  keys0 -< input
    (keyLAlt_ , keyLAltE)    <- keyEvent SDL.ScancodeLAlt   keyLAlt   keys0 -< input

    (keyUp_,    keyUpE)    <- keyEvent SDL.ScancodeUp    keyUp    keys0 -< input
    (keyDown_,  keyDownE)  <- keyEvent SDL.ScancodeDown  keyDown  keys0 -< input
    (keyLeft_,  keyLeftE)  <- keyEvent SDL.ScancodeLeft  keyLeft  keys0 -< input
    (keyRight_, keyRightE) <- keyEvent SDL.ScancodeRight keyRight keys0 -< input

    let events = [      keyWe, keySe, keyAe, keyDe, keyQe, keyEe, keyZe, keyCe, keyUpE, keyDownE, keyLeftE,   keyRightE,    keyPageUpE, keyPageDownE, keyLShiftE, keyLCtrlE, keyLAltE ]
        keys'  = ( Keys keyW_  keyS_  keyA_  keyD_  keyQ_  keyE_  keyZ_  keyC_  keyUp_  keyDown_  keyLeft_    keyRight_     keyPageUp_  keyPageDown_  keyLShift_  keyLCtrl_  keyLAlt_ )

    returnA -< (keys', events)

keyEvent :: SDL.Scancode -> (Keys -> Bool) -> Keys -> SF AppInput (Bool, Event ())
keyEvent code keyFunc keys0 =
  proc input -> do
    keyPressed     <- keyInput code  "Pressed"  -< input
    keyReleased    <- keyInput code  "Released" -< input
    let
      result = keyState (keyFunc keys0) keyPressed keyReleased
      event' = lMerge keyPressed keyReleased
    returnA -< (result, event')

keyState :: Bool -> Event () -> Event () -> Bool
keyState state pressed released
  | isEvent pressed  = True
  | isEvent released = False
  | otherwise        = state

updateMouse :: SF AppInput (Mouse, [Event (Double, Double)])
updateMouse =
  proc input -> do
    lmbE <- lbpPos       -< input
    rmbE <- rbpPos       -< input
    mmovE <- mouseMoving -< input

    mpos' <- mousePos     -< input
    rpos' <- mouseRelPos  -< input

    let
      events = [lmbE, rmbE, mmovE]
      mouse' =
        Mouse
        (case isEvent lmbE of
           True -> Just $ fromEvent lmbE
           _    -> Nothing)
        (case isEvent rmbE of
           True -> Just $ fromEvent rmbE
           _    -> Nothing)
        mpos'
        rpos'
        (isEvent mmovE)
        []
    returnA -< (mouse', events)
    --returnA -< (mouse, DT.trace ("mouse :" ++ show events) events)

-- is mouse moving or stopped?
mouseState :: Bool -> Event () -> Event () -> Bool
mouseState state moving stopped
  | isEvent moving  = True
  | isEvent stopped = False
  | otherwise       = state

updateCameraController :: Camera -> SF (AppInput, Camera) Camera
updateCameraController cam0 =
  switch sf cont
  where
    sf =
      proc (input, cam) ->
        do
          (mouse', mevs) <- updateMouse         -< input
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0  :: Double -- | mouse sensiticity scale
            t'       = 2    :: Double -- | mouse idle threshold
            rlag     = 0.95           -- | rotation    stop lag/innertia
            tlag     = 0.9            -- | translation stop lag/innertia
            
            ypr'     =
              (view (controller.ypr) cam +) $
              (0.00001 * (view mouseS cam) *
                (V3 (case (abs mry' <= t') of True -> 0; _ -> (mry' / t') * (abs mry')**s')
                    (case (abs mrx' <= t') of True -> 0; _ -> (mrx' / t') * (abs mrx')**s')
                     0.0) +) $
              foldr1 (+) $
              fmap ( 0.0000001 * scalar * (view keyboardRS cam) *) $ -- <- make it keyboard controllabe: speed up/down            
              zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys kbrd') <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyPageUp,  keyPageDown ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                (mrx', mry') = view rpos mouse'
                pPitch = (keyVecs kbrd')!!6  -- positive  pitch
                nPitch = (keyVecs kbrd')!!7  -- negative  pitch
                pYaw   = (keyVecs kbrd')!!8  -- positive  yaw
                nYaw   = (keyVecs kbrd')!!9  -- negative  yaw
                pRoll  = (keyVecs kbrd')!!10 -- positive  roll
                nRoll  = (keyVecs kbrd')!!11 -- negative  roll
           
                baseSpeed     = 5000
                ctl    = keyLCtrl  $ (keys kbrd')
                shift  = keyLShift $ (keys kbrd')
                alt    = keyLAlt   $ (keys kbrd')
                scalar = s ctl shift alt
                s ctl' shift' alt'
                  | ctl' && shift' = baseSpeed * 2     -- superfast
                  | ctl' && alt'   = baseSpeed * 0.01  -- very slow
                  | shift'         = baseSpeed * 1.5   -- fast
                  | alt'           = baseSpeed * 0.1   -- slow
                  | otherwise      = baseSpeed         -- base speed

          let
            ctl0 = view controller cam -- change cam to cam0 has a drastic difference
            mtx0 = view Controllable.transform ctl0
            rot =
              (view _m33 mtx0)
              !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
              !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
              !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
   
            vel' =
              (view (controller.vel) cam +) $
              foldr1 (+) $
              fmap ( 0.1 * (scalar) * (view keyboardTS cam) *) $ -- <- make it keyboard controllabe: speed up/down
              fmap (transpose (rot) !*) $
              zipWith (*^) ((\x -> if x then (1::Double) else 0) . ($ (keys kbrd')) <$>
                            [keyW, keyS, keyA, keyD, keyQ, keyE])
                            [fVel, bVel, lVel, rVel, uVel, dVel]
   
              where fVel   = (keyVecs kbrd')!!0  -- forwards  velocity
                    bVel   = (keyVecs kbrd')!!1  -- backwards velocity
                    lVel   = (keyVecs kbrd')!!2  -- left      velocity
                    rVel   = (keyVecs kbrd')!!3  -- right     velocity
                    uVel   = (keyVecs kbrd')!!4  -- up        velocity
                    dVel   = (keyVecs kbrd')!!5  -- down      velocity

                    baseSpeed  = 5000000
                    ctl    = keyLCtrl  $ (keys kbrd')
                    shift  = keyLShift $ (keys kbrd')
                    alt    = keyLAlt   $ (keys kbrd')
                    scalar = s ctl shift alt
                    s ctl' shift' alt'
                      | ctl' && shift' && alt' = baseSpeed^(2 :: Integer) * 100 -- superduperfast
                      | ctl' && shift'         = baseSpeed^(2 :: Integer) * 0.5        -- superfast
                      | shift'                 = baseSpeed * 100        -- fast
                      | ctl'                   = baseSpeed * 0.1          -- slow
                      | otherwise              = baseSpeed                -- base speed

          let
            tr'  = (view translation (Controllable._transform (view controller cam)) +) vel'
            mtx' =
              mkTransformationMat
              rot
              tr'
   
          let
            dev' = view (controller.device) cam
            ctl' = (view controller cam)
                   {
                     Controllable._transform = mtx'
                   , Controllable._vel       = vel' * tlag
                   , Controllable._ypr       = ypr' * rlag
                   , Controllable._device    =
                       (dev' { _keyboard = kbrd'
                             , _mouse    = mouse' })
                   }
            result =
              cam {Camera._controller = ctl'}
                       
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController

updateCamera :: Camera -> SF (AppInput, Camera) Camera
updateCamera cam0 = 
  proc (input, _) -> do
    rec cam  <- iPre cam0 -< cam'
        cam' <- updateCameraController cam0 -< (input, cam)
    returnA -< cam

updateCameras :: ([Camera], Camera) -> SF (AppInput, Camera) ([Camera], Camera)
updateCameras (cams0, _) =
  switch sf cont
  where
    sf =
      proc (input, cam') -> do
        cams <- switchCameras cams0 -< ()
        cam  <- updateCamera  $ head cams0 -< (input, cam')

        kev <- keyInput SDL.ScancodeC "Pressed" -< input -- switch camera

        let
          result  = (cams0, cam)  
          result' = (cams,  cam)
          
        returnA -<
          ( result
          , kev $> result' )
          
    cont = updateCameras

switchCameras' :: SF [Camera] [Camera]
switchCameras' =
  proc cams ->
    do
      let result = rotateList 1 cams
      returnA -< result

switchCameras :: [Camera] -> SF () [Camera]
switchCameras cams0 =
  proc _ ->
    do
      let result = rotateList 1 cams0
      returnA -< result

updateApp :: App -> SF AppInput App
updateApp app' =
  proc input -> do
    (cams, cam) <- updateCameras (App._cameras app', App._playCam app') -< (input, App._playCam app')

    objs        <- updateObjects        filteredLinObjs -< ()
    let objsIntMap = IM.fromList (zip filteredLinObjsIdxs objs)
    
    objs'       <- updateObjects' filteredNonLinObjs -< filteredNonLinObjs
    let objs'IntMap = IM.fromList (zip filteredNonLinObjsIdxs objs')

    let
      unionObjs = IM.union objs'IntMap objsIntMap
      objTree = App._objects app'
      result =
        app'
        { App._objects = (objTree {_foreground = snd <$> IM.toList unionObjs})
        , App._cameras = cams
        , _playCam      = cam
        }

    returnA  -< result
      where
        idxObjs    = DL.indexed $ _foreground (App._objects app')
        intObjMap  = IM.fromList idxObjs :: IntMap Object
        
        filterNonLinIntObjMap  = IM.filter (any (\case Gravity {} -> True; _ -> False) . view Object.solvers) intObjMap
        filteredNonLinObjs     = snd <$> IM.toList filterNonLinIntObjMap
        filteredNonLinObjsIdxs = fst <$> IM.toList filterNonLinIntObjMap

        filterLinIntObjMap  = IM.filter (any (\case Gravity {} -> False; _ -> True) . view Object.solvers) intObjMap
        filteredLinObjs     = snd <$> IM.toList filterLinIntObjMap
        filteredLinObjsIdxs = fst <$> IM.toList filterLinIntObjMap

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent

centerView :: SF AppInput Bool
centerView = centerEvent >>^ isEvent

appRun :: Application -> SF AppInput Application
appRun app' =
  loopPre app' $
  proc (input, appState) -> do
    as <- case _interface appState of
            Intro        -> appIntro -< (input, appState)
            Main Default -> appMain app' { _interface =  Main Default } -< input
            _ -> appMain app' { _interface =  Main Default } -< input
    returnA -< (as, as)

appIntro :: SF (AppInput, Application) Application
appIntro = 
  switch sf cont
     where sf =
             proc (input, appState) -> do
               introState <- returnA -< appState
               mainState  <- returnA -< appState { _interface =  Main Default }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after 5.0 () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> mainState)
           cont app' =
             proc _ -> returnA -< app'

appMain :: Application -> SF AppInput Application
appMain app0 = 
  switch sf cont
     where sf =
             proc input -> do
               app'        <- updateApp (fromApplication app0) -< input
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input

               let result = app0 { _main = app' }
               returnA     -< (result, reset $> app0)
               
           cont = appRun
