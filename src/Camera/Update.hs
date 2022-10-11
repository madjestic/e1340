{-# LANGUAGE Arrows #-}

module Camera.Update
  ( updateCameraController
  , updateCameras
  , updateCamera
  ) where

import Control.Lens
import Data.Functor        (($>))
import FRP.Yampa
import Linear.Matrix as LM
import SDL hiding          ((*^), (^+^), (^-^), (^/), norm, Event, Mouse)
import Data.Ord

import Graphics.RedViz.Camera       as Camera
import Graphics.RedViz.Input.FRP.Yampa.AppInput
import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Input
import Graphics.RedViz.Input.FRP.Yampa.Update
import Graphics.RedViz.Utils

import Debug.Trace    as DT

clampBy :: Double -> Double -> Double
clampBy d x = d'
  where
    d' = case signum x of
           (-1) -> clamp (x, 0) $ x + d
           1    -> clamp (0, x) $ x - d
           _    -> x

updateCameraController :: Camera -> SF (AppInput, Camera) Camera
updateCameraController cam0 =
  switch sf cont
  where
    sf =
      proc (input, cam) ->
        do
          (mouse', mevs) <- updateMouse -< input
          (kbrd',  kevs) <- updateKeyboard (view (controller.device.keyboard) cam0) -< (input, (view (controller.device.keyboard) cam))

          let
            s'       = 1.0  :: Double -- | mouse sensitivity scale
            t'       = 2    :: Double -- | mouse idle threshold
                                      -- | inactive radius
            rad      = (0.1 *) $ fromIntegral $ snd $ cam0^.res :: Double 
            rlag     = 0.95           -- | rotation    stop lag/innertia
            tlag     = 0.9            -- | translation stop lag/innertia

            -- | compute rotation velocity = length (mouseP - originP) * scalar
            (mrx', mry') = mouse' ^. rpos
            (mx' , my')  = mouse' ^. pos
            (cx  , cy)   = ( fromIntegral $ (fst $ cam0^.res) `div` 2
                           , fromIntegral $ (snd $ cam0^.res) `div` 2)
            mdist        = abs $ norm (V3 (mx' - cx) (my' - cy) (0.0))
            
            ypr'     =
              (view (controller.ypr) cam +) $
              (0.00001 * (view mouseS cam) *
                -- (V3 (case (abs mry' <= t') of True -> 0; _ -> (mry' / t') * (abs mry')**s')
                --     (case (abs mrx' <= t') of True -> 0; _ -> (mrx' / t') * (abs mrx')**s')
                --      0.0) +) $
                (V3
                 (case (mdist <= rad) of True -> 0; _ -> (clampBy rad $ my' - cy)**s')
                 (case (mdist <= rad) of True -> 0; _ -> (clampBy rad $ mx' - cx)**s')
                  0.0) +) $
              foldr1 (+) $
              fmap ( 0.0000001 * scalar * (view keyboardRS cam) *) $ -- <- make it keyboard controllabe: speed up/down            
              zipWith (*^) ((\x -> if x then (1.0::Double) else 0) . ($ keys kbrd') <$>
                            [ keyUp,  keyDown, keyLeft, keyRight, keyPageUp,  keyPageDown ])
                            [ pPitch, nPitch,  pYaw,    nYaw,     pRoll, nRoll ]
              where
                --(mrx', mry') = view rpos mouse'
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
              fmap (LM.transpose (rot) !*) $
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
                             --, _mouse    = mouse'
                             })
                   }
            result =
              cam {Camera._controller = ctl'}
                       
          returnA -<
            ( result
            , catEvents (kevs ++ ((tagWith ()) <$> mevs)) $> result )

    cont = updateCameraController

-- somehwere here is a problem with camera memorisation: 
updateCamera :: Camera -> SF (AppInput, Camera) Camera
updateCamera cam0 = 
  proc (input, _) -> do
    rec cam  <- iPre cam0 -< cam'
        cam' <- updateCameraController cam0 -< (input, cam)
    returnA -< cam

updateCameras :: ([Camera], Camera) -> SF (AppInput, Camera) ([Camera], Camera)
updateCameras (cams0, cam0) =
  switch sf cont
  where
    sf =
      proc (input, cam') -> do
        cams <- switchCameras cams0 -< ()
        --cam  <- updateCamera  $ head cams0 -< (input, cam')
        cam  <- updateCamera  cam0 -< (input, cam')

        kev <- keyInput SDL.ScancodeC "Pressed" -< input -- switch camera

        let
          result  = (cams0, cam)  
          result' = (tail $ cams ++ [cam],  head cams)
          
        returnA -<
          ( result
          , kev $> result' )
          
    cont = updateCameras

switchCameras :: [Camera] -> SF () [Camera]
switchCameras cams0 =
  proc _ ->
    do
      let result = rotateList 1 cams0
      returnA -< result
