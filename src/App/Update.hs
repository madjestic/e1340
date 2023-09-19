{-# LANGUAGE Arrows #-}

module App.Update where

import Control.Lens hiding (Empty)
import Data.Functor                          (($>))
import FRP.Yampa
import Linear.V3
import Linear.Matrix
import SDL (distance)
import SDL.Input.Keyboard.Codes as SDL

import Graphics.RedViz.Input
import Graphics.RedViz.Camera.Lens
import Graphics.RedViz.Controllable as Ctrl
import Graphics.RedViz.Widget (text)

import App.App as App
import GUI
import ObjectTree
import Object
import Camera

-- import Debug.Trace as DT (trace)

selectByDist :: Double -> Camera -> [Object] -> [Object]
selectByDist dist cam0 objs0 = selectable'
  where
    camPos      = cam0 ^. controller.Ctrl.transform.translation :: V3 Double
    camPos'     = camPos * (-1)
    selectable' = Prelude.filter (\obj -> distCamPosObj camPos' obj < dist) objs0

updateIntroApp :: App -> SF (AppInput, App) App
updateIntroApp app0 =
 proc (input, app') -> do
   (cams, cam) <- updateCameras    (App._cameras app0, App._playCam app0) -< (input, App._playCam app')
   objs        <- updateObjectsPre (app0 ^. objects . foreground)         -< ()
   gui'        <- updateGUIPre     (app0 ^. gui)                          -< input
    
   --let selectable' = selectByDist (10.0 :: Double) cam objs
   let selectable' = selectByDist (50000000.0 :: Double) cam objs
   selected'    <- updateSelected   app0 -< (input, selectable')

   let
     --selectedText = objectNames <$> view selectable result :: [String]
     objTree      = App._objects app'
     
     result = --app0
       app'
       {
         App._objects = (objTree {_foreground = objs })
       , App._gui         = gui' -- { _inpOpts = inpOpts'
                             -- , _inpQuit = inpQuit' }
       , App._cameras = cams                        
       , _playCam     = cam
       , _selectable  = selectable'
       , _selected    = selected'
       }

   returnA  -< result

updateOptsApp :: App -> SF (AppInput, App) App
updateOptsApp app0 =
 proc (input, app') -> do
   gui'         <- updateGUI (app0 ^. gui)                                 -< input
   let
     -- selectedText = objectNames <$> view selectable result :: [String]
     -- objTree      = App._objects app'
     result =
       app'
       { App._gui         = gui' }

   returnA  -< result

roundTo :: Int -> Double -> Double
roundTo m' n = (fromIntegral . round $ n * m)/m
  where
    m = 10 ** fromIntegral m'

updateMainApp :: App -> SF (AppInput, App) App
updateMainApp app0 =
 proc (input, app') -> do

   (cams, cam) <- updateCameras    (App._cameras app0, App._playCam app0) -< (input, App._playCam app')
   objs        <- updateObjectsPre (app0 ^. objects . foreground)         -< ()
   gui'        <- updateGUIPre (app0 ^. gui)                              -< input
    
   --let selectable' = selectByDist (10.0 :: Double) cam objs
   let selectable' = selectByDist (50000000.0 :: Double) cam objs
   selected'    <- updateSelected   app0 -< (input, selectable')

   let
     --selectedText = objectNames <$> view selectable result :: [String]
     objTree = App._objects app'
     result  =
       app'
       { 
         App._objects = (objTree {_foreground = objs })
       , App._cameras = cams
       , App._gui         = case gui' ^. speed of
           Just speed' -> gui' & speed ?~ (speed' & text .~ [show . roundTo 3 . abs . norm $ (cam ^. controller . vel)])
           Nothing     -> gui' & speed .~ Nothing
       , _playCam     = cam
       , _selectable  = selectable'
       , _selected    = selected'
       }

   returnA  -< result

updateSelected :: App -> SF (AppInput, [Object]) [Object]
updateSelected app0 =
  switch sf cont
  where
    sf =
      proc (input, objs) -> do
        kev <- keyInput SDL.ScancodeZ "Pressed" -< input

        let
          result = app0 & selected .~ objs

        returnA -<
          ( app0 ^. selected
          , kev $> result )
    cont = updateSelected'

updateSelected' :: App -> SF (AppInput, [Object]) [Object]
updateSelected' app0 =
  switch sf cont
  where
    sf =
      proc (input, _) -> do
        kev <- keyInput SDL.ScancodeZ "Pressed" -< input

        let
          result = app0 & selected .~ []

        returnA -<
          ( app0 ^. selected
          , kev $> result )
          
    cont = updateSelected

distCamPosObj :: V3 Double -> Object -> Double
distCamPosObj camPos0 obj0 = dist
  where
    dist    = distance camPos0 objPos 
    objPos  = view translation $ head $ view (base . transforms) obj0 :: V3 Double

testEvent :: SF Options (Event ())
testEvent = arr _test >>> edge
