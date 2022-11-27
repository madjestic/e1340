{-# LANGUAGE Arrows #-}

module Grapher.App.Update where

import Control.Lens hiding (Empty)
import Data.Functor                          (($>))
import FRP.Yampa
import Linear.V3
import Linear.Matrix
import SDL (distance)
import SDL.Input.Keyboard.Codes as SDL

import Graphics.RedViz.Input
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Ctrl

import Grapher.App.App as App
import Grapher.GUI
import Grapher.ObjectTree
import Grapher.Object as Obj

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

   objs        <- updateObjectsPre (app0 ^. objects . foreground)         -< ()
   gui'        <- updateGUI (app0 ^. gui)                                 -< input
    
   let
     objTree      = App._objects app'
     
     result =
       app'
       { App._objects = (objTree {_foreground = objs })
       , _gui         = gui'
       }

   returnA  -< result

updateOptsApp :: App -> SF (AppInput, App) App
updateOptsApp app0 =
 proc (input, app') -> do
   gui' <- updateGUI (app0 ^. gui) -< input
    
   let
     result =
       app'
       { _gui         = gui' }

   returnA  -< result

updateMainApp :: App -> SF (AppInput, App) App
updateMainApp app0 =
 proc (input, app') -> do
   objs        <- updateObjectsPre (app0 ^. objects . foreground)         -< ()
   gui'        <- updateGUI (app0 ^. gui)                                 -< input
    
   let
     objTree      = App._objects app'
     
     result =
       app'
       { 
         App._objects = (objTree {_foreground = objs })
       , _gui         = gui'
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
