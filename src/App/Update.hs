{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App.Update where

import Control.Lens hiding (Empty)
import Data.Functor                          (($>))
import Data.IntMap.Lazy         as IM hiding (keys)
import Data.List.Index          as DLI       (indexed)
--import Data.Sort                             (sortOn)
import FRP.Yampa
import Linear.V3
import Linear.Matrix
import SDL (distance)
import SDL.Input.Keyboard.Codes as SDL

import Graphics.RedViz.Input
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Ctrl
import Graphics.RedViz.Widget (text)

import App.App as App
--import Application.Interface
import GUI
import ObjectTree
import Object as Obj-- (Empty)
import Camera
import Solvable
import GUI

import Debug.Trace as DT (trace)

formatDebug' :: App -> String
formatDebug' app0 =
  "App.Cam pos     : " ++ show camPos ++ "\n" ++
  "App.Object name : " ++ show obj0Name ++ "\n" ++
  "App.Object time : " ++ show (obj0 ^. base . Obj.time) ++ "\n" ++
  "App.Object tr   : " ++ show obj0tr ++ "\n" ++
  "App.Object ypr  : " ++ show obj0ypr ++ "\n"
  where
    obj0  = head $ app0 ^. App.objects.foreground :: Object
    obj0Name =
      case obj0 of
        Obj.Empty _ -> "Empty"
        _ -> obj0 ^. nameP
    obj0tr =
      case obj0 of
        Obj.Empty _ -> V3 (-1) (-1) (-1)
        _ -> (head $ obj0^.base.transforms::M44 Double)^.translation :: V3 Double
    obj0ypr =
      case obj0 of
        Obj.Empty _ -> V3 (-1) (-1) (-1)
        _ -> obj0^.base.Obj.ypr :: V3 Double
    camPos =
      app0 ^. playCam . controller . Ctrl.transform . translation  :: V3 Double

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
   gui'        <- updateGUI (app0 ^. gui)                                 -< input
    
   --let selectable' = selectByDist (10.0 :: Double) cam objs
   let selectable' = selectByDist (50000000.0 :: Double) cam objs
   selected'    <- updateSelected   app0 -< (input, selectable')

   let
     selectedText = objectNames <$> view selectable result :: [String]
     objTree      = App._objects app'
     
     result =
       app'
       { App._objects = (objTree {_foreground = objs })
       , App._cameras = cams
       , _gui         = gui' -- { _inpOpts = inpOpts'
                             -- , _inpQuit = inpQuit' }
       , _playCam     = cam
       , _selectable  = selectable'
       , _selected    = selected'
       -- , _ui = IntrApp
       --         { _inpQuit = inpQuit'
       --         , _inpOpts = inpOpts' }
       }

   returnA  -< result

updateOptsApp :: App -> SF (AppInput, App) App
updateOptsApp app0 =
 proc (input, app') -> do

   -- (cams, cam) <- updateCameras    (App._cameras app0, App._playCam app0) -< (input, App._playCam app')
   -- objs        <- updateObjectsPre (app0 ^. objects . foreground)         -< ()
   gui'         <- updateGUI (app0 ^. gui)                                 -< input
    
   --let selectable' = selectByDist (10.0 :: Double) cam objs
   -- let selectable' = selectByDist (50000000.0 :: Double) cam objs
   -- selected'    <- updateSelected   app0 -< (input, selectable')

   let
     selectedText = objectNames <$> view selectable result :: [String]
     objTree      = App._objects app'
     
     result =
       app'
       { --App._objects = (objTree {_foreground = objs })
       --, App._cameras = cams
         _gui         = gui'
       --, _playCam     = cam
       -- , _selectable  = selectable'
       -- , _selected    = selected'
       }

   returnA  -< result

updateMainApp :: App -> SF (AppInput, App) App
updateMainApp app0 =
 proc (input, app') -> do

   (cams, cam) <- updateCameras    (App._cameras app0, App._playCam app0) -< (input, App._playCam app')
   objs        <- updateObjectsPre (app0 ^. objects . foreground)         -< ()
   gui'        <- updateGUI (app0 ^. gui)                                 -< input
    
   --let selectable' = selectByDist (10.0 :: Double) cam objs
   let selectable' = selectByDist (50000000.0 :: Double) cam objs
   selected'    <- updateSelected   app0 -< (input, selectable')

   let
     selectedText = objectNames <$> view selectable result :: [String]
     objTree      = App._objects app'
     
     result =
       app'
       { 
         App._objects = (objTree {_foreground = objs })
       , App._cameras = cams
       , _gui         = gui'
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
