{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App.Update where

import Control.Lens hiding (Empty)
import Data.Functor                          (($>))
import Data.IntMap.Lazy         as IM hiding (keys)
import Data.List.Index          as DLI       (indexed)
import Data.Sort                             (sortOn)
import FRP.Yampa
import Linear.V3
import Linear.Matrix
import SDL (distance)
import SDL.Input.Keyboard.Codes as SDL

import Graphics.RedViz.Input
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Ctrl
import Graphics.RedViz.Widget (text, Format(..), Alignment(..))

import App.App as App
import ObjectTree
import Object as Obj-- (Empty)
import Camera
import Solvable

import Debug.Trace as DT (trace)

fromUI :: UI -> [Widget]
fromUI ui' =
  case ui' of
    MainGUI fps' info' ->
      [fps', info']
    _ -> []

formatDebug' :: App -> String
formatDebug' app0 = -- show (app0 ^. debug) ++
                    "App.Update playCam          : " ++ show (app0 ^. App.playCam . controller . Ctrl.transform . translation) ++ "\n" ++
                    "App.Update obj name         : " ++ show (obj0 ^. nameP) ++ "\n" ++
                    "App.Update obj tr           : " ++ show (head(obj0 ^.base.transforms)^.translation) ++ "\n" ++
                    "App.Update selectable name  : " ++ show (sobj0 ^. nameP) ++ "\n" ++
                    "App.Update selectable tr    : " ++ show sobj0tr ++ "\n"
                    where
                      obj0  = head $ app0 ^. App.objects.foreground :: Object
                      sobj0 = case app0 ^. App.selectable of
                        [] -> Obj.Empty
                        _  -> (head (app0 ^. App.selectable)::Object)
                      sobj0tr = case sobj0 of
                        Obj.Empty -> V3 (-1) (-1) (-1)
                        _ -> (head $ sobj0^.base.transforms::M44 Double)^.translation :: V3 Double
                      -- sobj = case sobj0tr of
                      --   [] -> ""
                      --   _  -> show (head(sobj0tr ^.base.transforms)^.translation)

selectByDist :: Double -> Camera -> [Object] -> [Object]
selectByDist dist cam0 objs0 = selectable
  where
    camPos     = cam0 ^. controller.Ctrl.transform.translation :: V3 Double
    camPos'    = camPos * (-1)
    selectable = Prelude.filter (\obj -> distCamPosObj camPos' obj < dist) objs0

updateApp :: App -> SF (AppInput, App) App
updateApp app0 =
 proc (input, app') -> do
    (cams, cam) <- updateCameras (App._cameras app0, App._playCam app0) -< (input, App._playCam app')
    objs        <- updateObjects        (filteredLinObjs app0) -< ()
    
    --let selectable' = selectByDist (10.0 :: Double) cam objs
    let selectable' = selectByDist (50000000.0 :: Double) cam objs
    selected'    <- updateSelected   app0 -< (input, selectable')

    let objsIntMap = IM.fromList (zip (filteredLinObjsIdxs app') objs)
    
    objs'       <- updateObjects' (filteredNonLinObjs app0) -< (filteredNonLinObjs app')
    let
      objs'IntMap  = IM.fromList (zip (filteredNonLinObjsIdxs app') objs')
      selectedText = objectNames <$> view selectable result :: [String]
      app''        = app' & ui  . info . text .~ selectedText

      objTree      = App._objects app' & gui . widgets .~ fromUI (app''^.ui)
      unionObjs    = IM.union objs'IntMap objsIntMap
      result =
        app'
        { App._objects = (objTree {_foreground = snd <$> IM.toList unionObjs})
          -- App._objects = (objTree {_foreground = objs })
        , App._cameras = cams
        , _playCam     = cam
        , _selectable  = selectable'
        , _selected    = selected'
        }

    returnA  -< result
    --returnA  -< (DT.trace (formatDebug' result) result)
      where
        idxObjs app'    = DLI.indexed $ _foreground (App._objects app')
        intObjMap app'  = IM.fromList $ idxObjs app' :: IntMap Object
        
        filterNonLinIntObjMap app'  = IM.filter (any (\case Gravity {} -> True; _ -> False) . view Obj.solvers) $ intObjMap app'
        filteredNonLinObjs app'     = snd <$> IM.toList (filterNonLinIntObjMap app')
        filteredNonLinObjsIdxs app' = fst <$> IM.toList (filterNonLinIntObjMap app')

        filterLinIntObjMap app'  = IM.filter (any (\case Gravity {} -> False; _ -> True) . view Obj.solvers) (intObjMap app')
        filteredLinObjs app'     = snd <$> IM.toList (filterLinIntObjMap app') 
        filteredLinObjsIdxs app' = fst <$> IM.toList (filterLinIntObjMap app') 

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
