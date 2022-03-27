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
import Object hiding (Empty)
import Camera
import Solvable

import Debug.Trace as DT (trace)

fromUI :: UI -> [Widget]
fromUI ui' =
  case ui' of
    MainGUI fps' info' ->
      [fps', info']
    _ -> []

updateApp :: App -> SF AppInput App
updateApp app' =
  proc input -> do
-- Something like this, similar to camera switching?    
    (cams, cam) <- updateCameras (App._cameras app', App._playCam app') -< (input, App._playCam app')
    selectable' <- updateSelectable app' -< (cam, input)
    selected'   <- updateSelected   app' -< (input, selectable')

    objs        <- updateObjects        filteredLinObjs -< ()
    let objsIntMap = IM.fromList (zip filteredLinObjsIdxs objs)
    
    objs'       <- updateObjects' filteredNonLinObjs -< filteredNonLinObjs
    let
      objs'IntMap  = IM.fromList (zip filteredNonLinObjsIdxs objs')
      selectedText = objectNames <$> view selectable result :: [String]
      --selectedText = objectNames <$> view selected result :: [String]
      app''        = app' & ui  . info . text .~ selectedText -- SUKA!

      objTree      = App._objects app' & gui . widgets .~ fromUI (app''^.ui)
      unionObjs    = IM.union objs'IntMap objsIntMap
      result =
        app'
        { App._objects = (objTree {_foreground = snd <$> IM.toList unionObjs})
        , App._cameras = cams
        , _playCam     = cam
        , _selectable  = selectable'
        , _selected    = selected'
        }

    --returnA  -< result
    --returnA  -< DT.trace ("updateApp.result.selected : " ++ show (concat $ fmap objectNames $ view selected result)) $ result
    returnA  -< DT.trace ("updateApp.result.selected : "   ++ show (concat $ objectNames <$> view selected   result) ++
                          "updateApp.result.selectable : " ++ show (concat $ objectNames <$> view selectable result)
                         ) result
      where
        idxObjs    = DLI.indexed $ _foreground (App._objects app')
        intObjMap  = IM.fromList idxObjs :: IntMap Object
        
        filterNonLinIntObjMap  = IM.filter (any (\case Gravity {} -> True; _ -> False) . view Object.solvers) intObjMap
        filteredNonLinObjs     = snd <$> IM.toList filterNonLinIntObjMap
        filteredNonLinObjsIdxs = fst <$> IM.toList filterNonLinIntObjMap

        filterLinIntObjMap  = IM.filter (any (\case Gravity {} -> False; _ -> True) . view Object.solvers) intObjMap
        filteredLinObjs     = snd <$> IM.toList filterLinIntObjMap
        filteredLinObjsIdxs = fst <$> IM.toList filterLinIntObjMap

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

updateSelectable :: App -> SF (Camera, AppInput) [Object]
updateSelectable app0 =
  switch sf cont
  where
    sf =
      proc (cam, input) -> do
        (_, sev) <- selectObjectE (view (objects . foreground) app0)  -< cam
        --kev      <- keyInput SDL.ScancodeI "Pressed" -< input

        let
          result = app0 & selectable .~ fromEvent sev :: App
        
        returnA -<
            ( app0 ^. selectable
            , sev $> result)
    cont = selectObject

selectObject :: App -> SF (Camera, AppInput) [Object]
selectObject app0 =
  switch sf cont
  where
    sf =
      proc (cam, _) -> do
        (_, sev) <- unselectObjectE (view (objects . foreground) app0)  -< cam

        let
          result = app0 & selectable .~ []
        
        returnA -<
            ( app0 ^. selectable
            , sev $> result)
    cont = updateSelectable

selectObjectE :: [Object] -> SF Camera ([Object], Event [Object])
selectObjectE objs0 =
  proc cam' -> do
    let
      camPos = cam' ^. controller.Ctrl.transform.translation :: V3 Double
      camPos' = camPos * (-1)
      sortedObjs = sortOn (distCamPosObj (camPos')) $ objs0 :: [Object]
      sortedObjs' = [head sortedObjs]
      objPos     = view translation $ head $ view (base . transforms) $ head sortedObjs :: V3 Double
      dist       = 50000000.0 :: Double
      --dist       = 10.0 :: Double

    proxE <- iEdge True -< distance camPos' objPos <= dist

    let
      result  = objs0
      result' = sortedObjs' :: [Object]

    returnA -< (result, proxE $> result')

unselectObjectE :: [Object] -> SF Camera ([Object], Event [Object])
unselectObjectE objs0 =
  proc cam' -> do
    let
      camPos = cam' ^. controller.Ctrl.transform.translation :: V3 Double
      camPos' = camPos * (-1)
      sortedObjs = sortOn (distCamPosObj (camPos')) $ objs0 :: [Object]
      sortedObjs' = [head sortedObjs]
      objPos     = view translation $ head $ view (base . transforms) $ head sortedObjs :: V3 Double
      dist       = 50000000.0 :: Double
      --dist       = 10.0 :: Double

    proxE <- iEdge True -< distance camPos' objPos > dist

    let
      result  = objs0
      result' = sortedObjs' :: [Object]

    returnA -< (result, proxE $> result')

distCamPosObj :: V3 Double -> Object -> Double
distCamPosObj camPos0 obj0 = dist
  where
    dist    = distance camPos0 objPos 
    objPos  = view translation $ head $ view (base . transforms) obj0 :: V3 Double
