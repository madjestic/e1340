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
import SDL                                   (distance)

import Graphics.RedViz.Input
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Ctrl
import Graphics.RedViz.Widget (text, Format(..), Alignment(..))

import App.App as App
import ObjectTree
import Object hiding (Empty)
import Camera
import Solvable

-- import Debug.Trace as DT (trace)

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
    selected'   <- updateSelection app' -< cam

    objs        <- updateObjects        filteredLinObjs -< ()
    let objsIntMap = IM.fromList (zip filteredLinObjsIdxs objs)
    
    objs'       <- updateObjects' filteredNonLinObjs -< filteredNonLinObjs
    let
      objs'IntMap  = IM.fromList (zip filteredNonLinObjsIdxs objs')
      selectedText = concat $ objectNames <$> view selected result :: [String]
      app''        = app' & ui  . info . text .~ selectedText
      unionObjs    = IM.union objs'IntMap objsIntMap -- Override GUI somewhere here...
      --objTree = App._objects app' & gui . widgets .~ fromUI (app'^.ui)
      objTree      = App._objects app' & gui . widgets .~ fromUI (app''^.ui)
      result =
        app'
        { App._objects = (objTree {_foreground = snd <$> IM.toList unionObjs})
        , App._cameras = cams
        , _playCam     = cam
        , _selected    = selected' }

    returnA  -< result
    --returnA  -< DT.trace ("updateApp.result.selected : " ++ show (concat $ fmap objectNames $ view selected result)) $ result
      where
        idxObjs    = DLI.indexed $ _foreground (App._objects app')
        intObjMap  = IM.fromList idxObjs :: IntMap Object
        
        filterNonLinIntObjMap  = IM.filter (any (\case Gravity {} -> True; _ -> False) . view Object.solvers) intObjMap
        filteredNonLinObjs     = snd <$> IM.toList filterNonLinIntObjMap
        filteredNonLinObjsIdxs = fst <$> IM.toList filterNonLinIntObjMap

        filterLinIntObjMap  = IM.filter (any (\case Gravity {} -> False; _ -> True) . view Object.solvers) intObjMap
        filteredLinObjs     = snd <$> IM.toList filterLinIntObjMap
        filteredLinObjsIdxs = fst <$> IM.toList filterLinIntObjMap

updateSelection :: App -> SF Camera [Object]
updateSelection app0 =
  switch sf cont
  where
    sf =
      proc cam -> do
        (_, sev) <- selectObjectE (view (objects . foreground) app0)  -< cam

        let
          result = app0 & selected .~ fromEvent sev :: App
        
        returnA -<
            ( app0 ^. selected
            , sev $> result)
    cont = selectObject

selectObject :: App -> SF Camera [Object]
selectObject app0 =
  switch sf cont
  where
    sf =
      proc cam -> do
        (_, sev) <- unselectObjectE (view (objects . foreground) app0)  -< cam

        let
          result = app0 & selected .~ []
        
        returnA -<
            ( app0 ^. selected
            , sev $> result)
    cont = updateSelection

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
