{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Arrows #-}

module Solvable
  ( Solver (..)
  , CoordSys (..)
  , Animation (..)
  , preTransformer
  , preTranslate
  , preRotate
  , toSolver
  ) where

import Control.Lens      hiding (Identity)
import Linear.Matrix     hiding (identity)
import Linear.V3
import Linear.V4
import Linear.Quaternion hiding (rotate)

import Graphics.RedViz.Utils

--import Debug.Trace as DT

data CoordSys =
    WorldSpace
  | ObjectSpace
  deriving Show

data Animation =
    Static
  | Dynamic
  deriving Show

data Solver =
     Identity
  |  PreTranslate
     {
       _space :: CoordSys
     , _txyz  :: V3 Double
     }
  |  Translate
     {
       _anim  :: Animation
     , _space :: CoordSys
     , _txyz  :: V3 Double
     , _vel   :: V3 Double
     }
  |  PreRotate
     {
       _space :: CoordSys
     , _pivot :: V3 Double
     , _ypr   :: V3 Double
     }
  |  Rotate
     {
       _anim  :: Animation
     , _space :: CoordSys
     , _pivot :: V3 Double
     , _ypr   :: V3 Double
     , _avel  :: V3 Double
     }
  |  Orbit
     {
       _pivot :: V3 Double
     , _qrot  :: V4 Double
     , _ang0  :: Double       -- initial angle-position on orbit
     , _idx   :: Integer
     }
  |  Scale
     {
       _sxyz   :: V3 Double
     }
  |  LOD
     {
       _models :: [FilePath]
     }
  |  Gravity
  |  Trace
     {
       _trs :: [V3 Double]
     }
  deriving Show
$(makeLenses ''Solver)

toSolver :: (String, [Double]) -> Solver
toSolver (solver, parms) =
  --case DT.trace ("toSolver.solver :" ++ show solver) solver of
  case solver of
    -- default - world  (global) space
    -- '       - object (local)  space
    "pretranslate"  -> PreTranslate WorldSpace  (toV3 parms)
    "pretranslate'" -> PreTranslate ObjectSpace (toV3 parms)
    "prerotate"     -> PreRotate   WorldSpace   (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "prerotate'"    -> PreRotate   ObjectSpace  (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    -- Dynamic - dynamic animation (apply every frame,  ypr + matrix update)
    -- Static  - static  animation (apply a const value (matrix only update))
    "translate"     -> Translate    Dynamic WorldSpace  (toV3 parms) (toV3 parms)
    "translate'"    -> Translate    Dynamic ObjectSpace (toV3 parms) (toV3 parms)
    "translateconst"->
      Translate
      {
        _anim  = Static
      , _space = WorldSpace
      , _txyz  = toV3 parms
      , _vel   = toV3 parms
      }
    "rotate"        ->
      Rotate
      { _anim  = Dynamic
      , _space = WorldSpace
      , _pivot = toV3' 0 3 parms
      , _ypr   = toV3' 3 3 parms
      , _avel  = toV3' 6 3 parms
      }
    "rotate'"       ->
      Rotate
      { _anim  = Dynamic
      , _space = ObjectSpace
      , _pivot = toV3' 0 3 parms
      , _ypr   = toV3' 3 3 parms
      , _avel  = toV3' 6 3 parms
      }
    "rotateconst"   -> Rotate       Static  WorldSpace  (toV3 $ take 3 parms) (toV3 $ drop 3 parms) (toV3 parms)
    "gravity"       -> Gravity
    "orbit"         -> Orbit        (toV3 $ take 3 parms) (toV4 $ take 4 $ drop 3 parms) (parms!!7)  (round $ last parms)
    "trace"         -> Trace        []
    "identity"      -> Identity
    _               -> Identity

preTransformer :: Solver -> (M44 Double, V3 Double) -> (M44 Double, V3 Double)
preTransformer solver (mtx0, ypr0) = (mtx, ypr')
  where
    --(mtx, ypr) = case solver of
    (mtx, ypr') = case solver of
      PreTranslate cs offset   -> preTranslate cs mtx0 ypr0 offset
      PreRotate    cs _ ypr1   -> preRotate cs mtx0 ypr0 ypr1
      -- Orbit pivot qrot ang idx -> undefined
      Identity                 -> Solvable.identity' mtx0
      _                        -> (mtx0, ypr0)

-- preSpin :: V3 Double -> V3 Double -> V3 Double -> Double -> (M44 Double, V3 Double)
-- preSpin pos0 pos1 axis angle = result
--    where
--      result = undefined
--     rot    = (LM.identity :: M33 Double) !*! fromQuaternion (axisAngle axis angle)
--     result = (pos1 - pos0) *! rot + pos0

identity :: M44 Double -> M44 Double
identity mtx0 = mtx
  where
    mtx =
      mkTransformationMat
        rot
        tr
        where
          rot = view _m33 mtx0
          tr  = view (_w._xyz) mtx0

identity' :: M44 Double -> (M44 Double, V3 Double)
identity' mtx0 = (mtx, ypr')
  where
    ypr' = V3 0 0 0
    mtx  =
      mkTransformationMat
        rot
        tr
        where
          rot = view _m33 mtx0
          tr  = view (_w._xyz) mtx0

preTranslate :: CoordSys -> M44 Double -> V3 Double -> V3 Double -> (M44 Double, V3 Double)
preTranslate cs mtx0 ypr0 v0 = (mtx, ypr0)
  where
    rot =
      view _m33 mtx0
      !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr0)) -- yaw
      !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr0)) -- pitch
      !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr0)) -- roll

    v0' =
      case cs of
        WorldSpace  -> v0
        ObjectSpace -> v0 *! rot
    
    mtx =
      mkTransformationMat
        rot
        tr
        where
          tr  = v0' + view (_w._xyz) mtx0           

preRotate :: CoordSys -> M44 Double -> V3 Double -> V3 Double -> (M44 Double, V3 Double)
preRotate cs mtx0 ypr0 ypr1 = (mtx, ypr')
    where
      ypr' = ypr0' + ypr1 :: V3 Double
         where
           ypr0' = case cs of
             WorldSpace  -> V3 0 0 0
             ObjectSpace -> ypr0
      mtx  =
          mkTransformationMat
            rot
            tr
            where
              ypr0' = case cs of
                WorldSpace  -> V3 0 0 0
                ObjectSpace -> ypr0
              ypr'' = ypr0' + ypr1                
              rot =
                view _m33 mtx0
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr'')) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr'')) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr'')) -- roll
              tr  = view (_w._xyz) mtx0
