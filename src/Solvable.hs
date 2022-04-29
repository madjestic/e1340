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
  , translate
  , rotate
  , toSolver
  ) where

import Control.Lens      hiding (Identity)
import GHC.Float
import Linear.Matrix     hiding (identity)
import Linear.Matrix     as LM
import Linear.V3
import Linear.V4
import Linear.Quaternion hiding (rotate)
import FRP.Yampa         hiding (identity)

import Graphics.RedViz.Utils

import Debug.Trace as DT

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
       _txyz   :: V3 Double
     }
  |  PreTranslate'
     {
       _space :: CoordSys
     , _txyz  :: V3 Double
     }
  |  Translate
     {
       _anim  :: Animation
     , _space :: CoordSys
     , _txyz  :: V3 Double
     }
  |  PreRotate
     {
       _pivot :: V3 Double
     , _ypr   :: V3 Double
     }
  |  PreRotate'
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
     }
  -- |  Rotate' 
  --    {
  --      _anim  :: Animation
  --    , _space :: CoordSys
  --    , _pivot :: V3 Double
  --    , _ypr   :: V3 Double
  --    }
  -- |  RotateConst
  --    {
  --      _pivot :: V3 Double
  --    , _ypr   :: V3 Double
  --    }
  |  Scale
     {
       _sxyz   :: V3 Double
     }
  |  LOD
     {
       _models :: [FilePath]
     }
  |  Gravity
    {
      _idxs :: [Int]
    } deriving Show
$(makeLenses ''Solver)

toSolver :: (String, [Double]) -> Solver
toSolver (solver, parms) =
  --case DT.trace ("toSolver.solver :" ++ show solver) solver of
  case solver of
    -- default - world  (global) space
    -- '       - object (local)  space
    "pretranslate"  -> PreTranslate' WorldSpace  (toV3 parms)
    "pretranslate'" -> PreTranslate' ObjectSpace (toV3 parms)
    "prerotate"     -> PreRotate'   WorldSpace   (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "prerotate'"    -> PreRotate'   ObjectSpace  (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    -- D - dynamic animation (apply every frame,  ypr + matrix update)
    -- S - static  animation (apply a const value (matrix only update))
    "translate"     -> Translate    Dynamic WorldSpace  (toV3 parms)
    "translate'"    -> Translate    Dynamic ObjectSpace (toV3 parms)
    "rotate"        -> Rotate       Dynamic WorldSpace  (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "rotate'"       -> Rotate       Dynamic ObjectSpace (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "translateconst"-> Translate    Static  WorldSpace  (toV3 parms)
    "rotateconst"   -> Rotate       Static  WorldSpace  (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
--    "rotateconst"   -> RotateConst  (toV3 $ take 3 parms) (toV3 $ drop 3 parms)
    "gravity"       -> Gravity      (double2Int <$> parms)
    "identity"      -> Identity
    _               -> Identity

preTransformer :: Solver -> (M44 Double, V3 Double) -> (M44 Double, V3 Double)
preTransformer solver (mtx0, ypr0) = (mtx, ypr')
  where
    --(mtx, ypr) = case solver of
    (mtx, ypr') = case solver of
      PreTranslate' cs offset -> preTranslate cs mtx0 ypr0 offset
      PreRotate'    cs _ ypr1 -> preRotate cs mtx0 ypr0 ypr1
      Identity                -> Solvable.identity' mtx0
      _                       -> (mtx0, ypr0)

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

-- entry point from src/Object/Update.hs
translate :: CoordSys -> M44 Double -> V3 Double -> SF (V3 Double) (M44 Double)
translate cs mtx0 ypr0 =
  proc vel -> do
    let
      rot =
        view _m33 mtx0
        !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr0)) -- yaw
        !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr0)) -- pitch
        !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr0)) -- roll
      
      vel' =
        case cs of
          WorldSpace  -> vel
          ObjectSpace -> vel *! rot

    tr' <- (mtx0 ^. translation +) ^<< integral -< vel'
    let mtx =
          mkTransformationMat
            (view _m33 mtx0)
            tr'

    returnA -< mtx

-- entry point from src/Object/Update.hs
rotate :: CoordSys -> M44 Double -> V3 Double -> SF (V3 Double, V3 Double) (M44 Double, V3 Double)
rotate cs mtx0 ypr0 =
  proc (avel, pv0) -> do
    --ypr' <- (ypr0' +) ^<< integral -< avel
    ypr' <- case cs of
      WorldSpace  -> (V3 0 0 0 +) ^<< integral -< avel
      ObjectSpace -> (ypr0 +)     ^<< integral -< avel

    let mtx =
          mkTransformationMat
            rot
            tr
            where
              rot =
                view _m33 mtx0
                !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
                !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
                !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
              -- rot =
              --   (LM.identity :: M33 Double)
              --   !*! fromQuaternion (axisAngle (view _x (LM.identity :: M33 Double)) (view _x ypr')) -- yaw
              --   !*! fromQuaternion (axisAngle (view _y (LM.identity :: M33 Double)) (view _y ypr')) -- pitch
              --   !*! fromQuaternion (axisAngle (view _z (LM.identity :: M33 Double)) (view _z ypr')) -- roll
              
              --tr  = view (_w._xyz) (transpose mtx0)
              tr = V3 0 0 0 --(-10000000)
              --tr  = view (_w._xyz) (DT.trace ("rotate tr mtx0 : " ++ show mtx0) mtx0)
    returnA -< (mtx !*! mtx0, ypr')
    --returnA -< ((DT.trace ("rotate mtx : " ++ show mtx) mtx), (DT.trace ("rotate ypr' : " ++ show ypr') ypr'))
    --returnA -< (mtx0, ypr0)

-- Object Space Rotation works:
-- rotate :: CoordSys -> M44 Double -> V3 Double -> SF (V3 Double, V3 Double) (M44 Double, V3 Double)
-- rotate cs mtx0 ypr0 =
--   proc (avel, pv0) -> do
--     --ypr' <- (ypr0' +) ^<< integral -< avel
--     ypr' <- case cs of
--       WorldSpace  -> (V3 0 0 0 +) ^<< integral -< avel
--       ObjectSpace -> (ypr0 +)     ^<< integral -< avel

--     let mtx =
--           mkTransformationMat
--             rot
--             tr
--             where
--               rot =
--                 view _m33 mtx0
--                 !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr')) -- yaw
--                 !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr')) -- pitch
--                 !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr')) -- roll
--               -- rot =
--               --   (LM.identity :: M33 Double)
--               --   !*! fromQuaternion (axisAngle (view _x (LM.identity :: M33 Double)) (view _x ypr')) -- yaw
--               --   !*! fromQuaternion (axisAngle (view _y (LM.identity :: M33 Double)) (view _y ypr')) -- pitch
--               --   !*! fromQuaternion (axisAngle (view _z (LM.identity :: M33 Double)) (view _z ypr')) -- roll
              
--               tr  = view (_w._xyz) (transpose mtx0)
--               --tr  = view (_w._xyz) (DT.trace ("rotate tr mtx0 : " ++ show mtx0) mtx0)
--     returnA -< (mtx, ypr')
    
