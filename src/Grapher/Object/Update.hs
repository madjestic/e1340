{-# LANGUAGE Arrows #-}

module Grapher.Object.Update
  ( updateObjectsPre
  ) where

import Control.Lens    hiding (transform)
import Control.Arrow
import Data.List.Index as DLI (indexed)
import FRP.Yampa
import Linear.Matrix   as LM
import Linear.V3       as LV3
import Linear.V4
import Linear.Quaternion hiding (rotate)

import Graphics.RedViz.Utils
import Graphics.RedViz.Object as Obj

import Grapher.Object.Object as Obj
import Solvable

import Debug.Trace as DT (trace)

updateObjectsPre :: [Object] -> SF () [Object]
updateObjectsPre objs0 =
  loopPre objs0 $
  proc objs -> do
    objs1 <- updateObjects objs0 -< ()
    returnA -< (objs1, objs1)

updateObjects :: [Object] -> SF () [Object]
updateObjects objs0 =
  proc _ -> do
    rec objs   <- iPre objs0 -< objs'
        objs'  <- updateObjects' -< objs
    returnA -< objs

updateObjects' :: SF [Object] [Object]
updateObjects' =
  proc objs -> do
    let
      objs' = updateObject <$> objs
    returnA -< objs'

updateObject :: Object -> Object
updateObject obj0 = result
  where
    slvs' = updateSolvers $ obj0 ^. solvers
    mtx0  = obj0 ^. base . transform0 :: M44 Double
    mtx   = extractTransform mtx0 slvs' :: M44 Double
    transforms' = fmap (flip (!*!) mtx) (obj0 ^. base . transforms):: [M44 Double]
    result =
      obj0
      & base . transforms .~ transforms'

updateSolvers :: [Solver] -> [Solver]
updateSolvers slvs = updateSolver <$> slvs

updateSolver :: Solver -> Solver
updateSolver slv0 =
  case slv0 of
      Translate anim cs pos vel      -> translate anim cs pos vel
      Rotate    anim cs pv0 ypr avel -> rotate    anim cs pv0 ypr avel
      _ -> slv0

translate :: Animation -> CoordSys -> V3 Double -> V3 Double -> Solver
translate anim cs pos vel = Translate anim cs (pos + vel) vel

rotate :: Animation -> CoordSys -> V3 Double -> V3 Double -> V3 Double -> Solver
rotate anim cs pv0 ypr avel = Rotate anim cs pv0 (ypr + avel) avel

extractTransform :: M44 Double -> [Solver] -> M44 Double
extractTransform mtx0 slvs = mtx
  where
    mtxs = fmap (extractTransform' mtx0) slvs
    mtx  = foldl (!*!) (LM.identity :: M44 Double) mtxs

extractTransform' :: M44 Double -> Solver -> M44 Double
extractTransform' mtx0 slv =
  case slv of
    Translate _ _ txyz _ -> (LM.identity :: M44 Double) & translation .~ txyz
    Rotate  _ WorldSpace _ ypr0 _ -> mtx
      where
        mtx =
          mkTransformationMat
            rot
            tr
            where
              tr   = V3 0 0 0
              rot0 = (LM.identity :: M33 Double)
              rot  =                                                                                         
                (LM.identity :: M33 Double)                                                             
                !*! fromQuaternion (axisAngle (view _x rot0) (view _x ypr0)) -- yaw  
                !*! fromQuaternion (axisAngle (view _y rot0) (view _y ypr0)) -- pitch
                !*! fromQuaternion (axisAngle (view _z rot0) (view _z ypr0)) -- roll
                
    Rotate  _ ObjectSpace _ ypr0 _ -> mtx
      where
        mtx =
          mkTransformationMat
            rot
            tr
            where
              tr  = V3 0 0 0
              rot0 = mtx0 ^. _m33 
              rot =
                (LM.identity :: M33 Double)
                !*! fromQuaternion (axisAngle (view _x rot0) (view _x ypr0)) -- yaw
                !*! fromQuaternion (axisAngle (view _y rot0) (view _y ypr0)) -- pitch
                !*! fromQuaternion (axisAngle (view _z rot0) (view _z ypr0)) -- roll
    
    _ -> (LM.identity :: M44 Double)

------------------------------
-- Gravity Solver Prototype --
------------------------------
gravitySolver :: SF [Object] [Object]
gravitySolver =
  proc objs0 -> do
    let objs = (gravitySolver' <$> decomp objs0) :: [Object]
    returnA -< objs

decomp :: [a] -> [(a, [a])]
decomp = fmap decomp' . rotatedLists

decomp' :: [a] -> (a, [a])
decomp' xs = (head (take 1 xs), drop 1 xs)

rotatedLists :: [a] -> [[a]]
rotatedLists xs = rotateList' <$> DLI.indexed (replicate (length xs) xs)

gravitySolver' :: (Object, [Object]) -> Object
gravitySolver' (obj0, objs0) = obj
  where
    m0     =  _mass obj0                                   :: Double
    xform0 = head $ obj0 ^. base . transforms             :: M44 Double
    p0     = LM.transpose xform0 ^._w._xyz                 :: V3 Double
                                                            
    ms'    = fmap _mass objs0                              :: [Double]
    xforms = fmap (head . _transforms) $ _base <$> objs0  :: [M44 Double]
    ps'    = fmap ( view (_w._xyz) . LM.transpose) xforms  :: [V3 Double]

    acc = sum $ fmap (gravity p0 m0) (zip ps' ms') :: V3 Double
    -- acc = sum $ fmap (gravity (DT.trace ("p0 :" ++ show p0) p0)
    --                           (DT.trace ("m0 :" ++ show m0 ++ "\n") m0)) (zip (DT.trace ("\n\n ps :" ++ show ps) ps)
    --                                                                   (DT.trace ("ms :" ++ show ms) ms)) :: V3 Double

    s    = 100000000.0*1.0
    s1   = 0.0

    vel'= _velocity obj0*s1 + (acc*s)     :: V3 Double
    mtx =
      mkTransformationMat
      rot
      tr
      where
        rot = view _m33 xform0
        tr  = vel'+ p0

    obj = obj0
          & base . transforms .~ [mtx]
          & velocity .~ vel'

g :: Double
g = 6.673**(-11.0) :: Double

gravity :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravity p0 m0 (p1, m1) = acc
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    dist = norm dir                  :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    acc  = (f / m1) *^ (dir ^/ dist) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       // Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration
