{-# LANGUAGE Arrows #-}

module Object.Update
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
import Graphics.RedViz.Object as Object

import Object.Object as Obj
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
      objs' = (updateObject objs) <$> objs
    returnA -< objs'

updateXform :: M44 Double -> [M44 Double] -> [Solver] -> V3 Double -> [M44 Double]
updateXform xform0 xforms0 slvs0 v0 = transforms'
  where
    mtx0 = extractTransform xform0 slvs0 :: M44 Double
    mtx1 = mtx0 & translation .~ ((mtx0^. translation) + v0)
    transforms' = fmap (flip (!*!) mtx1) xforms0 :: [M44 Double]

updateVel :: V3 Double -> V3 Double -> V3 Double
updateVel v0 a0 = v0 + a0

updateAccel :: Double -> V3 Double -> V3 Double
updateAccel m0 f0 = f0^/m0

remove :: Object -> [Object] -> [Object]
remove _ [] = []
remove x (y:[]) =
  if x == y then [] else y:[]
remove x (y:ys) =
  if x == y then remove x ys else y : remove x ys

updateGForce :: Object -> [Object] -> V3 Double
updateGForce _ [] = V3 0 0 0
updateGForce obj0 objs0 = acc * 99999
  where
    --m0     =  _mass obj0                                   :: Double
    m0     =  (DT.trace (show $ _mass obj0)_mass obj0)                                    :: Double
    xform0 = head $ obj0 ^. base . transforms              :: M44 Double
    p0     = LM.transpose xform0 ^._w._xyz                 :: V3 Double
    xforms = fmap (head . _transforms) $ _base <$> objs0   :: [M44 Double]
    ps'    = fmap ( view (_w._xyz) . LM.transpose) xforms  :: [V3 Double]
    ms'    = fmap _mass objs0                              :: [Double]
    acc    = sum $ fmap (gravity p0 m0) (zip ps' ms')      :: V3 Double

updateObject :: [Object] -> Object -> Object
updateObject objs obj0@(RBD {}) = result
  where
    slvs0   = updateSolvers $ obj0 ^. solvers :: [Solver]
    xform0  = obj0 ^. base . transform0 :: M44 Double
    xforms0 = obj0 ^. base . transforms :: [M44 Double]
    gforce  = updateGForce obj0 (remove obj0 objs)
    force'  = gforce -- + other forces
    accel'  = updateAccel (_mass obj0)     force'
    vel'    = updateVel   (_velocity obj0) accel'
    transforms' = updateXform xform0 xforms0 slvs0 vel'
    result =
      obj0
      & base . transforms .~ transforms'
      & velocity          .~ vel'

updateObject _ obj0 = result
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
      --Gravity   objids               -> 
      _ -> slv0

translate :: Animation -> CoordSys -> V3 Double -> V3 Double -> Solver
translate anim cs pos vel = Translate anim cs (pos + vel) vel

rotate :: Animation -> CoordSys -> V3 Double -> V3 Double -> V3 Double -> Solver
rotate anim cs pv0 ypr avel = Rotate anim cs pv0 (ypr + avel) avel

extractTransform :: M44 Double -> [Solver] -> M44 Double
extractTransform mtx0 slvs = mtx
  where
    mtxs = fmap (extractTransform' mtx0) slvs
    mtx  = foldl1 (!*!) mtxs

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

g :: Double
g = 6.673**(-11.0) :: Double

gravity :: V3 Double -> Double -> (V3 Double, Double) -> V3 Double
gravity p0 m0 (p1, m1) = acc
  where
    dir  = p1 ^-^ p0                 :: V3 Double
    dist = norm dir                  :: Double
    f    = g * m0 * m1 / dist**2.0   :: Double
    acc  = (f / m0) *^ (dir ^/ dist) :: V3 Double
-- | F = G*@mass*m2/(dist^2);       // Newton's gravity equation
-- | a += (F/@mass)*normalize(dir); // Acceleration
