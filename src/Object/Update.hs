{-# LANGUAGE Arrows #-}

module Object.Update
  ( updateObjectsPre
  )where

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
      objs' = updateObject <$> objs
    returnA -< objs'

updateObject :: Object -> Object
updateObject obj0 = result
  where
    slvs' = updateSolvers $ obj0 ^. solvers
    mtx   = extractTransform slvs' :: M44 Double
    transforms' = fmap (flip (!*!) mtx) (obj0 ^. base . transforms):: [M44 Double]
    result =
      obj0
      & base . transforms .~ transforms'

updateSolvers :: [Solver] -> [Solver]
updateSolvers slvs = updateSolver <$> slvs

updateSolver :: Solver -> Solver
updateSolver slv0 =
  case slv0 of
      Translate anim cs vel      -> translate anim cs vel
      Move      anim cs pos vel  -> move      anim cs pos vel
      Rotate    anim cs pv0 avel -> rotate    anim cs pv0 avel
      _ -> slv0

move :: Animation -> CoordSys -> V3 Double -> V3 Double -> Solver
move anim cs pos vel = Move anim cs (pos+vel) vel

translate :: Animation -> CoordSys -> V3 Double -> Solver
translate anim cs vel = Translate anim cs (vel+vel)

rotate :: Animation -> CoordSys -> V3 Double -> V3 Double -> Solver
rotate = undefined

extractTransform :: [Solver] -> M44 Double
extractTransform slvs = mtx
  where
    mtxs = fmap extractTransform' slvs
    mtx = foldl1 (!*!) mtxs

extractTransform' :: Solver -> M44 Double
extractTransform' slv =
  case slv of
    Translate _ _ txyz -> (LM.identity :: M44 Double) & translation .~ txyz
    Move _ _ txyz _    -> (LM.identity :: M44 Double) & translation .~ txyz
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
