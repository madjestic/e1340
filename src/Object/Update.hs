{-# LANGUAGE Arrows #-}

module Object.Update
  ( updateObjects
  , updateObjects'
  )where

import Control.Lens    hiding (transform)
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

-- | Linear evolving objects (result depends only on initial condition and
-- | can be extrapolated
updateObjects :: [Object] -> SF () [Object]
updateObjects objs0 =
  proc () -> do
    objs' <- parB . fmap solve $ objs0 -< ()
    returnA -< objs'

-- | Objects that evolve over iterations, i.e. non-Linear
updateObjects' :: [Object] -> SF [Object] [Object]
updateObjects' objs0 =
  proc _ -> do
    rec objs   <- iPre objs0 -< objs'
        objs'  <- gravitySolver -< objs
    returnA -< objs

solve :: Object -> SF () Object
solve obj0 =
  proc () -> do
    trs     <- (parB . fmap (transform obj0)) slvs0 -< ()
    time'   <- ((obj0 ^. base . Obj.time :: Double) ^+^) ^<< integral -< (1.0 :: Double)

    let
      (mtx1, ypr1) = postTransform trs
      transforms1' = postRotate ypr1 . (!*!) mtx1 <$> obj0 ^. base . transforms0
      
      result = 
        obj0 & base . transforms1 .~ transforms1'
             & base . transformC  .~ mtx1
             & base . ypr         .~ ypr1
             & base . Obj.time    .~ time'
    
    returnA -< result
      where
        slvs0 = view solvers obj0

postTransform :: [(M44 Double, V3 Double)] -> (M44 Double, V3 Double)
postTransform trs = (mtx, ypr)
  where
    (mtxs, yprs) = unzip trs
    mtx          = foldl1 (!*!) mtxs
    ypr          = sum yprs

postRotate :: V3 Double -> M44 Double -> M44 Double
postRotate ypr0 mtx0 = mtx
  where
    mtx =
      mkTransformationMat
        rot
        tr
        where
          rot =
            view _m33 mtx0
            !*! fromQuaternion (axisAngle (view _x (view _m33 mtx0)) (view _x ypr0)) -- yaw
            !*! fromQuaternion (axisAngle (view _y (view _m33 mtx0)) (view _y ypr0)) -- pitch
            !*! fromQuaternion (axisAngle (view _z (view _m33 mtx0)) (view _z ypr0)) -- roll
          tr  = view (_w._xyz) (transpose mtx0)

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
    xform0 = head $ obj0 ^. base . transforms1             :: M44 Double
    p0     = LM.transpose xform0 ^._w._xyz                 :: V3 Double
                                                            
    ms'    = fmap _mass objs0                              :: [Double]
    xforms = fmap (head . _transforms1) $ _base <$> objs0  :: [M44 Double]
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
          & base . transforms1 .~ [mtx]
          & velocity .~ vel'

transform :: Object -> Solver -> SF () (M44 Double, V3 Double)
transform obj0 slv0 =
  proc () ->
    do
      result <- transform' obj0 slv0 -< obj0
      returnA -< result

transform' :: Object -> Solver -> SF Object (M44 Double, V3 Double)
transform' obj0 slv0 =
  proc obj -> do
      state <- case slv0 of
        Translate _ WorldSpace vel ->
          do
            result  <- translate obj0 -< (obj, vel)
            returnA -< result
        Rotate _ WorldSpace pv0 avel ->
          do
            result  <- rotate obj0 -< (obj, pv0, avel)
            returnA -< result
        _ ->
          do
            returnA -< (LM.identity::M44 Double, V3 0 0 0)
      
      returnA -< state

translate :: Object -> SF (Object, V3 Double) (M44 Double, V3 Double)
translate obj0 = 
  proc (obj, vel) -> do
    tr' <- ((obj0 ^. base . transformC . translation) +) ^<< integral -< vel

    let
      result = obj0 ^. base . transformC & translation .~ tr'
    returnA -< (result, V3 0 0 0)

rotate :: Object -> SF (Object, V3 Double, V3 Double) (M44 Double, V3 Double)
rotate obj0 =
  proc (obj, pv, avel) -> do
       ypr' <- ((obj0 ^. base . ypr) +) ^<< integral -< avel

       returnA -< (LM.identity::M44 Double, ypr')

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
