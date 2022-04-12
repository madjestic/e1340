{-# LANGUAGE Arrows #-}

module Object.Update
  ( updateObjects
  , updateObjects'
  )where

import Control.Lens    hiding (transform)
import Data.List       as DL  (transpose)
import Data.List.Index as DLI (indexed)
import FRP.Yampa
import Linear.Matrix   as LM
import Linear.V3
import Linear.V4

import Graphics.RedViz.Utils
import Graphics.RedViz.Object as Obj

import Object.Object as Obj
import Solvable

import Debug.Trace as DT (trace)

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
      (mtxs, yprs) = unzip trs
      result =
        obj0 & base . transforms .~ vectorizedCompose mtxs
        --obj0 & base . transforms .~ vectorizedCompose (DT.trace ("Objec.Update mtxs len : " ++ show (length mtxs)) mtxs)
             & base . ypr        .~ (head . head $ yprs)
             & base . Obj.time   .~ time'
    
    returnA -< result
      where
        slvs0 = view solvers obj0

vectorizedCompose :: [[M44 Double]] -> [M44 Double]
vectorizedCompose = fmap (foldr1 (^*^)) . DL.transpose

(^*^) :: M44 Double -> M44 Double -> M44 Double
(^*^) mtx0 mtx1 = mkTransformationMat rot tr
  where
    rot = view _m33 mtx0 !*! view _m33 mtx1 :: M33 Double
    --rot = LM.identity :: M33 Double -- DEBUG
    tr  = view translation mtx0 ^+^ view translation mtx1

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
    m0     =  _mass obj0               :: Double
    xform0 = head $ obj0 ^. base . transforms :: M44 Double
    --p0     = ( view (_w._xyz) . LM.transpose ) xform0 :: V3 Double
    --p0 = undefined :: V3 Double
    p0     = LM.transpose xform0 ^._w._xyz :: V3 Double

    ms'    = fmap _mass objs0          :: [Double]
    xforms = fmap (head . _transforms) $ _base <$> objs0      :: [M44 Double]
    ps'    = fmap ( view (_w._xyz) . LM.transpose) xforms :: [V3 Double]

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

--transform :: Object -> Solver -> SF () [(M44 Double, V3 Double)]
transform :: Object -> Solver -> SF () ([M44 Double], [V3 Double])
transform obj0 slv0 =
  proc () ->
    do
      result <- (parB . fmap (transform' slv0 ypr0')) mtxs0 -< ()
      returnA -< unzip result
        where
          mtxs0 = obj0 ^. base . transforms :: [M44 Double]
          ypr0'  = obj0 ^. base . ypr0 :: V3 Double

transform' :: Solver -> V3 Double -> M44 Double -> SF () (M44 Double, V3 Double)
transform' solver ypr0 mtx0 =
  proc () -> do
    state <- case solver of
      Rotate pv0 ypr0 ->
        do
          --(mtx', ypr') <- Solvable.rotate mtx0 pv0 ypr0 ypr1 -< ()
          (mtx', ypr') <- Solvable.rotate (LM.identity :: M44 Double) pv0 ypr0 ypr1 -< ()
          returnA -< (mtx', ypr')
      Translate _ ->
        do
          mtx' <- translate mtx0 txyz -< ()
          returnA -< (mtx', ypr0)
      Gravity _ ->
        do
          returnA -< (LM.identity :: M44 Double, ypr0)
      _ ->
        do
          returnA -< (mtx0, ypr0)
    returnA -< state
      where
        Rotate     pv0 ypr1 = solver
        Translate  txyz     = solver

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
