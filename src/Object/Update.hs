{-# LANGUAGE Arrows #-}

module Object.Update
  ( updateObjects
  , updateObjects'
  )where

import Control.Lens    hiding (transform)
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
      --(mtxs, yprs) = unzip trs
      (mtxs, yprs) = unzip (DT.trace ("DEBUG trs : " ++ show (fmap (100000*)$ concat $ snd $ unzip trs))trs)
      result =
        obj0 & base . transforms1 .~ vectorizedCompose mtxs
             & base . ypr         .~ (head . head $ yprs)
             & base . Obj.time    .~ time'
    
    returnA -< result
      where
        slvs0 = view solvers obj0

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

transform :: Object -> Solver -> SF () ([M44 Double], [V3 Double])
transform obj0 slv0 =
  proc () ->
    do
      result <- (parB . fmap (transform' slv0 ypr0')) mtxs0 -< ()
      returnA -< unzip result
        where
          mtxs0 = obj0 ^. base . transforms0 :: [M44 Double]
          ypr0' = obj0 ^. base . ypr        :: V3 Double

transform' :: Solver -> V3 Double -> M44 Double -> SF () (M44 Double, V3 Double)
transform' solver ypr0 mtx0 =
  proc () -> do
    state <- case solver of
    --state <- case (DT.trace ("transform' solver : " ++ show solver) solver) of
      Translate Dynamic _ _ -> do
        state' <- case solver of
          Translate _ WorldSpace offset -> do
            tr <- translate WorldSpace mtx0 ypr0  -< offset
            returnA -< (tr, ypr0)
          Translate _ ObjectSpace offset -> do
            tr <- translate ObjectSpace mtx0 ypr0 -< offset
            returnA -< (tr, ypr0)
          _ -> do
            returnA -< (mtx0, ypr0)
        returnA -< state'

      Rotate Dynamic _ _ _ -> do
        state' <- case solver of
          Rotate _ WorldSpace pv0 avel -> do
            (mtx', ypr') <- rotate WorldSpace mtx0 ypr0  -< (avel, pv0)
            --(mtx', ypr') <- rotate WorldSpace (DT.trace ("transform mtx0 : " ++ show mtx0) mtx0) (DT.trace ("transform ypr0 : " ++ show ypr0) ypr0)  -< (avel, pv0)
            returnA -< (mtx', ypr')
          Rotate _ ObjectSpace pv0 avel -> do
            (mtx', ypr') <- rotate ObjectSpace mtx0 ypr0 -< (avel, pv0)
            --(mtx', ypr') <- rotate ObjectSpace (DT.trace ("transform mtx0 : " ++ show mtx0) mtx0) (DT.trace ("transform ypr0 : " ++ show ypr0) ypr0) -< (avel, pv0)
            returnA -< (mtx', ypr')
          _ -> do
            returnA -< undefined                    
        returnA -< state'
        
      _ -> do
        returnA -< (mtx0, ypr0)
    returnA -< state
      
    --   Rotate' Static _ _ _ -> do
    --     state' <- case solver of
    --       Rotate' _ WorldSpace _ _ -> do
    --         (mtx', ypr') <- Solvable.rotate WorldSpace mtx0 pv0 ypr0' ypr1 -< ()
    --         returnA -< (mtx', ypr')
    --       Rotate' _ ObjectSpace _ _ -> do
    --         (mtx', ypr') <- Solvable.rotate ObjectSpace mtx0 pv0 ypr0' ypr1 -< ()
    --         returnA -< (mtx', ypr')
    --       _ -> do
    --         returnA -< undefined                    
    --     returnA -< state'
    --   Rotate' Dynamic _ _ _ -> do
    --     state' <- case solver of
    --       Rotate' _ WorldSpace _ _ -> do
    --         returnA -< undefined        
    --       Rotate' _ ObjectSpace _ _ -> do
    --         returnA -< undefined
    --       _ -> do
    --         returnA -< undefined                    
    --     returnA -< state'
    --   _ ->
    --     do
    --       returnA -< (mtx0, ypr0')

    -- returnA -< state
    --   where
    --     pv0  = undefined
    --     ypr1 = undefined

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
