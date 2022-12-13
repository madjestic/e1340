{-# LANGUAGE Arrows, LambdaCase #-}

module Object.Update
  ( updateObjectsPre
  , updateOnce
  ) where

import Control.Lens    hiding (transform)
import Control.Arrow
import FRP.Yampa
import Linear.Matrix   as LM
import Linear.V3       as LV3
import Linear.V4
import Linear.Quaternion hiding (rotate)

import Graphics.RedViz.Object as Object

import Object.Object as Obj
import Solvable

import Debug.Trace as DT (trace)

updateObjectsPre :: [Object] -> SF () [Object]
updateObjectsPre objs0 =
  loopPre objs0 $
  proc _ -> do
    objs1 <- updateObjects objs0 -< ()
    returnA -< (objs1, objs1)

updateObjects :: [Object] -> SF () [Object]
updateObjects objs0 =
  proc _ -> do
    rec objs   <- iPre objs0     -< objs'
        objs'  <- updateObjects' -< objs
    returnA -< objs

updateObjects' :: SF [Object] [Object]
updateObjects' =
  proc objs -> do
    let
      objs' = updateObject objs <$> objs
    returnA -< objs'

remove :: Object -> [Object] -> [Object]
remove _ [] = []
remove x (y:[]) =
  if x == y then [] else y:[]
remove x (y:ys) =
  if x == y then remove x ys else y : remove x ys

gvel :: Object -> [Object] -> V3 Double
gvel _ [] = V3 0 0 0
gvel obj0 objs0 = acc * 99999
  where
    m0     =  _mass obj0                                   :: Double
    xform0 = obj0 ^. base . transform0                     :: M44 Double
    p0     = LM.transpose xform0 ^._w._xyz                 :: V3 Double
    xforms = fmap (head . _transforms) $ _base <$> objs0   :: [M44 Double]
    ps'    = fmap ( view (_w._xyz) . LM.transpose) xforms  :: [V3 Double]
    ms'    = fmap _mass objs0                              :: [Double]
    acc    = sum $ fmap (gravity p0 m0) (zip ps' ms')      :: V3 Double

updateObject :: [Object] -> Object -> Object
updateObject objs0 obj0 = obj1
  where
    slvs = obj0 ^. solvers :: [Solver]
    obj1 = foldl (solveDynamic objs0) obj0 slvs

updateOnce :: Object -> Object
updateOnce obj0 = obj1
  where
    slvs = obj0 ^. solvers :: [Solver]
    obj1 = foldl solveStatic obj0 slvs

type Solvable = (M44 Double, V3 Double)

solveStatic :: Object -> Solver -> Object
solveStatic obj0 slv =
  case slv of
    Translate Static WorldSpace txyz _ -> obj1
      where
        mtx0 = obj0 ^. base . transform0
        mtx1 = (LM.identity :: M44 Double) & translation .~ txyz
        mtx  = mtx0 !*! mtx1
        obj1 = obj0 & base . transform0 .~ mtx
    _ -> obj0

solveDynamic :: [Object] -> Object -> Solver -> Object
solveDynamic objs0 obj0 slv =
  case slv of
    Translate Dynamic WorldSpace txyz _ -> obj1
      where
        mtx0   = obj0 ^. base . transform0
        mtx1   = (LM.identity :: M44 Double) & translation .~ txyz
        mtx    = mtx0 !*! mtx1
        obj1 = obj0 & base . transform0 .~ mtx

    Spin -> obj1
      where
        mtx0  = obj0 ^. base . transform0
        ypr0' = V3 0 0 (-0.035)
        
        mtx1 =
          mkTransformationMat
            rot
            tr
            where
              tr   = V3 0 0 0
              rot0 = LM.identity :: M33 Double
              rot  = 
                (LM.identity :: M33 Double)
                !*! fromQuaternion (axisAngle (view _x rot0) (view _x ypr0')) -- yaw  
                !*! fromQuaternion (axisAngle (view _y rot0) (view _y ypr0')) -- pitch
                !*! fromQuaternion (axisAngle (view _z rot0) (view _z ypr0')) -- roll

        mtx  = mtx0 !*! mtx1
        
        obj1 = obj0 & base . transform0 .~ mtx

    Rotate  _ WorldSpace _ ypr0' _ -> obj1
      where
        mtx0   = obj0 ^. base . transform0
        rot0 = LM.identity :: M33 Double
        rot  = 
          (LM.identity :: M33 Double)
          !*! fromQuaternion (axisAngle (view _x rot0) (view _x ypr0')) -- yaw  
          !*! fromQuaternion (axisAngle (view _y rot0) (view _y ypr0')) -- pitch
          !*! fromQuaternion (axisAngle (view _z rot0) (view _z ypr0')) -- roll
        mtx = mtx0 & translation .~ (mtx0 ^. translation *! rot)
        obj1 = obj0 & base . transform0 .~ mtx

    Orbit pivot qrot idx -> obj1
      where
        pos0  = obj0 ^. (base . transform0 . translation)
        pos1  = lookupObj objs0 idx ^. (base . transform0 . translation)
        axis  = qrot ^._xyz
        angle = qrot ^._w
        tr    = spin pos1 pos0 axis angle
        mtx0  = obj0 ^. base . transform0
        mtx   = mtx0 & translation .~ tr
        obj1  = obj0 & base . transform0 .~ mtx

    Gravity -> obj1
      where
        mtx0 = obj0 ^. base . transform0
        --av0  = _avelocity obj0
        vel0 = _velocity obj0
        vel1 = gvel obj0 (remove obj0 objs0)
        vel  = vel0 + vel1
        mtx  = mtx0 & translation .~ (mtx0 ^. translation + vel)

        obj1 =
          obj0
          & base . transform0 .~ mtx
          & velocity .~ vel

    Trace _ -> obj1
      where
        tr0  = obj0 ^. (base . transform0 . translation) :: V3 Double
        obj1 = obj0 & Obj.trs .~ take 100 (tr0 : obj0 ^. Obj.trs)

    _ -> obj0

spin :: V3 Double -> V3 Double -> V3 Double -> Double -> V3 Double
spin pos0 pos1 axis angle = result
  where
    rot    = (LM.identity :: M33 Double) !*! fromQuaternion (axisAngle axis angle)
    result = (pos1 - pos0) *! rot + pos0

lookupObj :: [Object] -> Integer -> Object
lookupObj objs0 idx = obj
  where
    obj = head $ filter (\obj' -> _idxP obj' == idx ) objs0

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
