{-# LANGUAGE Arrows #-}

module Object.Update
  ( updateObjectsPre
  , updateObjectStatic
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
import Graphics.Rendering.OpenGL (DataType(Double))

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
    xform0 = head $ obj0 ^. base . transforms              :: M44 Double
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

updateObjectStatic :: Object -> Object
updateObjectStatic obj0 = obj1
  where
    slvs = obj0 ^. solvers :: [Solver]
    obj1 = foldl solveStatic obj0 slvs

solve :: [Object] -> Object -> Solver -> Object
solve objs0 obj0 slv0 = obj1
  where
    (mtx, vel) = extractTransform objs0 obj0 slv0
    trxs' = fmap (!*! mtx) (obj0 ^. base . transforms):: [M44 Double]
    
    obj1 =
      obj0
      & base . transforms .~ trxs'
      & velocity          .~ vel

type Solvable = (M44 Double, V3 Double)

solveStatic :: Object -> Solver -> Object
solveStatic obj0 slv =
  case slv of
    Translate Static WorldSpace txyz _ -> obj1
      where
        mtx0   = obj0 ^. base . transform0
        mtx1   = (LM.identity :: M44 Double) & translation .~ txyz
        mtx    = mtx0 !*! mtx1
        trxs'  = (!*!) mtx <$> (obj0 ^. base . transforms):: [M44 Double]
        obj1 =
          obj0
          & base . transform0 .~ mtx
          & base . transforms .~ trxs'
    _ -> obj0

solveDynamic :: [Object] -> Object -> Solver -> Object
solveDynamic objs0 obj0 slv =
  case slv of
    Translate Dynamic WorldSpace txyz _ -> obj1
      where
        mtx0   = obj0 ^. base . transform0
        mtx1   = (LM.identity :: M44 Double) & translation .~ txyz
        mtx    = mtx0 !*! mtx1
        trxs   = obj0 ^. base . transforms
        obj1 =
          obj0
          & base . transform0 .~ mtx
          & base . transforms .~ replicate (length trxs) mtx

    Spin -> obj1
      where
        mtx0   = obj0 ^. base . transform0
        
        ypr0 = V3 0 0 (-0.01)
        
        mtx1 =
          mkTransformationMat
            rot
            tr
            where
              tr   = V3 0 0 0
              rot0 = LM.identity :: M33 Double
              rot  = 
                (LM.identity :: M33 Double)
                !*! fromQuaternion (axisAngle (view _x rot0) (view _x ypr0)) -- yaw  
                !*! fromQuaternion (axisAngle (view _y rot0) (view _y ypr0)) -- pitch
                !*! fromQuaternion (axisAngle (view _z rot0) (view _z ypr0)) -- roll

        mtx  = mtx0 !*! mtx1
        trxs = obj0 ^. base . transforms
        
        obj1 =
          obj0
          & base . transform0 .~ mtx
          & base . transforms .~ replicate (length trxs) mtx
          -- & base . transforms .~ fmap (flip (!*!) (mtx1)) trxs

    Rotate  _ WorldSpace _ ypr0 _ -> obj1
      where
        mtx0   = obj0 ^. base . transform0

        rot0 = LM.identity :: M33 Double
        rot  = 
          (LM.identity :: M33 Double)
          !*! fromQuaternion (axisAngle (view _x rot0) (view _x ypr0)) -- yaw  
          !*! fromQuaternion (axisAngle (view _y rot0) (view _y ypr0)) -- pitch
          !*! fromQuaternion (axisAngle (view _z rot0) (view _z ypr0)) -- roll

        mtx = mtx0 & translation .~ (mtx0 ^. translation *! rot)
        
        trxs = obj0 ^. base . transforms

        obj1 =
          obj0
          & base . transform0 .~ mtx
          -- & base . transforms .~ trxs' --fmap (flip (!*!) (mtx1)) trxs'
          & base . transforms .~ replicate (length trxs) mtx

    _ -> obj0
          
          
extractTransform :: [Object] -> Object -> Solver -> Solvable
extractTransform objs0 obj0 slv =
  case slv of
    Translate Static WorldSpace txyz _ -> (mtx, V3 0 0 0)
      where
        mtx = (LM.identity :: M44 Double) & translation .~ txyz
    Translate _ _ txyz _ -> (mtx, V3 0 0 0)
      where
        mtx = (LM.identity :: M44 Double) & translation .~ txyz  
    Rotate  _ WorldSpace _ ypr0 _ -> (mtx, V3 0 0 0)
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
        trxs' = fmap (flip (!*!) mtx) (obj0 ^. base . transforms):: [M44 Double]
        result      =
          obj0 & base . transforms .~ trxs'
        
    Rotate  _ ObjectSpace _ ypr0 _ -> (mtx, V3 0 0 0)
      where
        mtx0 = obj0 ^. base . transform0
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
    
    Gravity -> (mtx, vel)
      where
        av0  = _avelocity obj0
        vel0 = _velocity obj0
        vel  = gvel obj0 (remove obj0 objs0) + vel0
        mtx = 
          mkTransformationMat
          rot
          tr
          where
            tr  = vel :: V3 Double
            rot = LM.identity

    Spin -> (mtx, V3 0 0 0)
      where
        mtx0 = obj0 ^. base . transform0
        tr0  = mtx0 ^. translation
        av0  = _avelocity obj0
        mtx = 
          mkTransformationMat
          rot
          tr
          where
            tr  = V3 0 0 0 :: V3 Double
            rot = LM.identity
                  !*! fromQuaternion (axisAngle (view _x (LM.identity)) (view _x av0)) -- yaw
                  !*! fromQuaternion (axisAngle (view _y (LM.identity)) (view _y av0)) -- pitch
                  !*! fromQuaternion (axisAngle (view _z (LM.identity)) (view _z av0)) -- roll
                  
                  
    _ -> (LM.identity, V3 0 0 0)

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
