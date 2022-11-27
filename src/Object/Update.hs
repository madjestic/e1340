{-# LANGUAGE Arrows, LambdaCase #-}

module Object.Update
  ( updateObjectsPre
  , updateOnce
  ) where

import Control.Lens    hiding (transform)
import Control.Arrow
--import Data.List.Index as DLI (indexed)
--import Data.List (delete)
import FRP.Yampa
import Linear.Matrix   as LM
import Linear.V3       as LV3
import Linear.V4
import Linear.Quaternion hiding (rotate)

--import Graphics.RedViz.Utils
import Graphics.RedViz.Object as Object

import Object.Object as Obj
import Solvable

import Debug.Trace as DT (trace)
--import Graphics.Rendering.OpenGL (DataType(Double))

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
  -- case (DT.trace ("obj name : " ++ show (obj0 ^. nameP) ++ "\n" ++
  --                 "slv      : " ++ show (slv)           ++ "\n" ++
  --                 "tr0      : " ++ show (obj0 ^. (base . transform0 . translation)) ++ "\n" ++
  --                 "trace    : " ++ show (obj0 ^. trs)   ++ "\n"
  --                ) slv) of
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

    -- TODO: experiment with the center of rotation: analytic orbiting
    Orbit _ qrot _ -> obj1
      where
        --objs0' = filter orbits' objs0 :: [Object] -- Objects with Orbit Solver
        (vec, angle) = (\q -> (q^._xyz, q^._w)) qrot -- V4 -> (V3,Double)
        mtx0 = obj0 ^. base . transform0
        tr0  = mtx0 ^. translation
        rot  = 
          (LM.identity :: M33 Double)
          !*! fromQuaternion (axisAngle vec angle) -- yaw
        acc  = accOrbits objs0 (Just obj0)
        mtx  = mtx0 & translation .~ ((tr0 - acc) *! rot + acc)
        obj1 = obj0 & base . transform0 .~ mtx

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
        obj1 = obj0 & Obj.trs .~ (tr0 : obj0 ^. Obj.trs)

    _ -> obj0

accOrbits :: [Object] -> Maybe Object -> V3 Double
accOrbits _ Nothing = V3 0 0 0
accOrbits objs0 (Just obj0) = pivot + accOrbits objs0 obj
  where
    --p0    = obj0 ^. base . transform0 . translation
    obj   = obj0 `orbits` objs0
    pivot = case obj of
      Just obj' ->  obj' ^. base . transform0 . translation
      _ -> V3 0 0 0

orbits :: Object -> [Object] -> Maybe Object
orbits obj0 objs = result
  where
    slvs'  = filter (\case Orbit {} -> True; _ -> False) $ obj0 ^. solvers
    result =
      if not . null $ slvs'
      then Just $ lookupObj objs (_idx (head slvs'))
      else Nothing

-- orbits' :: Object -> Bool
-- orbits' obj0 = undefined
--   where
--     slvs'  = filter (\case Orbit {} -> True; _ -> False) $ obj0 ^. solvers
--     result = not . null $ slvs'

-- maybeOrbit1 :: [Object] -> Object -> Maybe (V3 Double, Integer)
-- maybeOrbit1 [] _ = Nothing
-- maybeOrbit1 objs0 obj0 = temp
--   where
--     temp = undefined
--     slvs'
--       = (\slv -> case slv of
--               Orbit {} -> Just $ (pv, idx)
--                 where
--                   pv  = undefined -- maybeOrbit1 objs0 
--                   idx = _idx slv
--               _ -> Nothing
--         ) <$> obj0 ^. solvers
--     result =
--       if not . null $ slvs'
--       then head slvs'
--       else Nothing

lookupObj :: [Object] -> Integer -> Object
lookupObj objs0 idx = obj
  where
    obj = head $ filter (\obj' -> _idxP obj' == idx ) objs0

accumulateOrbitalPivots :: [Object] -> Object -> V3 Double
accumulateOrbitalPivots [] _ = V3 0 0 0
accumulateOrbitalPivots [obj1] obj0 =
  case obj0 `maybeOrbits` obj1 of
    Just pivot -> pivot + accumulateOrbitalPivots [] obj1
    Nothing -> V3 0 0 0
--accumulateOrbitalPivots (obj1:objs) obj0 = pivot' + accumulateOrbitalPivots (delete obj0 objs) obj1
accumulateOrbitalPivots (obj1:objs) obj0 = pivot' + accumulateOrbitalPivots objs obj1
  where pivot' = 
          --case obj0 `maybeOrbits` obj1 of
          case obj0 `maybeOrbits` ( DT.trace (show (obj0 ^. nameP) ++ "`maybeOrbits`" ++ show (obj1 ^. nameP) ++ " : " ++ show (obj0 `maybeOrbits` obj1) ++ "\n" ++
                                              "obj0 ^. nameP : " ++ show (obj0 ^. nameP)   ++ "\n" ++
                                              "obj0 ^. idxP  : " ++ show (_idxP obj0)      ++ "\n" ++
                                              "obj0 ^. base . transform0 . translation : " ++ show (obj0 ^. base . transform0 . translation) ++ "\n" ++
                                              "obj1 ^. nameP : " ++ show (obj1 ^. nameP)   ++ "\n" ++
                                              "obj0 ^. idxP  : " ++ show (_idxP obj1)      ++ "\n" ++
                                              "obj1 ^. base . transform0 . translation : " ++ show (obj1 ^. base . transform0 . translation) ++ "\n" ++
                                              "\n"
                                             ) obj1) of
          --case obj0 `maybeOrbits` (DT.trace ("show (obj1 ^. base . transform0 . translation)" ++ show (obj1 ^. base . transform0 . translation)) obj1) of
            --Just pivot -> pivot
            Just pivot -> (DT.trace ("pivot : " ++ show pivot) pivot)
            Nothing -> V3 0 0 0

-- maybeOrbits' :: [Object] -> Object -> Maybe (V3 Double)
-- maybeOrbits' objs0 obj0 = undefined

-- maybeOrbits'' :: Object -> Object -> Maybe (V3 Double, Object)
-- maybeOrbits'' obj0 obj1 =
--   case maybeOrbit obj0 of
--     Just idx ->
--       if idx == _idxP obj1
--       then Nothing --Just (obj1 ^. base . transform0 . translation) -- obj0 orbits obj1
--       else Nothing
--     Nothing -> Nothing
    
maybeOrbits :: Object -> Object -> Maybe (V3 Double)
maybeOrbits obj0 obj1 =
  case maybeOrbit obj0 of
    Just idx ->
      if idx == _idxP obj1
      then Just (obj1 ^. base . transform0 . translation) -- obj0 orbits obj1
      else Nothing
    Nothing -> Nothing

-- | an object orbits something : index => 0
-- | or Nothing
maybeOrbit :: Object -> Maybe Integer
maybeOrbit obj0 = result
  where
    slvs'
      = (\slv -> case slv of
              Orbit {} -> Just $ _idx slv
              _ -> Nothing
        ) <$> obj0 ^. solvers
    result =
      if not . null $ slvs'
      then head slvs'
      else Nothing
            
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
