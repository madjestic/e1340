{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Object.Object
  ( Object (..)
  , emptyObj
  , base
  , name
  , idxP
  , materials
  , programs
  , descriptors
  , transforms
  , ypr0
  , ypr
  , velocity
  , avelocity
  , mass
  , solvers
  , trs
--  , objectNames
  , time
  ) where

import Control.Lens hiding (transform, pre)
import Linear (V3(..))

import Solvable
import Graphics.RedViz.Object

--import Debug.Trace    as DT

--------------------------------------------------------------------------------
-- < Object > ------------------------------------------------------------------

data Object
  =  Empty
     {
      _base        :: Object'
     }  -- a unit
  |  RBD
     {
       _base        :: Object'
     , _idxP        :: Integer
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double   -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _solvers     :: [Solver]
     , _trs         :: [V3 Double] -- | Trace - useful for visualising paths, orbits, etc.
     }
  |  Planet
     {
       _base        :: Object'
     , _idxP        :: Integer
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double    -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _solvers     :: [Solver]
     , _trs         :: [V3 Double]
     } 
  |  Sprite
     {
       _base        :: Object'
     }
  deriving Show
$(makeLenses ''Object)

instance Eq Object where
  (==) obj0 obj1 =
    (obj0 ^. base . name  == obj1 ^. base . name) && (_idxP obj0 == _idxP obj1)

emptyObj :: Object
emptyObj =
  Object.Object.Empty
  defaultObject'
