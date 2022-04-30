{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, Arrows #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}

module Object.Object
  ( Object (..)
  , emptyObj
  , defaultObj
  , base
  , nameP
  , materials
  , programs
  , descriptors
  , transforms0
  , transforms1
  , ypr0
  , ypr
  , velocity
  , solvers
  , objectNames
  , time
  ) where

import Control.Lens hiding (transform, pre)
import Linear.Matrix as LM
import Linear (V3(..))

import Graphics.RedViz.Material       as Material
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
  |  Planet
     {
       _base        :: Object'
     , _nameP       :: String
     , _velocity    :: V3 Double
     , _avelocity   :: V3 Double    -- | Angular velocity
     , _mass        :: Double
     , _density     :: Double
     , _solvers     :: [Solver]
     } 
  |  Sprite
     {
       _base        :: Object'
     }
  -- |  Graph
  --    {
  --      _sz     :: Sz2
  --    , _array  :: Array S Ix2 Word8
  --    , _marray :: MArray RealWorld S Ix2 Word8
  --    }
  deriving Show
$(makeLenses ''Object)

emptyObj :: Object
emptyObj =
  Object.Object.Empty
  defaultObject'
  -- (Object' [] [] [] [] 0.0)


defaultObj :: Object
defaultObj =
  Planet
    (Object'
     []
     [defaultMat]
     []
     [(identity::M44 Double)]
     [(identity::M44 Double)]
     (V3 0 0 0 :: V3 Double)
     (V3 0 0 0 :: V3 Double)
     0.0)
    ""
    (V3 0 0 0)
    (V3 0 0 0)
    (1.0)
    (1.0)
    []

objectNames :: Object -> String
--objectNames obj = obj ^.. base . materials . traverse . Material.name
objectNames obj = obj ^. nameP
