{-# LANGUAGE TemplateHaskell #-}
module Grapher.Graph
  ( Graph (..)
  , sz
  , array
  , marray
  ) where

import Data.Massiv.Array (Array, MArray, S, Ix2, RealWorld, Sz2)
import Data.Word (Word8)
import Control.Lens

data Graph
  =  Graph
  { _sz     :: Sz2
  , _array  :: Array S Ix2 Word8
  , _marray :: MArray RealWorld S Ix2 Word8
  }
$(makeLenses ''Graph)
