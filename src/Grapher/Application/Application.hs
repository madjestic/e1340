{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Grapher.Application.Application
  ( Application (..)
  , fromApplication
  , intr
  , main
  , gui
  ) where

import Control.Lens ( view, makeLenses )
import Data.UUID
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL as GL    (GLuint)

import Grapher.App (App(..))
import Grapher.GUI

--import Debug.Trace as DT

data Application
  = Application
  {
    _gui     :: GUI
  , _intr    :: App
  , _main    :: App
  , _hmap    :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
  , _counter :: MVar Int
  } 
$(makeLenses ''Application)

instance Show (MVar a) where
  show t = show t

fromApplication :: Application -> App
fromApplication app =
  case view gui app of
    IntrGUI {} -> view intr app
    MainGUI {} -> view main app
