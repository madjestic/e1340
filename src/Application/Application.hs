{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Application.Application
  ( Application (..)
  , fromApplication
  , intr
  , opts
  , main
  , info
  , counter
  , gui
  , quit
  ) where

import Control.Lens ( view, makeLenses )
import Data.UUID
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL as GL    (GLuint)

import App (App(..))
import GUI

--import Debug.Trace as DT

data Application
  = Application
  {
    _gui     :: GUI
  , _quit    :: Bool
  , _intr    :: App
  , _main    :: App
  , _opts    :: App
  , _info    :: App
  , _hmap    :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
  , _counter :: MVar Int
  } 
$(makeLenses ''Application)

instance Show (MVar a) where
  show = show

fromApplication :: Application -> App
fromApplication app =
  case view gui app of
    IntrGUI {} -> view intr app
    MainGUI {} -> view main app
    InfoGUI {} -> view info app
    OptsGUI {} -> view opts app
