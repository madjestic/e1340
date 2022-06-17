{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Application.Application
  ( Application (..)
--  , Context     (..)
--  , GUI         (..)
  , Interface   (..)
  , Planet      (..)
  , fromApplication
--  , fromproject  
  -- , inpQuit
  -- , inpOpts
  , intro
  , opts
  , main
  , info
  , counter
  , interface
  ) where

import Control.Lens ( view, makeLenses )
import Data.UUID
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL as GL    (GLuint)

import App (App(..))
import Application.Interface

import Debug.Trace as DT

data Application
  = Application
  {
    _interface :: Interface
  , _intro   :: App
  , _main    :: App
  , _opts    :: App
  , _info    :: App
  , _hmap    :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
  , _counter :: MVar Int
  } 
$(makeLenses ''Application)

instance Show (MVar a) where
  show t = show t

fromApplication :: Application -> App
fromApplication app =
  case view interface app of
    IntrApp _ _ -> view intro app
    MainApp     -> view main  app
    InfoApp _   -> view info app
    OptsApp _   -> view opts app
    _ ->
      view main app
