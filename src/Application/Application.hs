{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Application.Application
  ( Application (..)
  , Context     (..)
--  , GUI         (..)
  , Interface   (..)
  , Planet      (..)
  , fromApplication
  , inpQuit
  , intro
  , main
  , info
  , counter
  ) where

import Control.Lens ( view, makeLenses )
import Data.UUID
import Control.Concurrent.MVar

import Graphics.Rendering.OpenGL as GL    (GLuint)
--import Graphics.RedViz.Widget

import App (App(..))
--import GUI

import Debug.Trace as DT

data Menu =
    Start
  | PlanetInfo
  deriving Show

data Context =
    Default
  | Debug
  | ContextMenu Menu

instance Show Context where
  show (ContextMenu t) = show t
  show t = show t

data Planet =
    None
  | Earth

data Interface =
    IntroApp
  | OptionsApp
  | InfoApp Planet
  | MainApp Context
  | Finished

instance Show Interface where
  show (MainApp t) = show t
  show t = show t

data Application
  = Application
  {
    _interface :: Interface
  , _inpQuit :: Bool
--  , _gui     :: GUI    
  , _intro   :: App
  , _main    :: App
  , _options :: App
  , _info    :: App
  , _hmap    :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
  , _counter :: MVar Int
  } -- deriving Show
$(makeLenses ''Application)

instance Show (MVar a) where
  show t = show t

fromApplication :: Application -> App
fromApplication app =
  case view interface app of
    IntroApp        ->
      view intro app
    MainApp Default ->
      view main  app
    InfoApp _ ->
      view info app
    _ ->
      view main app
      --view main app
