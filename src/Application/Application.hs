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
  , main
  ) where

import Control.Lens ( view, makeLenses )
import Data.UUID
import Graphics.Rendering.OpenGL as GL    (GLuint)
--import Graphics.RedViz.Widget

import App (App(..))

-- import Debug.Trace as DT

data Menu =
    Start
  | PlanetInfo
  deriving Show

data Context =
    Default
  | ContextMenu Menu

instance Show Context where
  show (ContextMenu t) = show t
  show t = show t

data Planet =
    None
  | Earth

data Interface =
     Intro
   | Info Planet
   | Main Context
   | Finished

instance Show Interface where
  show (Main t) = show t
  show t = show t

data Application
  = Application
  {
    _interface  :: Interface
--  , _mainGUI    :: GUI
  , _intro      :: App
  , _main       :: App
  , _planetInfo :: App
  , _hmap       :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
  } deriving Show
$(makeLenses ''Application)

fromApplication :: Application -> App
fromApplication app =
  case view interface app of
    Intro        ->
      view intro app
    Main Default ->
      view main  app
    -- Main (ContextMenu PlanetInfo) ->
    --   view planetInfo app
    Info _ ->
      view planetInfo app
    _ ->
      view main app
