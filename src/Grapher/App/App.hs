{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE OverloadedRecordDot #-}

module Grapher.App.App
  ( App     (..)
  , Options (..)
  , Grapher.App.App.options
  , Grapher.App.App.name
  , Grapher.App.App.res
  , Grapher.App.App.gui
  , Grapher.App.App.objects
  , intrApp
  , mainApp
  , playCam
  , Grapher.App.App.cameras
  , selectable
  , selected
  , debug
  , Grapher.App.App.fromProject
  , Grapher.App.App.toDrawables
  ) where

import Control.Lens hiding (Empty)
import Foreign.C                     (CInt)
import Linear.Matrix
import Graphics.Rendering.OpenGL     (Program)
                                      
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Drawable as D ( Drawable(Drawable), Uniforms(Uniforms), toDrawables )
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material as M
import Graphics.RedViz.Utils ((<$.>), (<*.>))
import Graphics.RedViz.Project as P
import Graphics.RedViz.Project.Utils
import Graphics.RedViz.Widget (format, xoffset, yoffset, Format(..), defaultFormat)
                                      
import Grapher.Object hiding (Empty)                         
import Grapher.ObjectTree as ObjectTree
import Grapher.GUI
--import Data.Semigroup (Semigroup)
--import GHC.Float (int2Double)

--import Debug.Trace as DT

data App
  = App
  {
    _debug       :: (Double, Double)
  , _options     :: Options
  , _gui         :: GUI
  , _objects     :: ObjectTree
  , _playCam     :: Camera
  , _cameras     :: [Camera]
  , _selectable  :: [Object]
  , _selected    :: [Object]
  } deriving Show

data Options
  = Options
  { _name  :: String
  , _res   :: (Int, Int)
  , _test  :: Bool
  } deriving Show

$(makeLenses ''Options)
$(makeLenses ''App)

-- -- < Init App State > ------------------------------------------------------

fromProject :: Project -> IO (ObjectTree, [Camera], Camera)
fromProject prj0 = do 
  objs <- ObjectTree.fromProject prj0
  let
    cams = (fromProjectCamera prj0) <$> view P.cameras prj0
    pCam = head cams

  return (objs, cams, pCam)    

intrApp :: Project -> IO App
intrApp prj0 = do
  (objTree, cams, pCam) <- Grapher.App.App.fromProject prj0
  let
    result = 
      App
      { _debug   = (0,0)
      , _options = Options
                   { _name = view P.name prj0
                   , _res  = res'
                   , _test = False }
      , _gui     = introGUI res'
      , _objects = objTree
      , _playCam = pCam
      , _cameras = cams
      , _selectable = []
      , _selected   = [] }
      where
        res' = (view P.resx prj0, view P.resy prj0)

  return result

mainApp :: Project -> IO App
mainApp prj0 = do
  (objTree, cams, pCam) <- Grapher.App.App.fromProject prj0
  let
    result =
      App
      { _debug   = (0,0)
      , _options = Options
                   { _name = view P.name prj0
                   , _res  = res'
                   , _test = False }
      , _gui     = mainGUI res' 
      , _objects = objTree
      , _playCam = pCam
      , _cameras = cams
      , _selectable = []
      , _selected   = [] }
      where
        res' = (view P.resx prj0, view P.resy prj0)

  return result

instance Monoid Format where
  mempty = defaultFormat
instance Semigroup Format where
  f <> _ = f

instance Monoid Double where
  mempty = 0.0
instance Semigroup Double where
  x <> _ = x

toDrawables :: App -> Double -> Object -> [Drawable]
toDrawables app time obj = --drs -- (drs, drs')
    case app ^. Grapher.App.App.gui . cursor of
      Just cursor'  -> drs
        where
          resX = fromEnum $ fst $ view (Grapher.App.App.options . Grapher.App.App.res) app :: Int
          resY = fromEnum $ snd $ view (Grapher.App.App.options . Grapher.App.App.res) app :: Int
          res' = (toEnum resX, toEnum resY) :: (CInt, CInt)
          cam  = view playCam app :: Camera
          drs  = D.toDrawables time res' cam (obj ^. base)
      Nothing -> []
