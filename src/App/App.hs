{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
--{-# LANGUAGE OverloadedRecordDot #-}

module App.App
  ( App     (..)
  , Options (..)
  , App.App.options
  , App.App.name
  , App.App.res
  , App.App.gui
  , App.App.objects
  , playCam
  , App.App.cameras
  , selectable
  , selected
  , debug
  , App.App.fromProject
  , App.App.toDrawables
  ) where

import Control.Lens hiding (Empty)
import Foreign.C                     (CInt)
import Linear.Matrix
import Graphics.Rendering.OpenGL     (Program)
                                      
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Drawable as D ( Drawable(Drawable), Uniforms(Uniforms), toDrawables )
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material ( Material )
import Graphics.RedViz.Utils ((<$.>), (<*.>))
import Graphics.RedViz.Project.Project as P ( name, resx, resy, cameras, Project )
import Graphics.RedViz.Project.Utils
import Graphics.RedViz.Widget (format, xoffset, yoffset)
                                      
import Object hiding (Empty)                         
import ObjectTree
import GUI
import Graphics.RedViz.Object as Obj (transform0, options)
import GHC.Float (int2Double)
import Graphics.RedViz (BackendOptions(BackendOptions))

--import Debug.Trace as DT
-- TODO: add camMode here:
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

-- TODO: refactor Project to PreApp : PreApp -> IO App
fromProject :: Project -> GUI -> IO App
fromProject prj0 gui0 = do
  objTree <- ObjectTree.fromProject prj0
  let
    cams = fromProjectCamera prj0 <$> view P.cameras prj0
    pCam = head cams
    res' = (view P.resx prj0, view P.resy prj0)    

    result = 
      App
      { _debug   = (0,0)
      , _options = Options
                   { _name = view P.name prj0
                   , _res  = res'
                   , _test = False }
      , _gui     = gui0    -- introGUI res'
      , _objects = objTree -- TODO abstrace GUI
      , _playCam = pCam
      , _cameras = cams
      , _selectable = []
      , _selected   = [] }

  return result

instance Monoid Double where
  mempty = 0.0
instance Semigroup Double where
  x <> _ = x

toDrawables :: App -> Double -> Object -> [Drawable]
toDrawables app time obj = --drs -- (drs, drs')
    case app ^. App.App.gui . cursor of
      Just cursor'  -> drs
        where
          mpos = (cursor' ^. format . xoffset, cursor' ^. format . yoffset)
          resX = fromEnum $ fst $ view (App.App.options . App.App.res) app :: Int
          resY = fromEnum $ snd $ view (App.App.options . App.App.res) app :: Int
          res' = (toEnum resX, toEnum resY) :: (CInt, CInt)
          cam  = view playCam app :: Camera
          drs  = D.toDrawables mpos time res' cam (obj^.base)
      Nothing -> []
