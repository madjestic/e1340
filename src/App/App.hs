{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App.App
  ( App     (..)
  , Options (..)
  , options
  , App.App.name
  , App.App.resx
  , App.App.resy
  , App.App.objects
  , playCam
  , App.App.cameras
  , selected
  , App.App.fromProject
  ) where

import Control.Lens
import Foreign.C                 (CInt)
import Unsafe.Coerce

import Graphics.RedViz.Camera
import Object
import ObjectTree
import Graphics.RedViz.Project     as P
import Graphics.RedViz.Project.Utils

-- import Debug.Trace as DT

data App
  = App
  {
    _debug     :: (Double, Double)
  , _options   :: Options
  , _objects   :: ObjectTree
  , _playCam   :: Camera
  , _cameras   :: [Camera]
  , _selected  :: [Object]
  } deriving Show

data Options
  = Options
  { _name  :: String
  , _resx  :: CInt
  , _resy  :: CInt
  } deriving Show

$(makeLenses ''Options)
$(makeLenses ''App)

-- -- < Init App State > ------------------------------------------------------

fromProject :: Project -> IO App
fromProject prj0 =
  do
    putStrLn   "initializing app resources..."
    putStrLn $ "project name : " ++ view P.name prj0 ++ "\n"
    objTree <- ObjectTree.fromProject prj0

    let
      cams = fromProjectCamera <$> view P.cameras prj0
      pCam = head cams
      app =
        App
        (0,0)
        ( Options
          name'
          resX'
          resY'
        )
        objTree
        pCam
        cams
        []

    print "finished initializing app resources..."
    return app
      where
        name' = view P.name prj0
        resX' = (unsafeCoerce $ view P.resx prj0) :: CInt
        resY' = (unsafeCoerce $ view P.resy prj0) :: CInt
