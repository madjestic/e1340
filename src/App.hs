{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App
  ( App     (..)
  , Options (..)
  , options
  , App.name
  , App.resx
  , App.resy
  , App.objects
  , playCam
  , App.cameras
  , selected
  , initApp
  ) where

import Control.Lens
import Foreign.C                 (CInt)
import Unsafe.Coerce

import Graphics.RedViz.Camera
import Graphics.RedViz.Descriptor
import Object
import Graphics.RedViz.Project     as P
import Graphics.RedViz.Project.Utils
import Graphics.RedViz.Rendering (initVAO)

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
    objTree <- Object.fromProject prj0

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

initApp ::
     (([Int], Int, [Float]) -> IO Descriptor)
  -> Project
  -> IO App
initApp initVAO project =
  do
    putStrLn   "initializing app resources..."
    putStrLn $ "project name : " ++ view P.name project ++ "\n"
    --objTree <- initObjectTree initVAO project
    objTree <- Object.fromProject project

    let
      cams = fromProjectCamera <$> view P.cameras project
      pCam = head cams
      -- camerasP = Utils.fromList <$> toListOf (P.cameras . traverse . pTransform) project
      -- playCamP = head camerasP --fromList $ camerasP!!0
    --pc <- fromVGeo $ fromPGeo pCloud  -- PCloud Point Cloud
    --let objTree = [pc]
    let app =
          App
          (-42,-17)
          ( Options
            name'
            resX'
            resY'
          )
          --Main
          objTree
          pCam
          cams
          []

    print "finished initializing app resources..."
    return app
      where
        name' = view P.name project
        resX' = (unsafeCoerce $ view P.resx project) :: CInt
        resY' = (unsafeCoerce $ view P.resy project) :: CInt
