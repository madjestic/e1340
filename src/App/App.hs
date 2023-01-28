{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE OverloadedRecordDot #-}

module App.App
  ( App     (..)
  , Options (..)
  , options
  , App.App.name
  , App.App.res
  , App.App.gui
  , App.App.objects
  , intrApp
  , mainApp
  , optsApp
  , playCam
  , App.App.cameras
  , selectable
  , selected
  , debug
  , App.App.fromProject
  , toDrawable
  ) where

import Control.Lens hiding (Empty)
import Foreign.C                     (CInt)
import Linear.Matrix
import Graphics.Rendering.OpenGL     (Program)
                                      
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Drawable
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Material as M
import Graphics.RedViz.Utils ((<$.>), (<*.>))
import Graphics.RedViz.Project as P
import Graphics.RedViz.Project.Utils
                                      
import Object hiding (Empty)                         
import ObjectTree
import GUI
import Graphics.RedViz.Object (transform0)

-- import Debug.Trace as DT

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
  (objTree, cams, pCam) <- App.App.fromProject prj0
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

optsApp :: Project -> IO App
optsApp prj0 = do
  (objTree, cams, pCam) <- App.App.fromProject prj0
  
  let
    result = 
      App
      { _debug   = (0,0)
      , _options = Options
                   { _name = view P.name prj0
                   , _res  = res'
                   , _test = False }
      , _gui     = optsGUI res'
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
  (objTree, cams, pCam) <- App.App.fromProject prj0
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

toDrawable :: App -> [Object] -> Double -> [Drawable]
toDrawable app objs time0 = drs -- (drs, drs')
  where
    mpos = _coords (app ^. App.App.gui . cursor)
    resX = fromEnum $ fst $ view (options . App.App.res) app :: Int
    resY = fromEnum $ snd $ view (options . App.App.res) app :: Int
    res' = (toEnum resX, toEnum resY) :: (CInt, CInt)
    cam  = view playCam app :: Camera
    drs  = concatMap (toDrawable' mpos time0 res' cam) objs :: [Drawable]

toDrawable' :: (Double, Double) -> Double -> (CInt, CInt) -> Camera -> Object -> [Drawable]
toDrawable' mpos time0 res0 cam obj = drs
  where
    drs      =
      (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' ds' ps' name'
        -> Drawable name' (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform') ds' ps')
      <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> cam_a_ <*.> cam_f_ <*.> xforms <*.> ds <*.> progs <*.> names

    n      = length $ obj ^. base . descriptors :: Int
    mpos_  = replicate n mpos  :: [(Double, Double)]
    time_  = replicate n time0 :: [Double]
    res_   = replicate n res0  :: [(CInt, CInt)]
    cam_   = replicate n $ view (controller . Controllable.transform) cam  :: [M44 Double]
    cam_a_ = replicate n $ _apt cam :: [Double]
    cam_f_ = replicate n $ _foc cam :: [Double]

    names  = repeat $ objectNames obj
    mats   = obj ^. base . materials :: [Material]
    progs  = obj ^. base . programs  :: [Program]
    xform0 = obj ^. base . transform0
    xforms = concat $ replicate n $ replicate n xform0 :: [M44 Double]
    ds     = obj ^. base . descriptors :: [Descriptor]
