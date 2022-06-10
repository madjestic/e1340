{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module App.App
  ( App     (..)
  , Options (..)
  , options
  , App.App.name
  , App.App.res
  , App.App.gui
  , App.App.objects
  , playCam
  , App.App.cameras
  , selectable
  , selected
  , debug
  , inpQuit
  , ui
  , App.App.fromProject
  , toDrawable
  , App.App.Interface (..)
  -- , inpQuit
  -- , inpOpts
  , inpBack
  ) where

import Control.Lens hiding (Empty)
import Foreign.C                     (CInt)
import Linear.Matrix
import Unsafe.Coerce
import Graphics.Rendering.OpenGL     (Program)
                                      
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable as Controllable
import Graphics.RedViz.Drawable
import Graphics.RedViz.Descriptor
import Graphics.RedViz.Input.Mouse
import Graphics.RedViz.Material as M
import Graphics.RedViz.Utils ((<$.>), (<*.>))
import Graphics.RedViz.Project as P
import Graphics.RedViz.Project.Utils
                                      
import Object hiding (Empty)                         
import ObjectTree
import GUI
import Application.Interface as AI

-- import Debug.Trace as DT

data Interface =
    Intro
    {
      _inpQuit :: Bool
    , _inpOpts :: Bool
    }
  | Opts
    { _inpBack :: Bool
    } deriving Show
$(makeLenses ''App.App.Interface)  

data App
  = App
  {
    _debug       :: (Double, Double)
  --, _inpQuit     :: Bool
  , _ui          :: App.App.Interface
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

introApp :: Project -> ObjectTree -> Camera -> [Camera] -> App
introApp prj0 objTree pCam cams =
  App
  { _debug   = (0,0)
  , _ui      = Intro {_inpQuit = False
                     ,_inpOpts = False}
  , _options = Options
               { _name = view P.name prj0
               , _res  = res
               , _test = False }
  , _gui     = introGUI res 
  , _objects = objTree
  , _playCam = pCam
  , _cameras = cams
  , _selectable = []
  , _selected   = [] }
  where
    res@(resx, resy) = (view P.resx prj0, view P.resy prj0)

optsApp :: Project -> ObjectTree -> Camera -> [Camera] -> App
optsApp prj0 objTree pCam cams =
  App
  { _debug   = (0,0)
  , _ui      = Opts {_inpBack = False}
  , _options = Options
               { _name = view P.name prj0
               , _res  = res
               , _test = False }
  , _gui     = optsGUI res 
  , _objects = objTree
  , _playCam = pCam
  , _cameras = cams
  , _selectable = []
  , _selected   = [] }
  where
    res@(resx, resy) = (view P.resx prj0, view P.resy prj0)

mainApp :: Project -> ObjectTree -> Camera -> [Camera] -> App
mainApp prj0 objTree pCam cams =
  App
  { _debug   = (0,0)
  , _ui      = Intro {_inpQuit = False}
  , _options = Options
               { _name = view P.name prj0
               , _res  = res
               , _test = False }
  , _gui     = mainGUI res 
  , _objects = objTree
  , _playCam = pCam
  , _cameras = cams
  , _selectable = []
  , _selected   = [] }
  where
    res@(resx, resy) = (view P.resx prj0, view P.resy prj0)


fromProject :: Project -> AI.Interface -> IO App
fromProject prj0 ui = do
  putStrLn   "initializing IntroApp resources..."
  putStrLn $ "project name : " ++ view P.name prj0 ++ "\n"
  objTree <- ObjectTree.fromProject prj0
       
  let
    cams = fromProjectCamera <$> view P.cameras prj0
    pCam = head cams
    app  = case ui of
      IntroApp        -> introApp prj0 objTree pCam cams
      MainApp Default -> mainApp  prj0 objTree pCam cams
      OptionsApp      -> optsApp  prj0 objTree pCam cams
       
  print "finished initializing IntroApp resources..."
  return app

toDrawable :: App -> [Object] -> Double -> [Drawable]
toDrawable app objs time0 = drs -- (drs, drs')
  where
    mpos = unsafeCoerce $ view (playCam . controller . device' . mouse . pos) app -- :: (Double, Double)
    resX = fromEnum $ fst $ view (options . App.App.res) app :: Int
    resY = fromEnum $ snd $ view (options . App.App.res) app :: Int
    res  = (toEnum resX, toEnum resY) :: (CInt, CInt)
    cam  = view playCam app :: Camera
    drs  = concatMap (toDrawable' mpos time0 res cam) objs :: [Drawable]

toDrawable' :: (Double, Double) -> Double -> (CInt, CInt) -> Camera -> Object -> [Drawable]
toDrawable' mpos time0 res cam obj = drs
  where
    drs      =
      (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' ds' ps' name'
        -> Drawable name' (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform') ds' ps')
      <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> cam_a_ <*.> cam_f_ <*.> xforms <*.> ds <*.> progs <*.> names

    n      = length $ obj ^. base . descriptors :: Int
    mpos_  = replicate n mpos  :: [(Double, Double)]
    time_  = replicate n time0 :: [Double]
    res_   = replicate n res   :: [(CInt, CInt)]
    cam_   = replicate n $ view (controller . Controllable.transform) cam  :: [M44 Double]
    cam_a_ = replicate n $ _apt cam :: [Double]
    cam_f_ = replicate n $ _foc cam :: [Double]

    names  = repeat $ objectNames obj
    mats   = obj ^. base . materials :: [Material]
    progs  = obj ^. base . programs  :: [Program]
    xforms = concat $ replicate n $ obj ^. base . transforms :: [M44 Double]
    ds     = obj ^. base . descriptors :: [Descriptor]
