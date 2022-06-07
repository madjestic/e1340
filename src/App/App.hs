{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module App.App
  ( App     (..)
  , Options (..)
--  , UI      (..)
  , options
  , App.App.name
  , App.App.res
  -- , App.App.resx
  -- , App.App.resy
  , App.App.gui
  , App.App.objects
  , playCam
  , App.App.cameras
  , selectable
  , selected
  , debug
  , inpQuit
  , App.App.fromProject
  , toDrawable
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
import Graphics.RedViz.Project       as P
import Graphics.RedViz.Project.Utils
                                      
import Object hiding (Empty)                         
import ObjectTree
import GUI

-- import Debug.Trace as DT

data App
  = App
  {
    _debug       :: (Double, Double)
  , _inpQuit     :: Bool
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
        False
        ( Options
          ( view P.name prj0)
          ( view P.resx prj0
          , view P.resy prj0)
          False
        )
        Empty
        objTree
        pCam
        cams
        []
        []


    print "finished initializing app resources..."
    return app
      where
        -- name' = view P.name prj0
        -- resX' = (unsafeCoerce $ view P.resx prj0) :: CInt
        -- resY' = (unsafeCoerce $ view P.resy prj0) :: CInt

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
