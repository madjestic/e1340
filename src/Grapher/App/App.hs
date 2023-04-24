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
          mpos = (cursor' ^. format . xoffset, cursor' ^. format . yoffset)
          resX = fromEnum $ fst $ view (Grapher.App.App.options . Grapher.App.App.res) app :: Int
          resY = fromEnum $ snd $ view (Grapher.App.App.options . Grapher.App.App.res) app :: Int
          res' = (toEnum resX, toEnum resY) :: (CInt, CInt)
          cam  = view playCam app :: Camera
          drs  = D.toDrawables mpos time res' cam (obj ^. base)
      Nothing -> []

-- type MousePos    = (Double, Double)
-- type Time        = Double
-- type Res         = (CInt, CInt)
-- type CameraM44   = M44 Double
-- type ViewAngle   = Double
-- type FieldOfView = Double

-- toDrawable ::
--      String
--   -> MousePos
--   -> Time
--   -> Res
--   -> Camera
--   -> M44 Double
--   -> BackendOptions
--   -> (Material, Program, Descriptor)
--   -> Drawable
-- toDrawable name mpos time res cam xformO opts (mat, prg, d) = dr
--   where
--     apt    = _apt cam
--     foc    = _foc cam
--     xformC = view (controller . Controllable.transform) cam  :: M44 Double
--     dr  = Drawable name (Uniforms mat prg mpos time res xformC apt foc xformO) d opts

-- toDrawables'
--   :: (Double, Double)
--   -> Double
--   -> (CInt, CInt)
--   -> Camera
--   -> Object -> [Drawable]
-- toDrawables' mpos time0 res0 cam obj = drs
--   where
--     drs = toDrawable name' mpos time0 res0 cam xformO opts'
--           <$> [(mats, progs, ds)
--               | mats  <- obj ^. base . materials
--               , progs <- obj ^. base . programs
--               , ds    <- obj ^. base . descriptors]

--     name'  = obj ^. base . Object.name
--     xformO = obj ^. base . transform0
--     opts'  = obj ^. base . Obj.options :: BackendOptions
--     mats   = obj ^. base . materials   :: [Material]
--     progs  = obj ^. base . programs    :: [Program]
--     ds     = obj ^. base . descriptors :: [Descriptor]

-- toDrawable :: App -> [Object] -> Double -> [Drawable]
-- toDrawable app objs time0 = drs -- (drs, drs')
--   where
--     --mpos = _coords (app ^. Grapher.App.App.gui . cursor)
--     fmt  = app ^. Grapher.App.App.gui . cursor . format
--     mpos = (fmt ^. xoffset, fmt ^. yoffset)
--     resX = fromEnum $ fst $ view (Grapher.App.App.options . Grapher.App.App.res) app :: Int
--     resY = fromEnum $ snd $ view (Grapher.App.App.options . Grapher.App.App.res) app :: Int
--     res' = (toEnum resX, toEnum resY) :: (CInt, CInt)
--     cam  = view playCam app :: Camera
--     drs  = concatMap (toDrawable' mpos time0 res' cam) objs :: [Drawable]

-- toDrawable' :: (Double, Double) -> Double -> (CInt, CInt) -> Camera -> Object -> [Drawable]
-- toDrawable' mpos time0 res0 cam obj = undefined --drs
  -- where
  --   drs      =
  --     (\u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform' ds' ps' name'
  --       -> Drawable name' (Uniforms u_mats' u_prog' u_mouse' u_time' u_res' u_cam' u_cam_a' u_cam_f' u_xform') ds' ps')
  --     <$.> mats <*.> progs <*.> mpos_ <*.> time_ <*.> res_ <*.> cam_ <*.> cam_a_ <*.> cam_f_ <*.> xforms <*.> ds <*.> progs <*.> names

  --   n      = length $ obj ^. base . descriptors :: Int
  --   mpos_  = replicate n mpos  :: [(Double, Double)]
  --   time_  = replicate n time0 :: [Double]
  --   res_   = replicate n res0  :: [(CInt, CInt)]
  --   cam_   = replicate n $ view (controller . Controllable.transform) cam  :: [M44 Double]
  --   cam_a_ = replicate n $ _apt cam :: [Double]
  --   cam_f_ = replicate n $ _foc cam :: [Double]

  --   names  = repeat $ objectNames obj
  --   mats   = obj ^. base . materials :: [Material]
  --   progs  = obj ^. base . programs  :: [Program]
  --   xforms = concat $ replicate n $ obj ^. base . transforms :: [M44 Double]
  --   ds     = obj ^. base . descriptors :: [Descriptor]
