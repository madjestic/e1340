{-# LANGUAGE TemplateHaskell #-}

module Grapher.GUI.GUI
  ( GUI (..)
  , introGUI
  , mainGUI
  , fromGUI
  , res
  , cursor
  , fromFormat
  ) where

import Control.Lens
import Data.Maybe

import Graphics.RedViz.Widget
import Graphics.RedViz.Backend

data GUI
  =  IntrGUI
     {
       _res             :: (Int, Int)
     , _cursor          :: Maybe Widget
     }
  |  MainGUI
     {
       _res    :: (Int, Int)
     , _fps    :: Maybe Widget
     , _cursor :: Maybe Widget
     }
  deriving Show
$(makeLenses ''GUI)

fromFormat :: Format -> (Double, Double)
fromFormat (Format alignment_ _ _ x_ y_ _ _ _) =
  (\ (x0, y0) (x1,y1) -> (x0+x1, y0+y1)) (x_,y_) $
  case alignment_ of
    TL -> (-1.0, 0.5)
    TC -> ( 0.0, 0.5)
    TR -> ( 1.0, 0.5) -- TODO: adjust the Right alignment by the string length.
    CL -> (-1.0, 0.0)
    CC -> ( 0.0, 0.0)
    CR -> ( 1.0, 0.0)
    BL -> (-1.0,-0.5)
    BC -> ( 0.0,-0.5)
    BR -> ( 1.0, 0.5)

introGUI :: (Int, Int) -> GUI
introGUI res0@(resx,resy) =
  IntrGUI
  {
    _res    = res0
  , _cursor = Just $ Cursor True "" (defaultCursorFormat resx resy) defOpts
  }

mainGUI :: (Int, Int) -> GUI
mainGUI res0@(resx, resy) =
  MainGUI
  {
    _res    = res0
  , _fps    = Just $ FPS True (Format TC resx resy (0.0) (0.0) (0.0) 0.085 1.0) defOpts
  , _cursor = Just $ Cursor True "" (defaultCursorFormat resx resy)  defOpts
  }

fromGUI :: GUI -> [Widget]
fromGUI gui =
  case gui of
    IntrGUI {} ->
      [] ++ maybeToList (_cursor gui)

    MainGUI _ fps' cursor' ->
      []
      ++ maybeToList fps'
      ++ maybeToList cursor'

