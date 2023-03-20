{-# LANGUAGE TemplateHaskell #-}

module GUI.GUI
  ( GUI (..)
  , introGUI
  , mainGUI
  , optsGUI  
  , fromGUI
  , infoGUI
  , res
  , cursor
  , fromFormat
  , optsB
  , quitB
  , xx
  , backB
  , strtB
  , speed
  ) where

import Control.Lens

import Graphics.RedViz.Widget
import Graphics.RedViz.Backend

data GUI
  =  IntrGUI
     {
       _res             :: (Int, Int)
     , _cursor          :: Widget
     , _xx              :: Widget
     , _a_space_oddysey :: Widget
     , _strtB           :: Widget
     , _optsB           :: Widget
     , _quitB           :: Widget -- button
     }
  |  OptsGUI
     {
       _res      :: (Int, Int)
     , _cursor   :: Widget
     , _backB    :: Widget -- button
     }
  |  MainGUI
     {
       _res    :: (Int, Int)
     , _fps    :: Widget
     , _speed  :: Widget
     , _cursor :: Widget
     , _gizmo  :: Widget
     }
  |  InfoGUI
     {
       _res    :: (Int, Int)
     , _fps    :: Widget
     , _infos  :: [Widget]
     , _cursor :: Widget
     }
  deriving Show
$(makeLenses ''GUI)
--deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''GUI

fromFormat :: Format -> (Double, Double)
fromFormat (Format alignment_ x_ y_ _ _ _) =
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

defBBox :: BBox
defBBox =
  BBox (-0.2) (0.1) (0.2) (-0.1)

introGUI :: (Int, Int) -> GUI
introGUI res0 =
  IntrGUI
  {
    _res    = res0
  , _cursor = Cursor True "" ((fromIntegral $ fst res0)/2, (fromIntegral $ snd res0)/2) defOpts
  , _xx =
    TextField True ["PARAYA"] 
    (Format TC (-0.19) (-0.2) 0.0 0.08 1.1) defOpts
  , _a_space_oddysey =
    TextField True ["a space odyssey"] 
    (Format TC (-0.2) (-0.25) 0.0 0.03 0.5) defOpts
  , _strtB   = Button True "NEW GAME" defBBox False False
    (Format CC (0.0) ( 0.0) 0.0 0.033 0.5) defOpts
  , _optsB    = Button True "OPTIONS" defBBox False False
    (Format CC (0.0) (-0.075) 0.0 0.033 0.5) defOpts
  , _quitB    = Button True "QUIT"    defBBox False False
    (Format CC (0.0) (-0.15) 0.0 0.033 0.5) defOpts
  }

optsGUI :: (Int, Int) -> GUI
optsGUI res0 =
  OptsGUI
  {
    _res     = res0
  , _cursor  = Cursor True "" (0.0, 0.0) defOpts
  , _backB   = Button True "< BACK" defBBox False False (Format CC (0.0) (0.0) 0.0 0.085 1.0) defOpts
  }

mainGUI :: (Int, Int) -> GUI
mainGUI res0 =
  MainGUI
  {
    _res    = res0
  , _fps    = FPS       True (Format TC (-0.1) (-0.05) (0.0) 0.015 0.2) defOpts
  , _speed  = TextField True ["speed : 0.777"] (Format BC 0.53 0.094 0.0 0.015 0.2) defOpts
  , _cursor = Cursor    True "" ((fromIntegral $ fst res0)/2, (fromIntegral $ snd res0)/2) defOpts
  , _gizmo  = Icon      True "" 1
  }

infoGUI :: (Int, Int) -> GUI
infoGUI res0 =
  InfoGUI
  {
    _res   = res0
  , _fps   = FPS True (Format TC 0.0 (0.0) (0.0) 0.085 1.0) defOpts
  , _infos =
    [ TextField True ["planet ebanat"] (Format BC 0.0 0.0 (0.0) 0.085 1.0) defOpts
    , TextField True ["population: 11,000,000,000 ebanats"] (Format TC (-0.15) (0.0) 0.0 0.085 1.0) defOpts
    ]
  , _cursor = Cursor True "" (0.0, 0.0) defOpts
  }
  
fromGUI :: GUI -> [Widget]
fromGUI gui =
  case gui of
    IntrGUI {} ->
      [
        _cursor gui
      , _xx     gui
      , _a_space_oddysey gui -- ^. _a_space_oddysey
      , _strtB gui
      , _optsB gui
      , _quitB gui
      ]
    OptsGUI {} ->
      [
        _cursor gui 
      , _backB  gui 
      ]
    MainGUI {} ->
      [
        _fps    gui
      , _speed  gui
      , _cursor gui 
      ]
    InfoGUI {} ->
      [
        _fps    gui 
      , _cursor gui     
      ] ++ _infos gui
