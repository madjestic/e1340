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
  ) where

import Control.Lens
import Data.Maybe (fromJust)

import Graphics.RedViz.Widget

data GUI
  =  IntrGUI
     {
       _res             :: (Int, Int)
     , _cursor          :: Widget
     --   _fps      :: Widget
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
     --, _inpBack  :: Bool
     }
  |  MainGUI
     {
       _res    :: (Int, Int)
     , _fps    :: Widget
     , _speed  :: Widget
     --, _info   :: Widget
     --, _backB  :: Widget -- button
     , _cursor :: Widget
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

fromFormat :: Format -> (Double, Double)
fromFormat fmt@(Format alignment_ x_ y_ _ _ _) =
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
introGUI res =
  IntrGUI
  {
    _res    = res
  --, _cursor = Cursor True "" (0.0, 0.0)
  , _cursor = Cursor True "" ((fromIntegral $ fst res)/2, (fromIntegral $ snd res)/2)
  --   _fps    = FPS True (Format TC 0.0 (0.0) 0.025 0.25)
  , _xx =
    TextField True ["XXII"]
    (Format TC (-0.16) (-0.2) 0.0 0.12 2.0)
  , _a_space_oddysey =
    TextField True ["a space odyssey"]
    (Format TC (-0.2) (-0.25) 0.0 0.03 0.5)
  , _strtB   = Button True "NEW GAME" defBBox False False
    (Format CC (0.0) ( 0.0) 0.0 0.033 0.5)
  , _optsB    = Button True "OPTIONS" defBBox False False
    (Format CC (0.0) (-0.075) 0.0 0.033 0.5)
  , _quitB    = Button True "QUIT"    defBBox False False
    (Format CC (0.0) (-0.15) 0.0 0.033 0.5)
  }

optsGUI :: (Int, Int) -> GUI
optsGUI res =
  OptsGUI
  {
    _res     = res
  , _cursor  = Cursor True "" (0.0, 0.0)
  , _backB   = Button True "< BACK" defBBox False False (Format CC (0.0) (0.0) 0.0 0.085 1.0)
--  , _inpBack = False
  }

mainGUI :: (Int, Int) -> GUI
mainGUI res =
  MainGUI
  {
    _res    = res
  , _fps    = FPS True (Format TC (-0.1) (-0.1) (0.0) 0.03 0.5)
  , _speed  = TextField True ["speed : 0.777"] (Format BC 0.2 0.1 0.0 0.03 0.5)
  --, _info   = TextField True ["you approach ebanat"] (Format BC 0.0 0.0 0.0 0.085 1.0)
  --, _backB  = Button True "< Main" defBBox False False (Format CC (0.0) (0.0) 0.0 0.085 1.0)  
  , _cursor = Cursor True "" (0.0, 0.0)
  }

infoGUI :: (Int, Int) -> GUI
infoGUI res =
  InfoGUI
  {
    _res   = res
  , _fps   = FPS True (Format TC 0.0 (0.0) (0.0) 0.085 1.0)
  , _infos =
    [ TextField True ["planet ebanat"] (Format BC 0.0 0.0 (0.0) 0.085 1.0)
    , TextField True ["population: 11,000,000,000 ebanats"] (Format TC (-0.15) (0.0) 0.0 0.085 1.0)
    ]
  , _cursor = Cursor True "" (0.0, 0.0)
  }
  
fromGUI :: GUI -> [Widget]
fromGUI gui =
  case gui of
    gui@(IntrGUI {}) ->
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
      --, _info   gui 
      , _cursor gui 
      ]
    InfoGUI {} ->
      [
        _fps    gui 
      , _cursor gui     
      ] ++ _infos gui
