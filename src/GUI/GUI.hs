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
     , _startB          :: Widget
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
     , _info   :: Widget
     , _backB  :: Widget -- button
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

introGUI :: (Int, Int) -> GUI
introGUI res =
  IntrGUI
  {
    _res    = res
  , _cursor = Cursor True "" (0.0, 0.0)
  --   _fps    = FPS True (Format TC 0.0 (0.0) 0.025 0.25)
  , _xx =
    TextField True ["XXII"]
    (Format TC (-0.16) (-0.2) 0.0 0.12 2.0)
  , _a_space_oddysey =
    TextField True ["a space odyssey"]
    (Format TC (-0.2) (-0.25) 0.0 0.03 0.5)
  --, _quitB  = Button True "exit" (BBox (0.5) (-0.5) (-0.5) (0.5)) False (Format CC (-0.25) (0.0) 0.085 1.0)
  -- , _startB   = Button True "start"   (BBox (-100) (50) (100) (-50)) False (Format CC (-0.25) (0.0) 0.085 1.0)
  -- , _optionsB = Button True "options" (BBox (-100) (50) (100) (-50)) False (Format CC (-0.35) (0.0) 0.085 1.0)
  , _startB   = Button True "NEW GAME" (BBox (-0.2) (0.1) (0.2) (-0.1)) False False
    (Format CC (0.0) ( 0.0) 0.0 0.033 0.5)
  , _optsB    = Button True "OPTIONS"  (BBox (-0.2) (0.1) (0.2) (-0.1)) False False
    (Format CC (0.0) (-0.075) 0.0 0.033 0.5)
--  , _inpOpts  = False
  , _quitB    = Button True "QUIT"    (BBox (-0.2) (0.1) (0.2) (-0.1)) False False
    (Format CC (0.0) (-0.15) 0.0 0.033 0.5)
--  , _inpQuit  = False
  }

optsGUI :: (Int, Int) -> GUI
optsGUI res =
  OptsGUI
  {
    _res     = res
  , _cursor  = Cursor True "" (0.0, 0.0)
  , _backB   = Button True "< BACK" (BBox (-0.2) (0.1) (0.2) (-0.1)) False False (Format CC (0.0) (0.0) 0.0 0.085 1.0)
--  , _inpBack = False
  }

mainGUI :: (Int, Int) -> GUI
mainGUI res =
  MainGUI
  {
    _res    = res
  , _fps  = FPS True (Format TC (0.0) (0.0) (0.0) 0.085 1.0)
  , _info   = TextField True ["you approach ebanat"] (Format BC 0.0 0.0 0.0 0.085 1.0)
  , _backB  = Button True "< Main" (BBox (-0.2) (0.1) (0.2) (-0.1)) False False (Format CC (0.0) (0.0) 0.0 0.085 1.0)  
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
    --IntrGUI fps info startB optionsB quitB cursor ->
    gui@(IntrGUI {}) ->
      [
        _cursor gui
      --   fps
      , _xx     gui
      , _a_space_oddysey gui -- ^. _a_space_oddysey
      , _startB gui
      , _optsB gui
      , _quitB gui
      -- , cursor
      ]
    OptsGUI res cursor backB ->
      [
        cursor
      , backB
      ]
    MainGUI res fps info backB cursor ->
      [
        fps
      , info
      , backB
      , cursor
      ]
    InfoGUI res fps infos cursor ->
      [
        fps
      , cursor      
      ] ++ infos
