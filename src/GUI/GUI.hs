{-# LANGUAGE TemplateHaskell #-}

module GUI.GUI
  ( GUI (..)
  , introGUI
  , mainGUI
  , optsGUI  
  , fromGUI
  , infoGUI
  , cursor
  , fromFormat
  ) where

import Control.Lens

import Graphics.RedViz.Widget

data GUI
  =  Empty
  |  IntroGUI
     {
       _res      :: (Int, Int)
     , _cursor   :: Widget
     --   _fps      :: Widget
     -- , _info     :: Widget
     -- , _startB   :: Widget
     -- , _optionsB :: Widget
     , _quitB    :: Widget -- button
     -- , _cursor   :: Widget
     }
  |  OptionsGUI
     {
       _res      :: (Int, Int)
     , _cursor   :: Widget
     , _backB    :: Widget -- button
     }
  -- |  OptionsGUI
  --    {
  --      _res      :: (Int, Int)
  --    , _fps      :: Widget
  --    , _soundT   :: Widget
  --    , _musicT   :: Widget
  --    , _resMT    :: Widget
  --    }
  |  MainGUI
     {
       _res    :: (Int, Int)
     , _fps    :: Widget
     , _info   :: Widget
     , _cursor :: Widget
     }
  |  PlanetInfo
     {
       _res    :: (Int, Int)
     , _fps    :: Widget
     , _infos  :: [Widget]
     , _cursor :: Widget
     }
  deriving Show
$(makeLenses ''GUI)

fromFormat :: Format -> (Double, Double)
fromFormat fmt@(Format alignment_ _ _ _ _ _) =
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
  IntroGUI
  {
    _res    = res
  , _cursor = Cursor True "" (0.0, 0.0)
  --   _fps    = FPS True (Format TC 0.0 (0.0) 0.025 0.25)
  -- , _info   = TextField True ["sukanah"] (Format CC 0.0 (0.0) 0.025 0.25)
  --, _quitB  = Button True "exit" (BBox (0.5) (-0.5) (-0.5) (0.5)) False (Format CC (-0.25) (0.0) 0.085 1.0)
  -- , _startB   = Button True "start"   (BBox (-100) (50) (100) (-50)) False (Format CC (-0.25) (0.0) 0.085 1.0)
  -- , _optionsB = Button True "options" (BBox (-100) (50) (100) (-50)) False (Format CC (-0.35) (0.0) 0.085 1.0)
  , _quitB    = Button True "quit" (BBox (-0.2) (0.1) (0.2) (-0.1)) False False (Format CC (0.0) (0.0) 0.0 0.085 1.0)
  }

optsGUI :: (Int, Int) -> GUI
optsGUI res =
  OptionsGUI
  {
    _res    = res
  , _cursor = Cursor True "" (0.0, 0.0)
  , _backB  = Button True "< Back" (BBox (-0.2) (0.1) (0.2) (-0.1)) False False (Format CC (0.0) (0.0) 0.0 0.085 1.0)
  }

mainGUI :: (Int, Int) -> GUI
mainGUI res =
  MainGUI
  {
    _res  = res
  , _fps  = FPS True (Format TC (0.0) (0.0) (0.0) 0.085 1.0)
  , _info = TextField True ["you approach ebanat"] (Format BC 0.0 0.0 0.0 0.085 1.0)
  , _cursor = Cursor True "" (0.0, 0.0)
  }

infoGUI :: (Int, Int) -> GUI
infoGUI res =
  PlanetInfo
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
    --IntroGUI fps info startB optionsB quitB cursor ->
    IntroGUI res cursor quitB ->
      [
        cursor
      --   fps
      -- , info
      -- , startB
      -- , optionsB
      , quitB
      -- , cursor
      ]
    MainGUI res fps info cursor ->
      [
        fps
      , info
      , cursor
      ]
    PlanetInfo res fps infos cursor ->
      [
        fps
      , cursor      
      ] ++ infos
    _ -> []
