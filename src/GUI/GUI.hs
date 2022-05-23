{-# LANGUAGE TemplateHaskell #-}

module GUI.GUI
  ( GUI (..)
  , introGUI
  , mainGUI
  , fromGUI
  , infoGUI
  , cursor
  ) where

import Control.Lens

import Graphics.RedViz.Widget

data GUI
  =  Empty
  |  IntroGUI
     {
       _fps    :: Widget
     , _info   :: Widget
     , _exitB  :: Widget -- button
     , _cursor :: Widget
     } 
  |  MainGUI
     {
       _fps    :: Widget
     , _info   :: Widget
     , _cursor :: Widget
     }
  |  PlanetInfo
     {
       _fps    :: Widget
     , _infos  :: [Widget]
     , _cursor :: Widget
     }
  deriving Show
$(makeLenses ''GUI)

introGUI :: GUI
introGUI =
  IntroGUI
  {
    _fps    = FPS True (Format TC 0.0 (0.0) 0.085 1.0)
  , _info   = TextField True ["sukanah"] (Format CC 0.0 (0.0) 0.185 2.0)
  --, _exitB  = Button True "exit" (BBox (0.5) (-0.5) (-0.5) (0.5)) False (Format CC (-0.25) (0.0) 0.085 1.0)
  , _exitB  = Button True "exit" (BBox (250) (150) (390) (250)) False (Format CC (-0.25) (0.0) 0.085 1.0)
  , _cursor = Cursor True "" (0.0, 0.0)
  }

mainGUI :: GUI
mainGUI =
  MainGUI
  {
    _fps  = FPS True (Format TC 0.0 (0.0) 0.085 1.0)
  , _info = TextField True ["you approach ebanat"] (Format BC 0.0 0.0 0.085 1.0)
  , _cursor = Cursor True "" (0.0, 0.0)
  }

infoGUI :: GUI
infoGUI =
  PlanetInfo
  {
    _fps   = FPS True (Format TC 0.0 (0.0) 0.085 1.0)
  , _infos =
    [ TextField True ["planet ebanat"] (Format BC 0.0 (0.0) 0.085 1.0)
    , TextField True ["population: 11,000,000,000 ebanats"] (Format TC (-0.15) (0.0) 0.085 1.0)
    ]
  , _cursor = Cursor True "" (0.0, 0.0)
  }
  
fromGUI :: GUI -> [Widget]
fromGUI gui =
  case gui of
    IntroGUI fps info exitB cursor ->
      [ fps
      , info
      , exitB
      , cursor
      ]
    MainGUI fps info cursor ->
      [ fps
      , info
      , cursor
      ]
    PlanetInfo fps infos cursor ->
      [ fps
      , cursor      
      ] ++ infos
    _ -> []
