module Projects.SolarSystemExtended where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)

gui :: GUI'
gui =
  GUI'
  defaultFonts
  defaultIcons

project :: Project
project =
  Project
  "Solar System Extended"
  1280
  800
  "AbsoluteLocation"
  [ (Model   "models/star_0.bgeo")
  , (Model   "models/planet_0.bgeo")
  , (Model   "models/moon_0_0.bgeo")
  , (Model   "models/planet_1.bgeo")
  , (Model   "models/moon_1_0.bgeo")
  , (Model   "models/planet_2.bgeo")
  , (Model   "models/moon_2_0.bgeo")
  , (Model   "models/moon_2_1.bgeo")  
  ]
  [
    PreObject
    "star_0"
    "rbd"
    0
    nil
    [0] [] []
    []
    []
  , PreObject
    "planet_0"
    "rbd"
    1
    nil
    [1] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.005
     ,0], []]
  , PreObject
    "moon_0_0"
    "rbd"
    2
    nil
    [2] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.05
     ,1], []]
  , PreObject
    "planet_1"
    "rbd"
    3
    nil
    [3] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.0015
     ,0], []]
  , PreObject
    "moon_1_0"
    "rbd"
    4
    nil
    [4] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.025
     ,3], []]
  , PreObject
    "planet_2"
    "rbd"
    5
    nil
    [5] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.0005
     ,0], []]
  , PreObject
    "moon_2_0"
    "rbd"
    6
    nil
    [6] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.015
     ,5], []]
  , PreObject
    "moon_2_1"
    "rbd"
    7
    nil
    [7] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.015
     ,6], []]
    
  ]
  []
  gui
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-30,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

emptyGUI :: GUI'
emptyGUI =
  GUI'
  []
  []
