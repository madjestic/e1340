module Projects.SolarSystemMini where

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
  "Solar System Mini"
  1280
  800
  "AbsoluteLocation"
  [ (Model   "models/star_0.bgeo")
  , (Model   "models/planet_0.bgeo")
  , (Model   "models/moon_0_0.bgeo")
  , (Model   "models/moon_0_0_0.bgeo")
  , (Model   "models/moon_0_0_0_0.bgeo")
  , (Model   "models/planet_1.bgeo")
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
     ,0,0,1,0.001
     ,0], []]
  , PreObject
    "moon_0_0"
    "rbd"
    2
    nil
    [2] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.01
     ,1], []]
  , PreObject
    "moon_0_0_0"
    "rbd"
    3
    nil
    [3] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.1
     ,2], []]
  , PreObject
    "moon_0_0_0_0"
    "rbd"
    4
    nil
    [4] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.75
     ,3], []]
  , PreObject
    "moon_0_0_0_0"
    "rbd"
    5
    nil
    [5] [] []
    ["orbit", "trace"]
    [[0,0,0
     ,0,0,1,0.01
     ,0], []]
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