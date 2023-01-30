module Projects.SolarSystem where

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
  [ (Model   "models/sun.bgeo")
  , (Model   "models/mercury.bgeo")
  , (Model   "models/venus.bgeo")
  , (Model   "models/earth.bgeo")
  , (Model   "models/moon.bgeo")
  , (Model   "models/mars.bgeo")
  , (Model   "models/phobos.bgeo")
  , (Model   "models/deimos.bgeo")
  ]
  [
    PreObject "sun"     "rbd"
    0 nil [0] [] [] [] []
  , PreObject "mercury" "rbd"
    1 nil [1] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.005,0], []]
  , PreObject "venus"   "rbd"
    2 nil [2] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.0025,0], []]
  , PreObject "earth" "rbd"
    3 nil [3] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.00125,0], []]
  , PreObject "moon" "rbd"
    4 nil [4] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.0625,3], []]
  , PreObject "mars" "rbd"
    5 nil [5] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.000625,0], []]
  , PreObject "phobos" "rbd"
    6 nil [6] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.0625,5], []]
  , PreObject "deimos" "rbd"
    7 nil [7] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,0.0325,5], []]
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
