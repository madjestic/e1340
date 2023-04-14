module Projects.SolarSystem where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)
import Projects.InfoEarth (jupiter)

gui :: PreGUI
gui =
  PreGUI
  defaultFonts
  defaultIcons

project :: Int -> Int -> Project
project resx resy =
  Project
  "Solar System Extended"
  resx --1280 --2265 --1280
  resy --720  --1416 --800
  "AbsoluteLocation"
  [ (Model   "models/sun.bgeo")
  , (Model   "models/mercury.bgeo")
  , (Model   "models/venus.bgeo")
  , (Model   "models/earth.bgeo")
  , (Model   "models/moon.bgeo")
  , (Model   "models/mars.bgeo")
  , (Model   "models/phobos.bgeo")
  , (Model   "models/deimos.bgeo")
  , (Model   "models/jupiter.bgeo")
  , (Model   "models/europa.bgeo")
  , (Model   "models/ganymede.bgeo")
  , (Model   "models/io.bgeo")
  , (Model   "models/callisto.bgeo")
  ]
  [
    PreObject "sun"     "rbd"
    0 nil [0] [] [] [] []
  , PreObject "mercury" "rbd"
    1 nil [1] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-4.417,0], []]
  , PreObject "venus"   "rbd"
    2 nil [2] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-1.426,0], []]
  , PreObject "earth" "rbd"
    3 nil [3] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-1.0, 0.5 ,0], []]
  , PreObject "moon" "rbd"
    4 nil [4] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-13.036,3], []]
  , PreObject "mars" "rbd"
    5 nil [5] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-0.531,0], []]
  , PreObject "phobos" "rbd"
    6 nil [6] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-1190.22,5], []]
  , PreObject "deimos" "rbd"
    7 nil [7] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-292,5], []]
  , PreObject "jupiter" "rbd"
    8 nil [8] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-0.0842,0], []]
  , PreObject "europa"  "rbd"
    9 nil [9] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-104.2,8], []]
  , PreObject "ganymede"  "rbd"
    10 nil [10] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-51.02,8], []]
  , PreObject "io"  "rbd"
    11 nil [11] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-206.33,8], []]
  , PreObject "callisto"  "rbd"
    12 nil [12] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-21.871,8], []]
  ]
  []
  gui
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-30000000,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

emptyGUI :: PreGUI
emptyGUI =
  PreGUI
  []
  []
