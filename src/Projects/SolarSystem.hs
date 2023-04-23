module Projects.SolarSystem where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)
import Graphics.RedViz.Backend

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
  [ (Model   "models/sun.bgeo")        -- 0
  , (Model   "models/mercury.bgeo")    -- 1 
  , (Model   "models/venus.bgeo")      -- 2
  , (Model   "models/earth.bgeo")      -- 3
  , (Model   "models/moon.bgeo")       -- 4
  , (Model   "models/mars.bgeo")       -- 5
  , (Model   "models/phobos.bgeo")     -- 6
  , (Model   "models/deimos.bgeo")     -- 7
  , (Model   "models/jupiter.bgeo")    -- 8
  , (Model   "models/europa.bgeo")     -- 9
  , (Model   "models/ganymede.bgeo")   -- 10
  , (Model   "models/io.bgeo")         -- 11
  , (Model   "models/callisto.bgeo")   -- 12
  , (Model   "models/TNS.bgeo")        -- 13
  ]
  [
    PreObject "sun"     "rbd"
    0 nil [0] [] [] [] [] defaultBackendOptions
  , PreObject "mercury" "rbd"
    1 nil [1] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-4.417,0], []] defaultBackendOptions
  , PreObject "venus"   "rbd"
    2 nil [2] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-1.426,0], []] defaultBackendOptions
  , PreObject "earth" "rbd"
    3 nil [3] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-1.0, 0.5 ,0], []] defaultBackendOptions
  , PreObject "moon" "rbd"
    4 nil [4] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-13.036,3], []] defaultBackendOptions
  , PreObject "mars" "rbd"
    5 nil [5] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-0.531,0], []] defaultBackendOptions
  , PreObject "phobos" "rbd"
    6 nil [6] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-1190.22,5], []] defaultBackendOptions
  , PreObject "deimos" "rbd"
    7 nil [7] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-292,5], []] defaultBackendOptions
  , PreObject "jupiter" "rbd"
    8 nil [8] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-0.0842,0], []] defaultBackendOptions
  , PreObject "europa"  "rbd"
    9 nil [9] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-104.2,8], []] defaultBackendOptions
  , PreObject "ganymede"  "rbd"
    10 nil [10] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-51.02,8], []] defaultBackendOptions
  , PreObject "io"  "rbd"
    11 nil [11] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-206.33,8], []] defaultBackendOptions
  , PreObject "callisto"  "rbd"
    12 nil [12] [] [] ["orbit", "trace"]
    [[0,0,0,0,0,1,-21.871,8], []] defaultBackendOptions
  , PreObject "NPK" "sprite"
    13 nil [13] [] [] [] [] defaultBackendOptions
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
