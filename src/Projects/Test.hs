module Projects.Test where

import Data.UUID

import Graphics.RedViz.Project.Project

defaultPreGUI :: PreGUI
defaultPreGUI =
  PreGUI
  defaultFonts
  defaultIcons

project :: Int -> Int -> Project
project resx resy =
  Project
  "Test Project"
  resx
  resy
  "AbsoluteLocation"
  --[ (Model   "models/graph.bgeo")]
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "box"
    "planet"
    0
    nil
    [0]
    []
    []
    ["rotate", "translate"]
    [[0,0,0,0,0,0.01,0,0,0]
    ,[0.01,0,0]]
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

test2 :: Project
test2 =
  Project
  "Test Project"
  1280
  800
  -- 640
  -- 400
  "AbsoluteLocation"
  [
    (Model   "models/earth.bgeo")
  ]
  [ (PreObject
    "box"
    "planet"
        0
    nil
    [0]
    []
    []
    --["rotate", "translate"]
    []
    [[0,0,0,0,0,0.01]
    ,[0.01,0,0]]
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

options :: Int -> Int -> Project
options resx resy =
  Project
  "Options Menu"
  -- 1280
  -- 800
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "box"
    "planet"
        0
    nil
    [0]
    []
    []
    ["rotate", "translate"]
    [[0,0,0,0,0,0.01]
    ,[0.01,0,0]]
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

guiTestRed :: PreGUI
guiTestRed =
  PreGUI
  -- [ TextField' True ["Test Red"] (Format' "CC" (-0.4) 0.0 0.085 1.0)
  -- , FPS' True (Format' "TC" (-0.4) 0.0 0.085 1.0) ]
  defaultFonts
  []

emptyGUI :: PreGUI
emptyGUI =
  PreGUI
  []
  []--defaultFonts
  

projectTestRed :: Int -> Int -> Project
projectTestRed resx resy =
  Project
  "test red"
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box_red.bgeo")]
  [ (PreObject
    "red box"
    "planet"
        0
    nil
    [0]
    []
    []
    []
    []
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

projectTestGreen :: Int -> Int -> Project
projectTestGreen resx resy =
  Project
  "test green"
  resx --1280
  resy --720
  "AbsoluteLocation"
  [ (Model   "models/box_green.bgeo")]
  [ (PreObject
    "green box"
    "planet"
        0
    nil
    [0]
    []
    []
    []
    []
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

projectTestBlue :: Int -> Int -> Project
projectTestBlue resx resy =
  Project
  "test blue"
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box_blue.bgeo")]
  [ (PreObject
    "blue box"
    "planet"
        0
    nil
    [0]
    []
    []
    []
    []
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

projectTestChecker :: Project
projectTestChecker =
  Project
  "test checkerboard"
  1280
  720
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "Checker box"
    "planet"
        0
    nil
    [0]
    [
      "prerotate"
    ]
    [
      [0,0,0,0,0,100]
    ]
    [
      "rotate"
    ]
    [
      [0,0,0,10000,0,0]
    ]
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]

projectTestCheckerOffset :: Project
projectTestCheckerOffset =
  Project
  "test checkerboard"
  800
  600
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "Checker box"
    "planet"
        0
    nil
    [0]
    [    ]
    [    ]
    [    ]
    [    ]
    )
  ]
  []
  defaultPreGUI
  [(ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1])
    1.0
    5.0
    0.000001
  ]
