module Projects.Curve where

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
  "Test Project"
  1280
  800
  -- 640
  -- 400
  "AbsoluteLocation"
  [ (Model   "models/square.bgeo")]
  [ (PreObject
    "curve"
    "sprite"
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
  gui
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
  gui
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

options :: Project
options =
  Project
  "Options Menu"
  -- 1280
  -- 800
  640
  400
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
  gui
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

guiTestRed :: GUI'
guiTestRed =
  GUI'
  -- [ TextField' True ["Test Red"] (Format' "CC" (-0.4) 0.0 0.085 1.0)
  -- , FPS' True (Format' "TC" (-0.4) 0.0 0.085 1.0) ]
  defaultFonts
  []

emptyGUI :: GUI'
emptyGUI =
  GUI'
  []
  []--defaultFonts
  

projectTestRed :: Project
projectTestRed =
  Project
  "test red"
  800
  600
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
  emptyGUI
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

projectTestGreen :: Project
projectTestGreen =
  Project
  "test green"
  800
  600
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
  emptyGUI
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

projectTestBlue :: Project
projectTestBlue =
  Project
  "test blue"
  800
  600
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
  emptyGUI
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
  gui
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
  gui
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
