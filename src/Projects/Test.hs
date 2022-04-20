module Projects.Test where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)

gui :: GUI'
gui =
  GUI'
  [ TextField' True ["Hello, World!"] (Format' "CC" (-0.4) 0.0 0.085 1.0)
  , FPS' True (Format' "TC" (-0.4) 0.0 0.085 1.0) ]
  defaultFonts

project :: Project
project =
  Project
  "Test Project"
  800
  600
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "box"
    "planet"
    nil
    [0]
    []
    []
    ["rotate", "translate"]
    [[0,0,0,0,0,1000]
    ,[1000,0,0]]
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
  [ TextField' True ["Test Red"] (Format' "CC" (-0.4) 0.0 0.085 1.0)
  , FPS' True (Format' "TC" (-0.4) 0.0 0.085 1.0) ]
  defaultFonts

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
  400
  300
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ (PreObject
    "Checker box"
    "planet"
    nil
    [0]
    [
      "prerotate"
    , "pretranslate'"
    ]
    [
      [0,0,0,0,0,100]
    , [1.33,0,0]
    ]
    [
      "translate"
    ]
    [
      [1000,0,0]
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
 
