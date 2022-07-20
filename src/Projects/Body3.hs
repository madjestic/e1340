module Projects.Body3 where

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
  "3 Body Problem"
  -- 1280
  -- 800
  640
  400
  "AbsoluteLocation"
  [ (Model   "models/body_0.bgeo")]
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

emptyGUI :: GUI'
emptyGUI =
  GUI'
  []
  []--defaultFonts
