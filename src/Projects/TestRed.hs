module Projects.TestRed where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)

gui :: GUI'
gui =
  GUI'
  [ TextField' True ["Test Red"] (Format' "CC" (-0.4) 0.0 0.085 1.0)
  , FPS' True (Format' "TC" (-0.4) 0.0 0.085 1.0) ]
  defaultFonts

project :: Project
project =
  Project
  "Test Red"
  800
  600
  "AbsoluteLocation"
  [ (Model   "models/box_red.bgeo")]
  [ (PreObject
    "box"
    "planet"
    nil
    [0]
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
