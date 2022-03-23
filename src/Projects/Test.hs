module Projects.Test where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)

-- defaultFormat :: Format'
-- defaultFormat =  Format' "CC" (-0.4) 0.0 0.085 1.0

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
    "Box"
    ""
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
     0, 0, 1,-10,
     0, 0, 0, 1])
    1.0
    1.0
    1.0
  ]
