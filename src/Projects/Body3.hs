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
  [ (Model   "models/body_0.bgeo")
  , (Model   "models/body_1.bgeo")
  , (Model   "models/body_2.bgeo")
  ]
  [ (PreObject
    "red"
    "rbd"
    nil
    [0]
    []
    []
    --["rotate", "translate"]
    -- [[0,0,0,0,0,1000]
    -- ,[1000,0,0]]
    ["gravity"]
    [[1,2]]
    )
  , (PreObject
    "blue"
    "rbd"
    nil
    [1]
    []
    []
    --["rotate", "translate"]
    -- [[0,0,0,0,0,1000]
    -- ,[1000,0,0]]
    ["gravity"]
    [[0,2]]
    )
  , (PreObject
    "green"
    "rbd"
    nil
    [2]
    []
    []
    --["rotate", "translate"]
    -- [[0,0,0,0,0,1000]
    -- ,[1000,0,0]]
    ["gravity"]
    [[0,1]]
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
  []--defaultFonts
