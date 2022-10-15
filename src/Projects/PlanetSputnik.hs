module Projects.PlanetSputnik where

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
  "Planet with Sputnik"
  1280
  800
  -- 640
  -- 400
  "AbsoluteLocation"
  [ (Model   "models/planet_0.bgeo")
  , (Model   "models/sputnik.bgeo")
  ]
  [
    PreObject
    "planet"
    "rbd"
    nil
    [0] [] []
    ["gravity"]
    [[]]
    -- ["rotate", "translate"]
    -- [[0,0,0,0,0,1000]
    -- ,[1000,0,0]]
    -- ["gravity"]
    -- [[1,2]]
  , PreObject
    "sputnik"
    "rbd"
    nil
    [1] [] []
    ["gravity", "spin"]
    [[],[]]
    -- [
    --   "translateconst"
    -- , "translate"      
    -- , "rotate"
    -- , "spin"
    -- ]
    -- [
    --   [0,3,0]
    -- , [0,-0.01,0]     
    -- , [0,0,0,0,0,-0.01]
    -- , []
    -- ]
    -- ["rotate","gravity"]
    -- [[0,0,0,0,0,-0.01],[]]
    -- ["rotate'"]
    -- [[-5,0,0,0,0,-0.01]]
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
