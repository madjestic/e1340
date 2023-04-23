module Projects.Graph where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)
import Graphics.RedViz.Backend

gui :: PreGUI
gui =
  PreGUI
  defaultFonts
  defaultIcons

graph   :: [Int] -> PreObject
graph   idxs =
  PreObject "graph"  "sprite" 0 nil idxs [] [] [] [] defaultBackendOptions

gizmo   :: [Int] -> PreObject
gizmo   idxs =
  PreObject "gizmo" "rbd" 0 nil idxs [] [] [] [] defaultBackendOptions

graphCam :: ProjectCamera
graphCam =
  ProjectCamera
    "PlayerCamera"
    50.0
    100.0
    [1, 0, 0, 0,
     0, 1, 0, 0,
     0, 0, 1,-11,
     0, 0, 0, 1]
    1.0
    5.0
    1.0

project :: Project
project =
  Project
  "Grapher"
  -- 1280
  -- 800
  256
  256
  -- "AbsoluteLocation"
  "RelativeLocation"
  [
    Model "models/graph.bgeo"
  , Model "models/gizmo.bgeo"
  ]
  [
    graph   [0]
  , gizmo   [1]
  ]
  []
  gui
  [ graphCam ]
