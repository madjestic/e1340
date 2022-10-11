module Projects.InfoEarth where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)

gui :: GUI'
gui =
  GUI'
  defaultFonts
  defaultIcons

sun     :: [Int] -> PreObject
sun     idxs =
  PreObject "sun"     "planet" nil idxs [] [] [] []
mercury :: [Int] -> PreObject
mercury idxs =
  PreObject "mercury" "planet" nil idxs [] [] [] []
venus   :: [Int] -> PreObject
venus   idxs =
  PreObject "venus"   "planet" nil idxs [] [] [] []
earth   :: [Int] -> PreObject
earth   idxs =
  PreObject "earth"   "planet" nil idxs
  ["prerotate"]
  [[0,0,0,0,0,0.5]]
  ["rotate'"]
  [[0,0,0,0,-0.005,0]]

moon   :: [Int] -> PreObject
moon   idxs =
  PreObject "moon"    "planet" nil idxs [] [] [] []
mars    :: [Int] -> PreObject
mars    idxs =
  PreObject "mars"    "planet" nil idxs [] [] [] []
jupiter :: [Int] -> PreObject
jupiter idxs =
  PreObject "jupiter" "planet" nil idxs [] [] [] []

gizmo   :: [Int] -> PreObject
gizmo   idxs =
  PreObject "gizmo" "planet" nil idxs [] [] [] []

stars :: [Int] -> PreObject
stars idxs =
  PreObject "stars" "planet" nil idxs [] [] [] []

earthCam :: ProjectCamera
earthCam =
  ProjectCamera
   "earth Camera"
   50.0
   100.0
   [1, 0, 0, -5000000,
    0, 1, 0, 0,
    0, 0, 1,-150040000000,
    0, 0, 0, 1]
   0.0
   0.0
   0.0  

project :: Project
project =
  Project
  "Info: Earth"
  800
  600
  "RelativeLocation"
  [
    Model "models/stars.bgeo"
  , Model "models/star_sector_01.bgeo"
  , Model "models/star_sector_02.bgeo"
  , Model "models/star_sector_03.bgeo"
  , Model "models/star_sector_04.bgeo"
  , Model "models/star_sector_05.bgeo"
  , Model "models/star_sector_06.bgeo"
  , Model "models/star_sector_07.bgeo"
  , Model "models/star_sector_08.bgeo"
  , Model "models/star_sector_09.bgeo"
  , Model "models/sun.bgeo"
  , Model "models/mercury.bgeo"
  , Model "models/venus.bgeo"
  , Model "models/earth.bgeo"
  , Model "models/moon.bgeo"
  , Model "models/mars.bgeo"
  , Model "models/jupiter.bgeo"
  , Model "models/gizmo.bgeo"
  ]
  [
    sun     [10]
  , mercury [11]
  , venus   [12]
  , earth   [13]
  , moon    [14]
  , mars    [15]
  , jupiter [16]
  , gizmo   [17]
  ]
  [
    stars [0..9]
  ]
  gui
  [
    earthCam
  ]
