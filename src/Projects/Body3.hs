module Projects.Body3 where

import Data.UUID

import Graphics.RedViz.Project.Project hiding (gui)
import Graphics.RedViz.Backend  

gui :: PreGUI
gui =
  PreGUI
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
  [ ( PreObject
      { _pname          = "red"
      , _ptype          = "rbd"
      , _pidx           = 0
      , _uuid           = nil
      , _modelIDXs      = [0]
      , _presolvers     = []
      , _presolverAttrs = []
      , _solvers        = ["gravity"]
      , _solverAttrs    = [[1,2]]
      , _options        = defaultBackendOptions 
      }
    )
  , ( PreObject
      { _pname          = "blue"
        , _ptype          = "rbd"
        , _pidx           = 0
        , _uuid           = nil
        , _modelIDXs      = [1]
        , _presolvers     = []
        , _presolverAttrs = []
        , _solvers        = ["gravity"]
        , _solverAttrs    = [[0,2]]
        , _options        = defaultBackendOptions 
        }
      )
  , ( PreObject
      { _pname          =  "green"
      , _ptype          = "rbd"
      , _pidx           = 0
      , _uuid           = nil
      , _modelIDXs      = [2]
      , _presolvers     = []
      , _presolverAttrs = []
      , _solvers        = ["gravity"]
      , _solverAttrs    =  [[0,1]]
      , _options        = defaultBackendOptions
      }
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

emptyGUI :: PreGUI
emptyGUI =
  PreGUI
  []
  []--defaultFonts
