module Projects.Test where

import Data.UUID

import Graphics.RedViz.Project.Project
import Graphics.RedViz.Backend

defaultPreGUI :: PreGUI
defaultPreGUI =
  PreGUI
  {
    _fonts = defaultFonts :: [Model]
  , _icons = defaultIcons :: [Model]
  }

project :: Int -> Int -> Project
project resx resy =
  Project
  "Test Project"
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ PreObject
    {
      _pname          = "box"
    , _ptype          = "planet"
    , _pidx           = 0
    , _uuid           = nil
    , _modelIDXs      = [0]
    , _presolvers     = []
    , _presolverAttrs = []
    , _solvers        = ["rotate", "translate"]
    , _solverAttrs    = [[0,0,0,0,0,0.01,0,0,0]
                        ,[0.01,0,0]]
    , _options        = defaultBackendOptions
    }
    
    ]
  []
  defaultPreGUI
  [ defaultPCam ]

options :: Int -> Int -> Project
options resx resy =
  Project
  "Options Menu"
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box.bgeo")]
  [ PreObject
    {
      _pname          = "box"
    , _ptype          = "planet"
    , _pidx           = 0
    , _uuid           = nil
    , _modelIDXs      = [0]
    , _presolvers     = []
    , _presolverAttrs = []
    , _solvers        = ["rotate", "translate"]
    , _solverAttrs    = [[0,0,0,0,0,0.01]
                        ,[0.01,0,0]]
    , _options        = defaultBackendOptions
    }
  ]
  []
  defaultPreGUI
  [ defaultPCam ]

projectTestRed :: Int -> Int -> Project
projectTestRed resx resy =
  Project
  "test red"
  resx
  resy
  "AbsoluteLocation"
  [ (Model "models/box_red.bgeo")
  , (Model "models/PNK_roll.bgeo")
  ]
  [ PreObject
    {
      _pname          = "red box"
    , _ptype          = "planet"
    , _pidx           = 0
    , _uuid           = nil
    , _modelIDXs      = [0]
    , _presolvers     = []
    , _presolverAttrs = []
    , _solvers        = []
    , _solverAttrs    = []
    , _options        = defaultBackendOptions
    }
  , PreObject
    {
      _pname          = "PNK_roll"
    , _ptype          = "sprite"
    , _pidx           = 1
    , _uuid           = nil
    , _modelIDXs      = [1]
    , _presolvers     = []
    , _presolverAttrs = []
    , _solvers        = []
    , _solverAttrs    = []
    , _options        = defaultBackendOptions'
    }
  ]
  []
  defaultPreGUI
  [ defaultPCam ]

projectTestGreen :: Int -> Int -> Project
projectTestGreen resx resy =
  Project
  "test green"
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box_green.bgeo")]
  [ PreObject
    {
      _pname          = "green box"
    , _ptype          = "planet"
    , _pidx           = 0
    , _uuid           = nil
    , _modelIDXs      = [0]
    , _presolvers     = []
    , _presolverAttrs = []
    , _solvers        = []
    , _solverAttrs    = []
    , _options        = defaultBackendOptions
    }
  ]
  []
  defaultPreGUI
  [ defaultPCam ]

projectTestBlue :: Int -> Int -> Project
projectTestBlue resx resy =
  Project
  "test blue"
  resx
  resy
  "AbsoluteLocation"
  [ (Model   "models/box_blue.bgeo")]
  [ PreObject
    { _pname          = "blue box"
    , _ptype          = "planet"
    , _pidx           = 0
    , _uuid           = nil
    , _modelIDXs      = [0]
    , _presolvers     = []
    , _presolverAttrs = []
    , _solvers        = []
    , _solverAttrs    = []
    , _options        =  defaultBackendOptions
    }
  ]
  []
  defaultPreGUI
  [ defaultPCam ]
