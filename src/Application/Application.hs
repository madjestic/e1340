{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Application.Application
  ( Application (..)
  , fromApplication
  , pfile
  , pname
  , Application.Application.resx
  , Application.Application.resy
  , intr
  , opts
  , main
  , info
  --, counter
  , Application.Application.gui
  , quit
  , PreApplication (..)
  , defaultPreApplication
  , Application.Application.read
  , Application.Application.write
  , fromPreApplication
  ) where

import Control.Lens                     (view, makeLenses)
import Data.UUID
import Control.Concurrent.MVar
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Text                        (Text, pack)
import Data.Maybe                       (fromMaybe)
import Control.Lens                     (toListOf, view, (^..), (^.), bimap)
import GHC.Generics

import Graphics.RedViz.Project as P ( camMode, resy, resx, name, read )
import Graphics.Rendering.OpenGL as GL    (GLuint)

import App (App(..), gui, fromProject)
import GUI

--import Debug.Trace as DT

-- data PreApp
--   =  PreApp
--   { _projectPath :: FilePath
--   , _guiPath     :: FilePath
--   } deriving (Generic, Show)
-- instance ToJSON PreApp

data PreApplication
  =  PreApplication
  {
    _pfile    :: FilePath
  , _pname    :: String
  , _resx     :: Int
  , _resy     :: Int
  , _resp     :: Int
  , _trace    :: Bool
  , _pintr    :: FilePath
  , _pmain    :: FilePath
  , _popts    :: FilePath
  , _pinfo    :: FilePath
  , _pintrGUI :: FilePath
  , _pmainGUI :: FilePath 
  , _poptsGUI :: FilePath 
  , _pinfoGUI :: FilePath
  } deriving Show
$(makeLenses ''PreApplication)
deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''PreApplication

write :: PreApplication -> IO ()
write preApp = 
  B.writeFile fileOut $ encodePretty' config preApp
  where
    fileOut = preApp ^. pfile
    config = defConfig { confCompare = comp }

comp :: Text -> Text -> Ordering
comp = keyOrder . fmap pack $
  [ "pfile"
  , "pname"
  , "resx"
  , "resy"
  , "resp"
  , "trace"
  , "pintr"
  , "pmain"
  , "popts"
  , "pinfo"
  , "pintrGUI"
  , "pmainGUI"
  , "poptsGUI"
  , "pinfoGUI"
  ]

read :: FilePath -> IO PreApplication
read filePath = do
  Prelude.putStrLn $ "filePath -> PreApplication : " ++ show filePath
  d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String PreApplication)
  let
    file'  = (_pfile . fromEitherDecode) d
    name'  = (_pname . fromEitherDecode) d
    resx'  = (_resx  . fromEitherDecode) d
    resy'  = (_resy  . fromEitherDecode) d
    resp'  = (_resp  . fromEitherDecode) d
    trace' = (_trace . fromEitherDecode) d
    intr'  = (_pintr . fromEitherDecode) d
    main'  = (_pmain . fromEitherDecode) d
    opts'  = (_popts . fromEitherDecode) d
    info'  = (_pinfo . fromEitherDecode) d
    pintrGUI' = (_pintrGUI . fromEitherDecode) d 
    pmainGUI' = (_pmainGUI . fromEitherDecode) d 
    poptsGUI' = (_poptsGUI . fromEitherDecode) d 
    pinfoGUI' = (_pinfoGUI . fromEitherDecode) d 
  return $  
    PreApplication
    {
      _pfile = file'
    , _pname = name'
    , _resx  = resx'
    , _resy  = resy'
    , _resp  = resp'
    , _trace = trace'
    , _pintr = intr' 
    , _pmain = main'
    , _popts = opts'
    , _pinfo = info'
    , _pintrGUI = pintrGUI' 
    , _pmainGUI = pmainGUI'  
    , _poptsGUI = poptsGUI'  
    , _pinfoGUI = pinfoGUI'  
    }
    where
      fromEitherDecode = fromMaybe defaultPreApplication . fromEither
      fromEither d =
        case d of
          Right pt -> Just pt            
          _ -> Nothing

fromPreApplication :: PreApplication -> IO Application
fromPreApplication p = do
  intrProj <- P.read (p ^. pintr)
  mainProj <- P.read (p ^. pmain)
  optsProj <- P.read (p ^. popts)
  infoProj <- P.read (p ^. pinfo)

  intrGUI  <- GUI.read (p ^. pintrGUI) IntrGUI'
  mainGUI  <- GUI.read (p ^. pmainGUI) MainGUI'
  optsGUI  <- GUI.read (p ^. poptsGUI) OptsGUI'
  infoGUI  <- GUI.read (p ^. pinfoGUI) InfoGUI'
  
  intrApp' <- fromProject intrProj intrGUI
  mainApp' <- fromProject mainProj mainGUI
  optsApp' <- fromProject optsProj optsGUI
  infoApp' <- fromProject infoProj infoGUI
  
  let appl =
        Application
        {
          Application.Application._gui = intrApp' ^. App.gui
        , _quit    = False
        , _intr    = intrApp'
        , _main    = mainApp' 
        , _opts    = optsApp' 
        , _info    = infoApp' 
        , _hmap    = []
        }
  return appl

defaultPreApplication :: PreApplication
defaultPreApplication =
  PreApplication
  {
    _pfile    = "./applications/solarsystem"
  , _pname    = "Hello, World!"
  , _resx     = 1280
  , _resy     = 720
  , _resp     = 0                            -- resolultion list position (for multi-res selctors)
  , _trace    = True
  , _pintr    = "./projects/testred"
  , _pmain    = "./projects/test"
  , _popts    = "./projects/testgreen"
  , _pinfo    = "./projects/testblue"
  , _pintrGUI = "./gui/pintrgui"
  , _pmainGUI = "./gui/pmaingui"
  , _poptsGUI = "./gui/poptsgui"
  , _pinfoGUI = "./gui/pinfogui"
  }

data Application
  = Application
  {
    _gui     :: GUI
  , _quit    :: Bool
  , _intr    :: App
  , _main    :: App
  , _opts    :: App
  , _info    :: App
  , _hmap    :: [(UUID, GLuint)] -- a placeholder for the future hmap, for now it's a map from a long texture unit index to a short version.
  --, _counter :: MVar Int
  } 
$(makeLenses ''Application)

instance Show (MVar a) where
  show = show

fromApplication :: Application -> App
fromApplication appl =
  --case view Application.Application.gui.gui' appl of
  case appl ^. Application.Application.gui . guiSwitch of
    IntrGUI' -> view intr appl 
    MainGUI' -> view main appl 
    OptsGUI' -> view opts appl 
    InfoGUI' -> view info appl 
