{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}

module Application.Application
  ( Application (..)
  , fromApplication
  , intr
  , opts
  , main
  , info
  --, counter
  , Application.Application.gui
  , quit
  , PreApplication (..)
  , defaultPreApplication
  , write
  , Application.Application.read
  , pfile
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

import Graphics.RedViz.Project as P ( camMode, resy, resx, name, read )
import Graphics.Rendering.OpenGL as GL    (GLuint)

import App (App(..), gui, intrApp, mainApp, optsApp, infoApp)
import GUI

--import Debug.Trace as DT

data PreApplication
  =  PreApplication
  {
    _resx  :: Int
  , _resy  :: Int
  , _resp  :: Int
  , _trace :: Bool
  , _pintr :: FilePath 
  , _pmain :: FilePath 
  , _popts :: FilePath 
  , _pinfo :: FilePath
  , _pfile :: FilePath
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
  [ "resx"
  , "resy"
  , "resp"
  , "trace"
  , "pintr"
  , "pmain"
  , "popts"
  , "pinfo"]

read :: FilePath -> IO PreApplication
read filePath = do
  Prelude.putStrLn $ "filePath : " ++ show filePath
  d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String PreApplication)
  let
    resx'  = (_resx  . fromEitherDecode) d
    resy'  = (_resy  . fromEitherDecode) d
    resp'  = (_resp  . fromEitherDecode) d
    trace' = (_trace . fromEitherDecode) d
    intr'  = (_pintr . fromEitherDecode) d
    main'  = (_pmain . fromEitherDecode) d
    opts'  = (_popts . fromEitherDecode) d
    info'  = (_pinfo . fromEitherDecode) d
    file'  = (_pfile . fromEitherDecode) d
  return $  
    PreApplication
    {
      _resx  = resx'
    , _resy  = resy'
    , _resp  = resp'
    , _trace = trace'
    , _pintr = intr' 
    , _pmain = main'
    , _popts = opts'
    , _pinfo = info'
    , _pfile = file'
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
  
  intrApp' <- intrApp intrProj
  mainApp' <- mainApp mainProj
  optsApp' <- optsApp optsProj
  infoApp' <- infoApp infoProj
  
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
    _resx  = 1280
  , _resy  = 720
  , _resp  = 0
  , _trace = True
  , _pintr = "./projects/solarsystem"
  , _pmain = "./projects/solarsystem"
  , _popts = "./projects/solarsystem"
  , _pinfo = "./projects/solarsystem"
  , _pfile = "./applications/solarsystem"
  }

defaultApplication :: Application
defaultApplication = undefined

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
  case view Application.Application.gui appl of
    IntrGUI {} -> view intr appl 
    MainGUI {} -> view main appl 
    InfoGUI {} -> view info appl 
    OptsGUI {} -> view opts appl 
