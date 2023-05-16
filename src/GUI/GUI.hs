{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module GUI.GUI
  ( GUI (..)
  , GUI'(..)
  , intrGUI
  , mainGUI
  , optsGUI  
  , fromGUI
  , infoGUI
  , res
  , cursor
  , fromFormat
  , optsB
  , quitB
  , xx
  , backB
  , strtB
  , speed
  , GUI.GUI.read
  , GUI.GUI.write
  , guiSwitch
  , pnkGUI
  , defGUI
  , defaultGUI
  ) where

import Control.Lens
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Encode.Pretty
import Data.Maybe                       (fromMaybe, listToMaybe, maybeToList)
import GHC.Generics
import Data.ByteString.Lazy as B hiding (drop, pack)
import Data.Text                        (Text, pack)

import Graphics.RedViz.Widget
import Graphics.RedViz.Backend

data GUI' = IntrGUI' | MainGUI' | OptsGUI' | InfoGUI' deriving (Generic, Show)
instance ToJSON GUI'
instance FromJSON GUI'

data GUI
  =  GUI
     {
       _res             :: (Int, Int)
     , _cursor          :: Maybe Widget
     , _xx              :: Maybe Widget
     , _title           :: Maybe Widget
     , _strtB           :: Maybe Widget
     , _optsB           :: Maybe Widget
     , _quitB           :: Maybe Widget -- button
     , _gizmo           :: Maybe Widget
     , _backB           :: Maybe Widget -- button
     , _fps             :: Maybe Widget
     , _speed           :: Maybe Widget
     , _infos           :: Maybe [Widget]
     , _guiSwitch       :: GUI'
     }
  deriving (Generic, Show)
$(makeLenses ''GUI)
--deriveJSON defaultOptions {fieldLabelModifier = drop 1} ''GUI
instance ToJSON GUI
instance FromJSON GUI

defGUI :: GUI  
defGUI =
  GUI
  {
    _res             = (800,600)
  , _cursor          = Nothing 
  , _xx              = Nothing 
  , _title           = Nothing 
  , _strtB           = Nothing 
  , _optsB           = Nothing 
  , _quitB           = Nothing 
  , _gizmo           = Nothing 
  , _backB           = Nothing 
  , _fps             = Nothing 
  , _speed           = Nothing 
  , _infos           = Nothing
  , _guiSwitch       = MainGUI'
  }

comp :: Text -> Text -> Ordering
comp = keyOrder . fmap pack $
  [
    "res"
  , "cursor"
  , "xx"
  , "a_space_oddysey"
  , "strtB"
  , "optsB"
  , "quitB"
  , "fps"
  , "speed"
  , "gizmo"
  , "infos"
  , "pnk_rcf"
  , "guiSwitch"
  ]

read :: FilePath -> GUI' -> IO GUI
read filePath guiSwitch =
  case guiSwitch of
    IntrGUI' -> do
      Prelude.putStrLn $ "filePath : " ++ show filePath
      d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String GUI)
      let
        res'      = (_res    . fromEitherDecode) d
        cursor'   = (_cursor . fromEitherDecode) d
        xx'       = (_xx     . fromEitherDecode) d
        title'    = (_title . fromEitherDecode) d
        strtB'    = (_strtB  . fromEitherDecode) d 
        optsB'    = (_optsB  . fromEitherDecode) d 
        quitB'    = (_quitB  . fromEitherDecode) d
        guiSwitch = (_guiSwitch   . fromEitherDecode) d
      
      return $  
        defGUI
        {
          _res    = res'
        , _cursor = cursor'
        , _xx     = xx'
        , _title  = title'
        , _strtB  = strtB'
        , _optsB  = optsB'
        , _quitB  = quitB'
        , _gizmo  = Nothing
        , _guiSwitch   = guiSwitch
        }
        where
          fromEitherDecode = fromMaybe (intrGUI (1280,720)) . fromEither
          fromEither d =
            case d of
              Right pt -> Just pt            
              _ -> Nothing
              
    MainGUI' -> do
      Prelude.putStrLn $ "filePath : " ++ show filePath
      d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String GUI)
      let
        res'     = (_res     . fromEitherDecode) d
        cursor'  = (_cursor  . fromEitherDecode) d
        gizmo'   = (_gizmo   . fromEitherDecode) d
        speed'   = (_speed   . fromEitherDecode) d
        fps'     = (_fps     . fromEitherDecode) d
      return $  
        defGUI
        {
          _res    = res'
        , _cursor = cursor'
        , _gizmo  = gizmo'
        , _speed  = speed'
        , _fps    = fps'
        }
        where
          fromEitherDecode = fromMaybe (intrGUI (1280,720)) . fromEither
          fromEither d =
            case d of
              Right pt -> Just pt            
              _ -> Nothing

    OptsGUI' -> do
      Prelude.putStrLn $ "filePath : " ++ show filePath
      d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String GUI)
      let
        res'     = (_res    . fromEitherDecode) d
        cursor'  = (_cursor . fromEitherDecode) d
        backB'   = (_backB  . fromEitherDecode) d
      return $  
        defGUI
        {
          _res    = res'
        , _cursor = cursor'
        , _backB  = backB'
        , _gizmo  = Nothing        
        }
        where
          fromEitherDecode = fromMaybe (intrGUI (1280,720)) . fromEither
          fromEither d =
            case d of
              Right pt -> Just pt            
              _ -> Nothing
              
    InfoGUI' -> do
      Prelude.putStrLn $ "filePath : " ++ show filePath
      d <- (eitherDecode <$> B.readFile filePath) :: IO (Either String GUI)
      let
        res'    = (_res    . fromEitherDecode) d
        cursor' = (_cursor . fromEitherDecode) d
        fps'    = (_fps    . fromEitherDecode) d
        infos'  = (_infos  . fromEitherDecode) d
      return $  
        defGUI
        {
          _res    = res'
        , _cursor = cursor'
        , _fps    = fps'
        , _infos  = infos'
        , _gizmo  = Nothing        
        }
        where
          fromEitherDecode = fromMaybe (intrGUI (1280,720)) . fromEither
          fromEither d =
            case d of
              Right pt -> Just pt            
              _ -> Nothing
              
    -- _ -> error $ "GUI' value does not exist: " ++ show guiSwitch
    --           ++ "when reading file: " ++ show filePath

write :: FilePath -> GUI -> IO ()
write filePath gui = do
  B.writeFile filePath $ encodePretty' config gui
    where
      config = defConfig { confCompare = comp }                    

fromGUI :: GUI -> [Widget]
fromGUI gui =
      []
      ++ maybeToList ( _title gui) -- ^. _title
      ++ maybeToList ( _strtB gui) 
      ++ maybeToList ( _optsB gui) 
      ++ maybeToList ( _quitB gui) 
      ++ maybeToList (_xx     gui)
      ++ maybeToList (_cursor gui)
      ++ maybeToList (_cursor gui)
      ++ maybeToList (_backB  gui) 
      ++ maybeToList (_fps    gui)
      ++ maybeToList (_speed  gui )
      ++ maybeToList (_cursor gui)
      ++ maybeToList (_gizmo  gui)
      ++ maybeToList (_fps    gui  )
      ++ fromMaybe [] (_infos  gui)
      ++ maybeToList (_cursor gui)

fromFormat :: Format -> (Double, Double)
fromFormat (Format alignment_ resx_ resy_ x_ y_ _ _ _) =
  (\ (x0, y0) (x1,y1) -> (x0+x1, y0+y1)) (x_,y_) $
  case alignment_ of
    TL -> (-1.0, 0.5)
    TC -> ( 0.0, 0.5)
    TR -> ( 1.0, 0.5) -- TODO: adjust the Right alignment by the string length.
    CL -> (-1.0, 0.0)
    CC -> ( 0.0, 0.0)
    CR -> ( 1.0, 0.0)
    BL -> (-1.0,-0.5)
    BC -> ( 0.0,-0.5)
    BR -> ( 1.0, 0.5)

defBBox :: BBox
defBBox =
  BBox (-0.2) (0.1) (0.2) (-0.1)

fontSize :: Int -> Double
fontSize s =
  case s of
    5 -> 0.3
    4 -> 0.2
    3 -> 0.1
    2 -> 0.05
    1 -> 0.02
    0 -> 0.01
    _ -> 1.0

defaultGUI :: (Int, Int) -> GUI
defaultGUI res0@(resx, resy) =
  defGUI
  {
    _res    = res0
  , _cursor = Just $ Cursor True ""
    (defaultCursorFormat resx resy) defOpts
  }

pnkGUI :: (Int, Int) -> GUI
pnkGUI res0@(resx, resy) =
  defGUI
  {
    _res    = res0
  , _cursor = Just $ Cursor True ""
    (defaultCursorFormat resx resy) defOpts
  , _fps    = Just $
    FPS       True
    (Format CC resx resy (-0.2) (-0.0) (0.0) 0.015 0.2) defOpts
  , _speed  = Just $
    TextField True ["speed : 0.777"]
    (Format CC resx resy 0.0 0.0 0.0 0.015 0.2) defOpts    
  , _title  = Just $
    TextField True ["a space odyssey"] 
    (Format CC resx resy (-0.2) (0.0) 0.0 0.03 (fontSize 4)) defOpts
  , _gizmo     = Just $ Icon True "" 1 (Format CC resx resy (-0.2) (0.0) (0) 0.0 0.5) defOpts
  , _guiSwitch = MainGUI'
  }

intrGUI :: (Int, Int) -> GUI
intrGUI res0@(resx, resy) =
  defGUI
  {
    _res    = res0
  , _cursor = Just $ Cursor True ""
    (defaultCursorFormat resx resy) defOpts
  , _xx = Just $
    TextField True ["PARAYA"] 
    (Format TC resx resy (0.19) (0.0) 0.0 0.08 (fontSize 5)) defOpts
  , _title = Just $
    TextField True ["a space odyssey"] 
    (Format TC resx resy (-0.2) (-0.25) 0.0 0.03 (fontSize 4)) defOpts
  , _strtB   = Just $
      Button True "NEW GAME" defBBox False False
    (Format CC resx resy (0.0) ( 0.0) 0.0 0.033 (fontSize 4)) defOpts
  , _optsB    = Just $
      Button True "OPTIONS" defBBox False False
    (Format CC resx resy (0.0) (-0.075) 0.0 0.033 (fontSize 4)) defOpts
  , _quitB    = Just $
      Button True "QUIT"    defBBox False False
    (Format CC resx resy (0.0) (-0.15) 0.0 0.033 (fontSize 4)) defOpts
  , _gizmo    = Nothing
  , _guiSwitch     = IntrGUI'
  }

optsGUI :: (Int, Int) -> GUI
optsGUI res0@(resx, resy) =
  defGUI
  {
    _res       = res0
  , _cursor    = Just $ Cursor True "" (defaultCursorFormat resx resy) defOpts
  , _backB     = Just $ Button True "< BACK" defBBox False False (Format CC resx resy (0.0) (0.0) 0.0  0.085 1.0) defOpts
  , _gizmo     = Nothing
  , _guiSwitch = OptsGUI'
  }

mainGUI :: (Int, Int) -> GUI
mainGUI res0@(resx,resy) =
  defGUI
  {
    _res       = res0
  , _fps       = Just $ FPS       True (Format TC resx resy (-0.1) (-0.05) (0.0) 0.015 0.2) defOpts
  , _speed     = Just $ TextField True ["speed : 0.777"] (Format BC resx resy 0.53 0.094 0.0 0.015 0.2) defOpts
  , _cursor    = Just $ Cursor    True ""
    (defaultCursorFormat resx resy) defOpts
  , _gizmo     = Just $ Icon      True "" 1 (Format TL resx resy (0) (0) (0) 0.0 0.5) defOpts
  , _guiSwitch = MainGUI'
  }

infoGUI :: (Int, Int) -> GUI
infoGUI res0@(resx, resy) =
  defGUI
  {
    _res   = res0
  , _fps   = Just $ FPS True (Format TC resx resy 0.0 (0.0) (0.0) 0.085 1.0) defOpts
  , _infos = Just $
    [ TextField True ["planet ebanat"] (Format BC resx resy 0.0 0.0 (0.0) 0.085 1.0) defOpts
    , TextField True ["population: 11,000,000,000 ebanats"] (Format TC resx resy (-0.15) (0.0) 0.0 0.085 1.0) defOpts
    ]
  , _cursor = Just $ Cursor True "" (defaultCursorFormat resx resy) defOpts
  , _gizmo  = Nothing
  , _guiSwitch   = InfoGUI'
  }
