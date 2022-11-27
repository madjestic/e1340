{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.ByteString   as BS   (writeFile)
import Data.Store                (encode)
import Data.Time.Clock.Compat
import Data.Time.LocalTime.Compat

import Options.Applicative

import Graphics.RedViz.PGeo

--import Debug.Trace as DT

data GeoArgs =
  GeoArgs
  {
    fileIn  :: FilePath
  , fileOut :: FilePath
  , skip    :: Bool
  } deriving Show

geoArgs :: Parser GeoArgs
geoArgs = GeoArgs
       <$> strOption
          ( long "input"
         <> short 'i'
         <> metavar "FILE"
         <> value "./models/model.pgeo"
         <> help "Read source model file")
       <*> strOption
          ( long "output"
         <> short 'o'
         <> metavar "FILE"
         <> value "./models/model.bgeo"
         <> help "Write output model file")
       <*> switch
          ( long "skip"
         <> short 's'
         <> help "Whether to perform the indexing" )


formatTime' :: UTCTime -> String
formatTime' = take 8 . show . timeToTimeOfDay . utctDayTime

formatTime'' :: UTCTime -> String
formatTime'' = take 8 . drop 11 . show . (utcToLocalTime (TimeZone 60 False "AMS"))

writeBGeo :: FilePath -> VGeo -> IO ()
writeBGeo fileOut' vgeo =
  do
    BS.writeFile fileOut' (encode vgeo)

main :: IO ()
main = do
  let
    opts = info (geoArgs <**> helper)
        ( fullDesc
       <> progDesc "(optionally) reduce repetitions in a pgeo and save as a bgeo"
       <> header   "(optionally) index (pgeo List -> bgeo Set)" )
  args <- execParser opts
  putStrLn $ "args :" ++ show args

  pgeo <- readPGeo (fileIn args)
  putStrLn "running indexer..."
  let vgeo = fromPGeo pgeo
  currentTime' <- getCurrentTime
  putStrLn $ "Finished converting PGeo: " ++ formatTime'' currentTime'
  
  writeBGeo (fileOut args) vgeo
  currentTime'' <- getCurrentTime
  putStrLn $ "Finished writing BGeo   : " ++ formatTime'' currentTime''

