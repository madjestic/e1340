--{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment
import System.Exit
import Data.List.Split

import Graphics.RedViz.Project as Project

import Projects.Test
import Projects.SolarSystem
--import Projects.InfoEarth
import Projects.Graph
import Projects.Body3
--import Projects.PlanetSputnik
--import Projects.SolarSystemMini
--import Projects.SolarSystemExtended
--import Projects.Curve

-- import Debug.Trace as DT

-- | This script generates a project file
-- |  example:
-- |  $ cabal run genProject Test
-- | TODO: should generate a project file, based on CLI ToArgs
-- | e.g.: `$ cabal run genProject Foo 800 600 "models/model.bgeo" "textures/texture.jpg" 0 0 0`

main :: IO ()
main = getArgs >>= parseArgs >>= splitter >>= \(projectName, filePath) ->
  case projectName of
  "testred"       -> Project.write (Projects.Test.projectTestRed   1280 720) filePath
  "testgreen"     -> Project.write (Projects.Test.projectTestGreen 1280 720) filePath 
  "testblue"      -> Project.write (Projects.Test.projectTestBlue  1280 720) filePath 
  --"testchecker"   -> Project.write Projects.Test.projectTestChecker filePath
  --"testcheckeroffset" -> Project.write Projects.Test.projectTestCheckerOffset filePath
  --"solarsystem"   -> Project.write Projects.SolarSystem.project     filePath
  "solarsystem"   -> Project.write (Projects.SolarSystem.project   1280 720) filePath
  --"solar_system_mini" -> Project.write Projects.SolarSystemMini.project     filePath
  --"solar_system_extended" -> Project.write Projects.SolarSystemExtended.project     filePath
--  "infoearth"     -> Project.write Projects.InfoEarth.project       filePath
  "test"          -> Project.write (Projects.Test.project 1280 720)            filePath
  --"test2"         -> Project.write Projects.Test.test2              filePath
  "graph"         -> Project.write Projects.Graph.project           filePath
  "options"       -> Project.write (Projects.Test.options 1280 720) filePath
  "body3"         -> Project.write Projects.Body3.project           filePath
  --"planetsputnik" -> Project.write Projects.PlanetSputnik.project   filePath
  --"curve"         -> Project.write Projects.Curve.project           filePath
  _ -> Project.write (Projects.Test.project 1280 720)               filePath
                                   
splitter :: String -> IO (String, String)
splitter fs = return (last $ splitOn "/" fs, fs )

parseArgs :: [[Char]] -> IO String
parseArgs ["-h"] = help    >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = getContents
parseArgs fs     = putStrLn ("Generating project file: " ++ show (head fs)) >> return (concat fs)

help :: IO ()
help    = putStrLn "Usage: genProject [-- -vh] [file ..]"

version :: IO ()
version = putStrLn "genProject 0.1"

exit :: IO String
exit    = exitWith ExitSuccess

die :: IO String
die     = exitWith (ExitFailure 1)
