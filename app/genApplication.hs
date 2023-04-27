module Main where

import Application.Application as A
import Control.Lens
import System.Environment
import System.Exit
import Data.List.Split
import Data.List (intercalate)  

import Graphics.RedViz.Project.Project as P hiding (resx, resy, _resx, _resy)

import Projects.SolarSystem
import Projects.Test

main :: IO ()
main = do --getArgs >>= parseArgs >>= splitter >>= \(applName, filePath) ->
  args     <- getArgs
  args'    <- parseArgs args
  let
    mode     = head . words $ args'
    filePath = last . words $ args'
  case mode of
    "-a" -> genApplication filePath
    "-p" -> previewProject filePath
    _    -> error $ "wrong input: " ++ show mode

previewProject :: FilePath -> IO ()
previewProject fp = splitter fp >>= \(applName, filePath) -> do
  let pappl  = previewPreApplication filePath
      resx' = pappl ^. resx
      resy' = pappl ^. resy

  A.write pappl
  case applName of
    "test" -> do
      P.write (Projects.Test.project resx' resy') filePath
    "box2" -> do
      P.write (Projects.Test.box2 resx' resy') filePath
    "pnk" -> do
      P.write (Projects.Test.pnk resx' resy') filePath
    _ -> error $ "Project " ++ show filePath ++ " does not exist."

genApplication :: FilePath -> IO ()
genApplication fp = splitter fp >>= \(applName, filePath) -> do
  case applName of
    "solarsystem" -> do
      let pappl  = solarsystemPreApplication
          resx' = pappl ^. resx
          resy' = pappl ^. resy
      A.write pappl
      P.write (Projects.SolarSystem.project   resx' resy') "./projects/solarsystem"
      P.write (Projects.Test.project          resx' resy') "./projects/test"
      P.write (Projects.Test.projectTestRed   resx' resy') "./projects/testred"
      P.write (Projects.Test.projectTestGreen resx' resy') "./projects/testgreen"
      P.write (Projects.Test.projectTestBlue  resx' resy') "./projects/testblue"
      P.write (Projects.Test.options          resx' resy') "./projects/options"
    _ -> error $ "Application " ++ show filePath ++ " does not exist."

splitter :: String -> IO (String, String)
splitter fs = return (last $ splitOn "/" fs, fs )
parseArgs :: [String] -> IO String
parseArgs ["-h"] = help    >> exit
parseArgs ["-v"] = version >> exit
parseArgs []     = getContents
parseArgs fs     = putStrLn ("(re)Generating Application for project file: " ++ show fs) >> return (unwords fs)

help :: IO ()
help    = putStrLn "Usage: genProject [-- -vh] [file ..]"

version :: IO ()
version = putStrLn "genProject 0.1"

exit :: IO String
exit    = exitWith ExitSuccess

die :: IO String
die     = exitWith (ExitFailure 1)
