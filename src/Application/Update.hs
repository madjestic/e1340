{-# LANGUAGE Arrows #-}

module Application.Update
  ( appRun
  , appLoop
  , handleExit
  , appIntro
  , appMain
  ) where

import FRP.Yampa
import SDL          hiding ((*^), Event, Mouse, Debug)
import Data.Functor        (($>))
import Control.Lens ((^.), (&), (.~), view)
import Control.Concurrent ( swapMVar, newMVar, readMVar, MVar, putMVar, takeMVar )

import Control.Monad.IO.Class
import System.IO.Unsafe

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Application.Application as Appl
import App

import Debug.Trace    as DT
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable hiding (_debug)

-- Kleisli Arrows?  How to embed IO in arrows?
-- arrIO _ =
--   proc _ -> do
--     counter <- newMVar 0 :: IO (MVar Int)
--     takeMVar counter >>= print
--     returnA -< ()

formatDebug :: Application -> String
formatDebug app0 = "main : " ++ show (app0 ^. Appl.main . App.playCam . controller . transform . translation) ++ "\n" -- ++
                   -- "info : " ++ show (app0 ^. Appl.info . App.playCam . controller . transform . translation) ++ "\n"

formatDebug' :: App -> String
formatDebug' app0 = -- show (app0 ^. debug) ++
                    show (app0 ^. App.playCam . controller . transform . translation)

appLoop :: Application -> SF AppInput Application
appLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appRun app0 -< (input, gameState)
    returnA -< (app1, app1)

appRun :: Application -> SF (AppInput, Application) Application
appRun app0 =
  proc (input, app') -> do
    as <- case _interface app0 of
            Intro        -> appIntro   app0 -< (input, app')
            Main Default -> appMainPre app0 -< input
            Main Debug   -> appMain    app0 -< (input, app')
            Info Earth   -> appInfoPre app0 -< input
            _ -> appMainPre app0  -< input
    returnA -< as

appIntro :: Application -> SF (AppInput, Application) Application
appIntro app0  = 
  switch sf cont
     where sf =
             proc (input, app') -> do
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               returnA    -< (app0, skipE $> app0 { _interface =  Main Default })
           cont = appRun

appMainPre :: Application -> SF AppInput Application
appMainPre app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appMain app0 -< (input, gameState)
    returnA -< (app1, app1)

appMain :: Application -> SF (AppInput, Application) Application
appMain app0 = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'        <- updateApp (fromApplication app0) -< (input, app1^.Appl.main)
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input
               zE          <- keyInput SDL.ScancodeZ     "Pressed" -< input

               let
                 result =
                   app1 { _main = app' }
                          
               returnA     -< if isEvent reset
                              then (result, reset $> app0 { _interface = Intro } )
                              else (result, zE    $> result { _interface = fromSelected app' } )
                              --else (result, zE    $> app1 { _interface = fromSelected (DT.trace ("debug main : " ++ "\n" ++ formatDebug result) app')} )
               
                 where
                   fromSelected app' =
                     case _selected app' of
                       [] -> Main Default
                       _  -> Info Earth

           cont app0 =
             proc (input', app') -> do
               result <- appLoop app0 -< input'
               returnA -< result

appInfoPre :: Application -> SF AppInput Application
appInfoPre app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appInfo app0 -< (input, gameState)
    returnA -< (app1, app1)

appInfo :: Application -> SF (AppInput, Application) Application
appInfo app0 = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'        <- updateApp (fromApplication app0) -< (input, app1^.Appl.info)
               exitE       <- keyInput SDL.ScancodeZ "Pressed" -< input

               let
                 result =
                   app1 { Appl._info = app' }

                 result' =
                   app1
                   { _interface = Main Default
                   , Appl._info = app' }
                          
               returnA     -< (result, exitE $> result { _interface = Main Default } )

           cont app0 =
             proc (input', app') -> do
               result <- appLoop app0 -< input'
               returnA -< result
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
