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
import Control.Lens ((^.))

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Application.Application as Appl
import App

-- import Debug.Trace    as DT

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
            Intro        -> appIntroPre app0 -<  input
            Main Default -> appMainPre  app0 -<  input
            Info Earth   -> appInfoPre  app0 -<  input
            _ -> appMainPre app0  -< input
    returnA -< as

appIntroPre :: Application -> SF AppInput Application
appIntroPre app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appIntro app0 -< (input, gameState)
    returnA -< (app1, app1)

appIntro :: Application -> SF (AppInput, Application) Application
appIntro app0  = 
  switch sf cont
     where sf =
             --proc (input, app') -> do
             proc (input, _) -> do
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
                 where
                   fromSelected app' =
                     case _selected app' of
                       [] -> Main Default
                       _  -> Info Earth

           cont arg =
             proc (input', app') -> do
               result <- appRun arg -< (input', app')
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

               returnA     -< (result, exitE $> result { _interface = Main Default } )

           cont arg =
             proc (input', _) -> do
               result <- appLoop arg -< input'
               returnA -< result
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
