{-# LANGUAGE Arrows #-}

module Application.Update
  ( appRun
  , appLoop
  , handleExit
  , appIntroLoop
  , appMain
  ) where

import FRP.Yampa
import SDL          hiding ((*^), Event, Mouse, Debug)
import Data.Functor        (($>))
import Control.Lens-- ((^.))

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Application.Application as Appl
import App
import GUI

import Debug.Trace    as DT
import App.App as App (inpQuit) 

appLoop :: Application -> SF AppInput Application
appLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appRun app0 -< (input, gameState)
    returnA -< (app1, app1)

appRun :: Application -> SF (AppInput, Application) Application
appRun app0 =
  --proc (input, app') -> do
  proc (input, _) -> do
    as <- case _interface app0 of
            IntroApp        -> appIntroPre app0 -<  input
            -- OptionsApp      -> appOptsPre  app0 -<  input
            MainApp Default -> appMainPre  app0 -<  input
            InfoApp Earth   -> appInfoPre  app0 -<  input
            _ -> appMainPre app0  -< input
    returnA -< as

appIntroPre :: Application -> SF AppInput Application
appIntroPre app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appIntroLoop app0 -< (input, gameState)
    returnA -< (app1, app1)

appIntroLoop :: Application -> SF (AppInput, Application) Application
appIntroLoop app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateIntroApp (fromApplication app0) -< (input, app1^.Appl.main)
               skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               qE    <- arr Appl._inpQuit >>> edge -< app1

               let
                 result = app1 { _intro        = app'
                               , Appl._inpQuit = app' ^. App.inpQuit
                               }
               returnA    -< (result, lMerge qE skipE $> result { _interface =  MainApp Default })
           cont = appRun

initLoop :: Application -> SF AppInput Application --SF (AppInput, Application) Application
initLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- case _interface app0 of
              IntroApp -> appIntroLoop app0  -< (input, gameState)
            -- OptionsApp      -> appOptsPre  app0 -<  input
              MainApp Default -> appMainPre  app0 -<  input
            -- InfoApp Earth   -> appInfoPre  app0 -<  input
              _ -> appIntroLoop app0  -< (input, gameState)
    returnA -< (app1, app1)

-- appIntroLoop :: Application -> SF (AppInput, Application) Application
-- appIntroLoop app0  = 
--   switch sf cont
--      where sf =
--              proc (input, app1) -> do
--                app'  <- updateIntroApp (fromApplication app0) -< (input, app1^.Appl.main)
--                skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
--                qE    <- arr Appl._inpQuit >>> edge -< app1

--                let
--                  result = app1 { _intro        = app'
--                                , Appl._inpQuit = app' ^. App.inpQuit
--                                }
--                returnA    -< (result, lMerge qE skipE $> result { _interface =  MainApp Default })
--            cont = appRun

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
               app'        <- updateMainApp (fromApplication app0) -< (input, app1^.Appl.main)
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input
               zE          <- keyInput SDL.ScancodeZ     "Pressed" -< input

               let
                 result =
                   app1 { _main = app' }
                          
               returnA     -< if isEvent reset 
                              then (result, reset $> app0   { _interface = IntroApp } )
                              else (result, zE    $> result { _interface = fromSelected app' } )
                 where
                   fromSelected app' =
                     case _selected app' of
                       [] -> MainApp Default
                       _  -> InfoApp Earth

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
               app'        <- updateMainApp (fromApplication app0) -< (input, app1^.Appl.info)
               exitE       <- keyInput SDL.ScancodeZ "Pressed" -< input

               let
                 result =
                   app1 { Appl._info = app' }

               returnA     -< (result, exitE $> result { _interface = MainApp Default } )

           cont arg =
             proc (input', _) -> do
               result <- appLoop arg -< input'
               returnA -< result
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
