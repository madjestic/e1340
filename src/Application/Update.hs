{-# LANGUAGE Arrows #-}

module Application.Update
  (
    mainLoop
  , handleExit
  , appIntro
  , appMain
  , appOpts  
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
import App.App as App (inpQuit, Interface(..)) 

mainLoop :: Application -> SF AppInput Application --SF (AppInput, Application) Application
mainLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- case _interface app0 of
              IntroApp        -> appIntro app0  -< (input, gameState)
              OptionsApp      -> appOpts  app0  -< (input, gameState)
              MainApp Default -> appMain  app0  -< (input, gameState)
              InfoApp Earth   -> appInfo  app0  -< (input, gameState)
              _ -> appMain app0  -< (input, gameState)
    returnA -< (app1, app1)

switchApp :: Event () -> Event () -> Appl.Interface
switchApp e0 e1 = ui
  where
    ui = if isEvent e0 then MainApp Default
         else OptionsApp

appIntro :: Application -> SF (AppInput, Application) Application
appIntro app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateIntroApp (fromApplication app0) -< (input, app1^.Appl.main)
               skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               qE    <- arr Appl._inpQuit >>> edge -< app1

               let
                 Intro inpQuit_ _ = app' ^. ui
                 result = app1 { _intro        = app'
                               , Appl._inpQuit = inpQuit_
                               }
                          
               returnA    -< (result, lMerge qE skipE $> result { _interface =  switchApp qE skipE })
               
           cont arg =
             proc (input', app') -> do
               result <- mainLoop arg -< input'
               returnA -< result

appOpts :: Application -> SF (AppInput, Application) Application
appOpts app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateOptsApp (fromApplication app0) -< (input, app1^.Appl.main)
               -- skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               backE <- arr Appl._inpQuit >>> edge -< app1

               let
                 Opts inpBack_ = app' ^. ui
                 result = app1 { _intro        = app'
                               , Appl._inpBack = inpBack_
                               }
                          
               returnA    -< (result, backE $> result { _interface =  IntroApp })
               
           cont arg =
             proc (input', app') -> do
               result <- mainLoop arg -< input'
               returnA -< result
               
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
               -- result <- appRun arg -< (input', app')
               result <- mainLoop arg -< input'
               returnA -< result

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
               --result <- mainLoop arg -< input'
               result <- mainLoop arg -< input'
               returnA -< result
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
