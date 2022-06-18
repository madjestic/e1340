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
--import Application.Interface (inpOpts)
--import App.App as App (inpQuit, Interface(..))
import GUI

mainLoop :: Application -> SF AppInput Application --SF (AppInput, Application) Application
mainLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- case Appl._gui app0 of
              IntrGUI {} -> appIntro app0 -< (input, gameState)
              OptsGUI {} -> appOpts  app0 -< (input, gameState)
              MainGUI {} -> appMain  app0 -< (input, gameState)
              InfoGUI {} -> appInfo  app0 -< (input, gameState)
    returnA -< (app1, app1)

switchApp :: Application -> Event () -> Event () -> GUI
switchApp appl optsE quitE = ui'
  where
    ui' =
      -- TODO : rename main to mainApp ?
      if isEvent optsE
      then appl ^. opts . App.gui
      else appl ^. main . App.gui

appIntro :: Application -> SF (AppInput, Application) Application
appIntro app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateIntroApp (fromApplication app0) -< (input, app1^.Appl.main)
               
               skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               quitE <- arr _inpQuit >>> edge -< app' ^. App.gui
               optsE <- arr _inpOpts >>> edge -< app' ^. App.gui

               let
                 result = app1 { Appl._gui  = app' ^. App.gui
                               , _intr      = app' }
                          
               returnA -< ( result
                          , catEvents [quitE, optsE] $>
                            result { Appl._gui =
                                     --switchApp app1 (DT.trace ("optsE : " ++ show (isEvent optsE)) optsE) (DT.trace ("quitE : " ++ show (isEvent quitE))quitE) })
                                     switchApp app1 optsE quitE })
               
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
               backE <- arr _inpBack >>> edge -< app' ^. App.gui

               let
                 result = app1 { _opts         = app' }
                          
               returnA    -< ( result
                             , backE $>
                               result { Appl._gui = app1 ^. intr . App.gui })
               
           cont arg =
             proc (input', app') -> do
               result <- mainLoop arg -< input'
               returnA -< result
               
appMain :: Application -> SF (AppInput, Application) Application
appMain app0 = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'        <- updateMainApp (fromApplication app0) -< (input, app1 ^. main)
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input
               zE          <- keyInput SDL.ScancodeZ     "Pressed" -< input

               let
                 result =
                   app1 { _main = app' }
                          
               returnA     -< if isEvent reset 
                              then (result, reset $> app0   { Appl._gui = app1 ^. intr . App.gui } )
                              else (result, zE    $> result { Appl._gui = fromSelected app1 app' } )
                 where
                   fromSelected appl' app' =
                     case _selected app' of
                       [] -> appl' ^. main . App.gui
                       _  -> appl' ^. info . App.gui

           cont arg =
             proc (input', app') -> do
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

               returnA     -< (result, exitE $> result { Appl._gui = app1 ^. main . App.gui } )

           cont arg =
             proc (input', _) -> do
               result <- mainLoop arg -< input'
               returnA -< result
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
