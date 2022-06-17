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
import Application.Interface (inpOpts)
--import App.App as App (inpQuit, Interface(..)) 

mainLoop :: Application -> SF AppInput Application --SF (AppInput, Application) Application
mainLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- case _interface app0 of
              IntrApp _ _     -> appIntro app0  -< (input, gameState)
              OptsApp _       -> appOpts  app0  -< (input, gameState)
              MainApp         -> appMain  app0  -< (input, gameState)
              InfoApp Earth   -> appInfo  app0  -< (input, gameState)
              _ -> appMain app0  -< (input, gameState)
    returnA -< (app1, app1)

switchApp :: Application -> Event () -> Event () -> Interface
switchApp appl optsE quitE = ui'
  where
    ui' =
      -- TODO : rename main to mainApp ?
      if isEvent quitE
      then appl ^. main . ui
      else appl ^. opts . ui

appIntro :: Application -> SF (AppInput, Application) Application
appIntro app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateIntroApp (fromApplication app0) -< (input, app1^.Appl.main)
               --appE  <- arr id >>> edge -< app1
               
               
               skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               --ui'   <- updateInterface -< (input, app'
               quitE <- case app1 ^. interface of
                 IntrApp _ _ -> 
                   arr _inpQuit >>> edge -< app' ^. ui
                   
               optsE <- case app1 ^. interface of
                 IntrApp _ _ -> 
                   arr _inpOpts >>> edge -< app' ^. ui

               let
                 --IntrApp inpQuit_ inpOpts_ = app' ^. ui
                 result = app1 { _intro    = app'
                               -- , _interface    = app' ^. 
                               -- , Appl._inpQuit = inpQuit_
                               -- , Appl._inpOpts = inpOpts_
                               }
                          
               returnA    -< ( result
                             , catEvents [skipE, optsE] $>
                             --, appE $>
                               result { _interface =
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
               -- skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               --backE <- arr Appl._inpBack >>> edge -< app1
               backE <- case app1 ^. interface of
                 OptsApp _ ->
                   arr _inpBack >>> edge -< app' ^. ui

               let
                 --OptsApp inpBack_ = app' ^. ui
                 result = app1 { _opts         = app'
                               --, Appl._inpBack = inpBack_
                               }
                          
               --returnA    -< (result, backE $> result { _interface =  IntroApp })
               returnA    -< (result, backE $> result { _interface = app1 ^. intro . ui })
               
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
                              then (result, reset $> app0   { _interface = app1 ^. intro . ui } )
                              else (result, zE    $> result { _interface = fromSelected app1 app' } )
                 where
                   fromSelected appl' app' =
                     case _selected app' of
                       [] -> appl' ^. main . ui
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

               returnA     -< (result, exitE $> result { _interface = app1 ^. main . ui } )

           cont arg =
             proc (input', _) -> do
               --result <- mainLoop arg -< input'
               result <- mainLoop arg -< input'
               returnA -< result
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
