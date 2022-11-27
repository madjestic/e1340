{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import Graphics.RedViz.Widget

--import Debug.Trace    as DT
import Data.Maybe (fromJust)

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

switchOptsMain :: Application -> Event () -> Event () -> GUI
--switchOptsMain appl optsE startE = ui'
switchOptsMain appl optsE _ = ui'
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
               
               --skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               strtE <- arr isPressed >>> edge -< (app' ^? App.gui . strtB :: Maybe Widget)
               optsE <- arr isPressed >>> edge -< (app' ^? App.gui . optsB :: Maybe Widget)
               quitE <- arr isPressed >>> edge -< (app' ^? App.gui . quitB :: Maybe Widget)
               
               let
                 strtB' = fromJust $ app0 ^? intr . App.gui . strtB
                 optsB' = fromJust $ app0 ^? intr . App.gui . optsB
                 quitB' = fromJust $ app0 ^? intr . App.gui . quitB
                 
                 result = app1 { _intr = app'
                               , _quit = isEvent quitE }
                 result'= app1 { _quit = isEvent quitE
                               , _intr = app' & App.gui . optsB .~ optsB'
                                              & App.gui . quitB .~ quitB'
                                              & App.gui . strtB .~ strtB'}
                          
               returnA -< ( result
                          , catEvents [strtE, optsE] $>
                            result' { Appl._gui =
                                      switchOptsMain app0
                                      (optsE $> ())
                                      (strtE $> ())
                                    })
               
           cont arg =
             proc (input', _) -> do
               result <- mainLoop arg -< input'
               returnA -< result

appOpts :: Application -> SF (AppInput, Application) Application
appOpts app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateOptsApp (fromApplication app0) -< (input, app1^.Appl.main)
               backE <- arr isPressed >>> edge -< (app' ^? App.gui . backB :: Maybe Widget)
               
               let
                 backB' = fromJust $ app0 ^? opts . App.gui . backB
                 
                 result = app1 { _opts = app' }
                 result'= app1 { _opts = app' & App.gui . backB .~ backB'}
                          
               returnA    -< ( result
                             , backE $>
                               result' { Appl._gui = app1 ^. intr . App.gui })
               
           cont arg =
             proc (input', _) -> do
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
             proc (input', _) -> do
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
