{-# LANGUAGE Arrows #-}

module Grapher.Application.Update
  (
    mainLoop
  , handleExit
  , appIntro
  , appMain
  ) where

import FRP.Yampa
import SDL          hiding ((*^), Event, Mouse, Debug)
import Data.Functor        (($>))
import Control.Lens

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Grapher.Application.Application as Appl
import Grapher.App as App
import Grapher.GUI

-- import Debug.Trace    as DT

mainLoop :: Application -> SF AppInput Application
mainLoop app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- case Appl._gui app0 of
              IntrGUI {} -> appIntro app0 -< (input, gameState)
              MainGUI {} -> appMain  app0 -< (input, gameState)
    returnA -< (app1, app1)

appIntro :: Application -> SF (AppInput, Application) Application
appIntro app0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateIntroApp (fromApplication app0) -< (input, app1^.Appl.main)
               skipE <- keyInput SDL.ScancodeSpace "Pressed" -< input
               
               let
                 result = app1 { _intr = app' }
               returnA -< ( result
                          , skipE $>
                            result { Appl._gui = app0 ^. main . App.gui}
                          )
               
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
                       _  -> appl' ^. main . App.gui

           cont arg =
             proc (input', _) -> do
               result <- mainLoop arg -< input'
               returnA -< result

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
