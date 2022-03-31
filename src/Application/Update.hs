{-# LANGUAGE Arrows #-}

module Application.Update
  ( appRun
  , appRunPre
  , handleExit
  , appIntro
  , appMain
  ) where

import FRP.Yampa
import SDL          hiding ((*^), Event, Mouse)
import Data.Functor        (($>))
import Control.Lens ((^.), (&), (.~), view)

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Application.Application as Appl
import App

import Debug.Trace    as DT

appRunPre :: Application -> SF AppInput Application
appRunPre app0 =
  loopPre app0 $
  proc (input, gameState) -> do
    app1 <- appRun app0 -< (input, gameState)
    returnA -< (app1, app1)

appRun :: Application -> SF (AppInput, Application) Application
appRun app0 =
  proc (input, app') -> do
    as <- case _interface app0 of
            Intro        -> appIntro   app0 -< (input, app')
            Main Default -> appMainPre  app0 -< input
            Info Earth   -> planetView app0  -< (input, app')
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
           -- cont app' =
           --   proc _ -> returnA -< app'

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
               app'        <- updateApp (fromApplication app0) -< input
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input
               zE          <- keyInput SDL.ScancodeZ     "Pressed" -< input

               let
                 result = app1 { _main      = app' }
                          
               returnA     -< if isEvent reset
                              then (result, reset $> app0 { _interface = Intro} )
                              else (result, zE    $> app1 { _interface = fromSelected app'} )
               
                 where
                   fromSelected app' =
                     case _selected app' of
                       [] -> Main Default
                       _  -> Info Earth
           cont = appRun
           -- cont app' =
           --   proc _ -> returnA -< app'

planetView :: Application -> SF (AppInput, Application) Application
planetView app0 = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'        <- updateApp (fromApplication app0) -< input
               exitE       <- keyInput SDL.ScancodeZ "Pressed" -< input

               let
                 result =
                   app1 { _main = app' }
                          
               returnA     -< (result, exitE $> app0 { _interface = Main Default
                                                     , _main = app0^.main & selected   .~ []
                                                                          & selectable .~ [] } )
           cont = appRun
           
handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
