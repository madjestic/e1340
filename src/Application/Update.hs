{-# LANGUAGE Arrows #-}

module Application.Update
  ( appRun
  , handleExit
  , appIntro
  , appMain
  ) where

import FRP.Yampa
import SDL          hiding ((*^), Event, Mouse)
import Data.Functor        (($>))
import Control.Lens ((^.))

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Application.Application
import App

appRun :: Application -> SF AppInput Application
appRun app' =
  loopPre app' $
  proc (input, appState) -> do
    as <- case _interface appState of
            Intro        -> appIntro -< (input, appState)
            Main Default -> appMain    app' { _interface = Main Default } -< input
            Info Earth   -> planetView (app'^.main)   -< (input, appState)
            --Info _       -> appIntro -< (input, appState)
            _ -> appMain app' { _interface =  Main Default } -< input
    returnA -< (as, as)

appIntro :: SF (AppInput, Application) Application
appIntro  = 
  switch sf cont
     where sf =
             proc (input, appState) -> do
               introState <- returnA -< appState
               mainState  <- returnA -< appState { _interface =  Main Default }
               skipE      <- keyInput SDL.ScancodeSpace "Pressed" -< input
               waitE      <- after 5.0 () -< ()
               returnA    -< (introState, (skipE `lMerge` waitE) $> mainState)
           cont app' =
             proc _ -> returnA -< app'

-- uppRun 
--      \ appMain -> app', interface
--              \ updateApp -> app'
--                        \ updateSelectable -> selectable
-- 
--

appMain :: Application -> SF AppInput Application
appMain app0 = 
  switch sf cont
     where sf =
             proc input -> do
               app'        <- updateApp (fromApplication app0) -< input
               reset       <- keyInput SDL.ScancodeSpace "Pressed" -< input

               -- let result = app0 { _main = app' }
               let result = app0 { _main      = app'
                                 , _interface = selected app' }
               returnA     -< (result, reset $> app0 { _interface = Intro} )
                 where
                   selected app' =
                     case _selected app' of
                       [] -> Main Default :: Interface
                       _  -> Info Earth
                              
           cont = appRun

--planetView :: Application -> SF AppInput Application
planetView :: App -> SF (AppInput, Application) Application
planetView app0  = 
  switch sf cont
     where sf =
             proc (input, appState) -> do
               currState  <- returnA -< appState
               mainState  <- returnA -< appState { _interface =  Main Default
                                                 , _main = app0 {_selected = []}}
               skipE      <- keyInput SDL.ScancodeQ "Pressed" -< input
               waitE      <- after 5.0 () -< ()
               returnA    -< (currState, (skipE `lMerge` waitE) $> mainState)
           cont app' =
             proc _ -> returnA -< app'

handleExit :: SF AppInput Bool
handleExit = quitEvent >>^ isEvent
