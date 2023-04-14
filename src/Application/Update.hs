{-# LANGUAGE Arrows #-}
--{-# LANGUAGE OverloadedRecordDot #-}

module Application.Update
  (
    mainLoop
  , handleExit
  , appIntr
  , appMain
  , appOpts  
  ) where

import FRP.Yampa
import SDL          hiding ((*^), Event, Mouse, Debug)
import Data.Functor        (($>))
import Control.Lens
import Data.Maybe (fromJust)

import Graphics.RedViz.Input.FRP.Yampa.AppInput

import Application.Application as Appl
import App
import GUI
import Graphics.RedViz.Widget
import System.IO.Unsafe

--import Debug.Trace    as DT

mainLoop :: Application -> SF AppInput Application --SF (AppInput, Application) Application
mainLoop appl0 =
  loopPre appl0 $
  proc (input, gameState) -> do
    app1 <- case appl0 ^. Appl.gui.guiSwitch of --case Appl._gui appl0 of
              IntrGUI' -> appIntr appl0 -< (input, gameState)
              OptsGUI' -> appOpts appl0 -< (input, gameState)
              MainGUI' -> appMain appl0 -< (input, gameState)
              InfoGUI' -> appInfo appl0 -< (input, gameState)
    returnA -< (app1, app1)

switchGUI' :: Application -> Event () -> Event () -> GUI'
switchGUI' appl strtE optsE = ui'
  where
    ui'
      | isEvent optsE  = OptsGUI'
      | isEvent strtE  = MainGUI'
      | otherwise = error "Unknown GUI' Option" undefined

appIntr :: Application -> SF (AppInput, Application) Application
appIntr appl0  = 
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateIntroApp (fromApplication appl0) -< (input, app1^.Appl.intr)

               strtE <- arr isPressed >>> edge -< (app' ^. App.gui . strtB)
               optsE <- arr isPressed >>> edge -< (app' ^. App.gui . optsB)
               quitE <- arr isPressed >>> edge -< (app' ^. App.gui . quitB)
               
               let
                 strtB' = fromJust $ appl0 ^? intr . App.gui . strtB
                 optsB' = fromJust $ appl0 ^? intr . App.gui . optsB
                 quitB' = fromJust $ appl0 ^? intr . App.gui . quitB

                 result = app1 { _intr = app'
                               , _quit = isEvent quitE }
                 result'= app1 { _quit = isEvent quitE
                               , _intr = app' & App.gui . optsB .~ optsB'
                                              & App.gui . quitB .~ quitB'
                                              & App.gui . strtB .~ strtB'}
               returnA -< ( result
                          , catEvents [strtE, optsE] $>
                            result' { Appl._gui =
                                      app' ^. App.gui & guiSwitch .~ switchGUI' appl0 strtE optsE
                                    })
               
           cont arg =
             proc (input', _) -> do
               result <- mainLoop arg -< input'
               returnA -< result

appOpts :: Application -> SF (AppInput, Application) Application
appOpts app0  = -- proc (input, app1) -> do returnA -< app0
  switch sf cont
     where sf =
             proc (input, app1) -> do
               app'  <- updateOptsApp (fromApplication app0) -< (input, app1^.Appl.main)
               backE <- arr isPressed >>> edge -< (app' ^. App.gui . backB :: Maybe Widget)
               
               let
                 backB' = fromJust $ app0 ^? opts . App.gui . backB
                 
                 result = app1 { _opts = app' }
                 result'= app1 { _opts = app' & App.gui . backB .~ backB'}
                          
               returnA    -< ( result
                             , backE $>
                               result' { Appl._gui = app1 ^. intr . App.gui })
               
           cont arg =
             proc (input', _) -> do
               result  <- mainLoop arg -< input'
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
