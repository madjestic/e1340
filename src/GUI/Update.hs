{-# LANGUAGE Arrows #-}

module GUI.Update
  ( updateGUIPre
  , updateGUI
  ) where

import Control.Lens
import Data.Functor                          (($>))
import FRP.Yampa

import Graphics.RedViz.Widget
import Graphics.RedViz.Input.Mouse
import Graphics.RedViz.Input.FRP.Yampa.Update.Mouse

import GUI.GUI
import Graphics.RedViz.Input (AppInput)

updateGUIPre :: GUI -> SF AppInput GUI
updateGUIPre gui0 =
  loopPre gui0 $
  proc (input, gui) -> do
    gui1 <- updateGUI gui0 -< input
    returnA -< (gui1, gui1)

updateGUI :: GUI -> SF AppInput GUI
updateGUI gui0 =
  proc input -> do
    rec gui  <- iPre gui0      -< gui'
        gui' <- updateGUI' gui0 -< (input, gui)
    returnA -< gui

updateGUI' :: GUI -> SF (AppInput, GUI) GUI
updateGUI' gui0@(IntroGUI _ _ exitB0 _) =
  proc (input, gui) -> do
    case gui of
      IntroGUI _ _ exitB_ cursor_ -> do
        cursor' <- updateCursor        -< (input, cursor_)
        exitB'  <- updateButton exitB0 -< (cursor', exitB_)
        let
          result =
            gui
            { _exitB  = exitB'
            , _cursor = cursor' }
        returnA -< result
      _ -> returnA -< gui0
updateGUI' gui = proc _ -> do returnA -< gui

updateButton :: Widget -> SF (Widget, Widget) Widget
updateButton btn0 =
  switch sf cont
  where
    sf = 
      proc (crsr, btn) -> do
        (btn', moE) <- arr $ uncurry mouseOverE -< (crsr, btn)
        returnA -< (btn0, moE $> btn')
    cont = updateButton'

updateButton' :: Widget -> SF (Widget, Widget) Widget
updateButton' btn0 =
  switch sf cont
  where
    sf = 
      proc (crsr, btn) -> do
        (btn', moE) <- arr $ uncurry mouseOverE' -< (crsr, btn)
        returnA -< (btn0, moE $> btn')
    cont = updateButton

mouseOverE :: Widget -> Widget -> (Widget, Event ())
mouseOverE crsr btn = (btn', event')
  where
    Button active__ lable__ bbox__ pressed__ format__ = btn
    Cursor active_  lable_  coords_                   = crsr
    
    --btn'   = Button active__ lable__ bbox__ pressed__ format__
    btn'   = Button active__ "*exit*" bbox__ pressed__ format__
    --event' = if insideBBox bbox__ coords_ then Event () else NoEvent
    event' = if insideBBox bbox__ coords_ &&
                lable__ == "exit"
             then Event ()
             else NoEvent
    --event' = if True then Event () else NoEvent
      where
        insideBBox (BBox x0 y0 x1 y1) (x, y) = over'
          where over' = x0 <= x && x <= x1 &&
                        y0 <= y && y <= y1
                        
--mouseOverE :: Widget -> Widget -> (Event (Widget)) -- ??? Possible?

mouseOverE' :: Widget -> Widget -> (Widget, Event ())
mouseOverE' crsr btn = (btn', event')
  where
    Button active__ lable__ bbox__ pressed__ format__ = btn
    Cursor active_  lable_  coords_                   = crsr
    
    --btn'   = Button active__ lable__ bbox__ pressed__ format__
    btn'   = Button active__ "exit" bbox__ pressed__ format__
    --event' = if insideBBox bbox__ coords_ then Event () else NoEvent
    event' = if not (insideBBox bbox__ coords_) &&
                lable__ == "*exit*"
             then Event ()
             else NoEvent
    --event' = if True then Event () else NoEvent
      where
        insideBBox (BBox x0 y0 x1 y1) (x, y) = over'
          where over' = x0 <= x && x <= x1 &&
                        y0 <= y && y <= y1

updateCursor :: SF (AppInput, Widget) Widget
updateCursor =
  proc (input, Cursor active_ lable_ coords_)-> do
    (mouse', mevs) <- updateMouse -< input
    let
      coords' = mouse' ^. pos
      result' = (Cursor active_ lable_ coords')
    returnA -< result'
