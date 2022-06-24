{-# LANGUAGE Arrows #-}

module GUI.Update
  ( updateGUIPre
  , updateGUI
  ) where

import Control.Lens
import Data.Functor                          (($>))
import FRP.Yampa
import Foreign.C                             (CInt)
import Data.Maybe (fromJust)

import Graphics.RedViz.Widget
import Graphics.RedViz.Input (AppInput)
import Graphics.RedViz.Input.Mouse
import Graphics.RedViz.Input.FRP.Yampa.Update.Mouse
import Graphics.RedViz.Input.FRP.Yampa.AppInput

import GUI.GUI
import Debug

-- import Debug.Trace    as DT

updateGUIPre :: GUI -> SF AppInput GUI
updateGUIPre gui0 =
  loopPre gui0 $
  proc (input, gui) -> do
    gui1 <- updateGUI gui0 -< input
    returnA -< (gui1, gui1)

updateGUI :: GUI -> SF AppInput GUI
updateGUI gui0 =
  proc input -> do
    rec gui  <- iPre gui0       -< gui'
        gui' <- updateGUI' gui0 -< (input, gui)
    returnA -< gui

updateGUI' :: GUI -> SF (AppInput, GUI) GUI
updateGUI' gui0@(IntrGUI {}) =
  proc (input, gui) -> do
    cursor' <- updateCursor        -< (input, _cursor gui)
    optsB'  <- updateButton (_res gui0) (_optsB gui0) -< (input, cursor', _optsB gui)
    quitB'  <- updateButton (_res gui0) (_quitB gui0) -< (input, cursor', _quitB gui)
    let
      result =
        gui
        { _optsB  = optsB'
        , _quitB  = quitB'
        , _cursor = cursor'
        }
        
    returnA -< result

updateGUI' gui0@(OptsGUI res cursor0 backB0) =
  proc (input, gui) -> do
    let OptsGUI res cursor_ backB_ = gui
    cursor' <- updateCursor            -< (input, cursor_)
    backB'  <- updateButton res backB0 -< (input, cursor', backB_)
    let
      result =
        gui
        { _backB  = backB'
        , _cursor = cursor'
        }
        
    returnA -< result

updateGUI' gui = proc _ -> do returnA -< gui

updateButton :: (Int, Int) -> Widget -> SF (AppInput, Widget, Widget) Widget
updateButton res btn0@(Button _ _ _ _ _ _) =
  proc (input, crsr, btn) -> do
    btn'     <- updateFormat  res btn0 -< (btn, crsr)
    lbpE     <- lbp -< input

    let
      overBtn = _rover btn'
      result  = btn' {_pressed = isEvent $ gate lbpE overBtn}
      --result  = btn' {_pressed = False}
    returnA -< result
updateButton res w = proc _ -> do returnA -< w

-- TODO : move to Widget formatting
bos = 0.05  -- Button Offset Scale
bms = 1.1   -- Button Scale  Multiplier

hsize :: BBox -> Double
hsize bb = (abs $ _bbx0 bb) + (abs $ _bbx1 bb)

updateFormat :: (Int, Int) -> Widget -> SF (Widget, Widget) Widget
updateFormat res btn0 =
  switch sf cont
  where
    sf =
      proc (btn,crsr) -> do
        (btn', moE) <- arr $ uncurry (mouseOverE res) -< (crsr, btn)
        returnA -< (btn0, moE $> btn')
    cont = updateFormat' res

updateFormat' :: (Int, Int) -> Widget -> SF (Widget, Widget) Widget
updateFormat' res btn0 =
  switch sf cont
  where
    sf =
      proc (btn,crsr) -> do
        (btn', moE) <- arr $ uncurry (mouseOverE' res) -< (crsr, btn)
        returnA -< (btn0, moE $> btn')
    cont = updateFormat res

insideBBox :: (Int, Int) -> BBox -> (Double, Double) -> (Double, Double) -> Bool
insideBBox res (BBox x0 y0 x1 y1) (bx, by) (mx, my) = inside 
  where
    (x,y) = (-0.8 + (mx/fromIntegral (snd res)), 0.5 - (my/fromIntegral (snd res))) :: (Double, Double)
    inside =
      x0+bx <= x && x <= x1+bx &&
      y0+by >= y && y >= y1+by

mouseOverE :: (Int, Int) -> Widget -> Widget -> (Widget, Event ())
mouseOverE res crsr btn = (btn', event')
  where
    Button _ _ bboxBtn _ _ fmtBtn = btn 
    Cursor _ _ (mx, my)           = crsr
    
    btn'   =
      btn
      { _format =
        fmtBtn
        { _soffset = fmtBtn ^. soffset * bms, _ssize = fmtBtn ^. ssize * bms
        , _xoffset = (\x y z -> x - y*z) (fmtBtn ^. xoffset) (hsize bboxBtn) bos }
      , _rover  = True
      }
            
    event' = if inside then Event () else NoEvent
      where
        (bx, by) = fromFormat fmtBtn
        inside   = insideBBox res bboxBtn (bx, by) (mx, my)

mouseOverE' :: (Int, Int) -> Widget -> Widget -> (Widget, Event ())
mouseOverE' res crsr btn = (btn', event')
  where
    Button _ _ bboxBtn _ _ fmtBtn = btn 
    Cursor _ _ (mx, my)           = crsr

    btn'   =
      btn
      { _format =
        fmtBtn
        { _soffset = fmtBtn ^. soffset * 1/bms, _ssize = fmtBtn ^. ssize * 1/bms
        , _xoffset = (\x y z -> x + y*z) (fmtBtn ^. xoffset) (hsize bboxBtn) bos
        }
      , _rover = False
      }
      
    event' = if not inside then Event () else NoEvent
      where
        (bx, by) = fromFormat fmtBtn
        inside   = insideBBox res bboxBtn (bx, by) (mx, my)

updateCursor :: SF (AppInput, Widget) Widget
updateCursor =
  proc (input, Cursor activeC lableC (mx, my))-> do
    (mouse', mevs) <- updateMouse -< input
    let
      coords' = mouse' ^. pos
      result' = (Cursor activeC lableC coords')
    returnA -< result'
