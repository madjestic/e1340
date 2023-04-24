{-# LANGUAGE Arrows #-}

module GUI.Update
  ( updateGUIPre
  , updateGUI
  ) where

import Control.Lens hiding (Empty)
import Data.Functor                          (($>))
import FRP.Yampa

import Graphics.RedViz.Widget
import Graphics.RedViz.Input.Mouse
import Graphics.RedViz.Input.FRP.Yampa.Update.Mouse
import Graphics.RedViz.Input.FRP.Yampa.AppInput

import GUI.GUI
import GHC.Float (int2Double)
import Data.Maybe  

  --import Debug.Trace    as DT

updateGUIPre :: GUI -> SF AppInput GUI
updateGUIPre gui0 =
  loopPre gui0 $
  proc (input, _) -> do
    gui1 <- updateGUI gui0 -< input
    returnA -< (gui1, gui1)

updateGUI :: GUI -> SF AppInput GUI
updateGUI gui0 =
  proc input -> do
    rec gui  <- iPre gui0       -< gui'
        gui' <- updateGUI' gui0 -< (input, gui)
    returnA -< gui

updateGUI' :: GUI -> SF (AppInput, GUI) GUI
updateGUI' gui0 =
  proc (input, gui) -> do
    cursor' <- updateCursor                           -< (input, _cursor gui)
    --strtB'  <- updateButton (_res gui0) (fromJust $ _strtB gui0) -< (input, cursor', fromJust $ _strtB gui)
    strtB'  <- updateButton (_res gui0) (_strtB gui0) -< (input, cursor', _strtB gui)
    optsB'  <- updateButton (_res gui0) (_optsB gui0) -< (input, cursor', _optsB gui)
    quitB'  <- updateButton (_res gui0) (_quitB gui0) -< (input, cursor', _quitB gui)
    backB'  <- updateButton (_res gui0) (_backB gui0) -< (input, cursor', _backB gui)
    let
      result =
            gui
            { _strtB  = strtB'
            , _optsB  = optsB'
            , _quitB  = quitB'
            , _backB  = backB'
            , _cursor = cursor'
            -- --, _cursor = (DT.trace ("cursor' : " ++ show cursor')) cursor'
            }
    returnA -< result

--updateGUI' gui = proc _ -> do returnA -< gui

updateButton :: (Int, Int) -> Maybe Widget -> SF (AppInput, Maybe Widget, Maybe Widget) (Maybe Widget)
--updateButton res0 btn0@(Button {}) =
updateButton _ Nothing = proc _ -> do returnA -< Nothing
updateButton res0 btn0 =
  proc (input, crsr, btn) -> do
    btn'     <- updateFormat  res0 (fromJust btn0) -< (fromJust btn, fromJust crsr)
    lbpE     <- lbp -< input
    let
      overBtn = _rover btn'
      result  = 
        case btn' of
          Button {} -> Just btn' {_pressed = isEvent $ gate lbpE overBtn}
          _ -> Just btn'
    returnA -< result
--updateButton _ w = proc _ -> do returnA -< w

-- TODO : move to Widget formatting
bos :: Double
bos = 0.05  -- Button Offset Scale

bms :: Double
bms = 1.1   -- Button Scale  Multiplier

hsize :: BBox -> Double
hsize bb = (abs $ _bbx0 bb) + (abs $ _bbx1 bb)

updateFormat :: (Int, Int) -> Widget -> SF (Widget, Widget) Widget
updateFormat res0 btn0 =
  switch sf cont
  where
    sf =
      proc (btn,crsr) -> do
        (btn', moE) <- arr $ uncurry (mouseOverE res0) -< (crsr, btn)
        returnA -< (btn0, moE $> btn')
    cont = updateFormat' res0

updateFormat' :: (Int, Int) -> Widget -> SF (Widget, Widget) Widget
updateFormat' res0 btn0 =
  switch sf cont
  where
    sf =
      proc (btn,crsr) -> do
        (btn', moE) <- arr $ uncurry (mouseOverE' res0) -< (crsr, btn)
        returnA -< (btn0, moE $> btn')
    cont = updateFormat res0

bbox' :: Widget -> BBox
bbox' btn0@(Button {}) = newBBox (_format btn0) (btn0 ^. lable)
  where
    newBBox :: Format -> String -> BBox
    newBBox fmt str = result
      where
        offset = fromIntegral $ length str + 1
        s1     = fmt ^. soffset
        result =
          BBox
          (0.0)
          (fmt^.ssize * 0.085)
          (offset *s1 * 0.85)
          (0.0)
bbox' _ = BBox 0 0 0 0          

insideBBox :: (Int, Int) -> BBox -> (Double, Double) -> (Double, Double) -> Bool
insideBBox res0 (BBox x0 y0 x1 y1) (bx, by) (mx, my) = inside'
  where
    (x,y) = (-0.8 + (mx/fromIntegral (snd res0)), 0.5 - (my/fromIntegral (snd res0))) :: (Double, Double)
    inside' =
      x0+bx <= x && x <= x1+bx &&
      y0+by >= y && y >= y1+by

mouseOverE :: (Int, Int) -> Widget -> Widget -> (Widget, Event ())
mouseOverE
  res0
  (Cursor _ _ fmt _)
  btn@(Button _ _ _ _ _ fmtBtn _)
  = (btn', event')
  where
    mx     = fmt ^. xoffset
    my     = fmt ^. yoffset
    btn'   =
      btn
      { _format =
        fmtBtn
        { _soffset = fmtBtn ^. soffset * bms, _ssize = fmtBtn ^. ssize * bms
        , _xoffset = (\x y z -> x - y*z) (fmtBtn ^. xoffset) (hsize $ bbox' btn) bos }
      , _rover  = True
      }
            
    event' = if inside' then Event () else NoEvent
      where
        (bx, by) = fromFormat fmtBtn
        inside'  = insideBBox res0 (bbox' btn) (bx, by) (mx, my)
mouseOverE _ _ _ = (Empty, NoEvent)    

mouseOverE' :: (Int, Int) -> Widget -> Widget -> (Widget, Event ())
mouseOverE' res0
  (Cursor _ _ fmt _ )
  btn@(Button  _ _ _ _ _ fmtBtn _)
  = (btn', event')
  where
    mx     = fmt ^. xoffset
    my     = fmt ^. yoffset
    btn'   =
      btn
      { _format =
        fmtBtn
        { _soffset = fmtBtn ^. soffset * 1/bms, _ssize = fmtBtn ^. ssize * 1/bms
        , _xoffset = (\x y z -> x + y*z) (fmtBtn ^. xoffset) (hsize $ bbox' btn) bos
        }
      , _rover = False
      }
      
    event' = if not inside' then Event () else NoEvent
      where
        (bx, by) = fromFormat fmtBtn
        inside'  = insideBBox res0 (bbox' btn) (bx, by) (mx, my)
mouseOverE' _ _ _ = (Empty, NoEvent)        

updateCursor :: SF (AppInput, Maybe Widget) (Maybe Widget)
updateCursor =
  proc (input, cursor)-> do
    case cursor of
      Just (Cursor activeC lableC fmt opts) -> do
        --(mouse', mevs) <- updateMouse -< input
        (mouse', _) <- updateMouse -< input
        let
          mousePos = mouse' ^. pos :: (Int, Int)
          coords'  = bimap int2Double int2Double mousePos
          result'  = Just $
            Cursor
            {
              _active  = activeC
            , _lable   = lableC
            , _format  = fmt
                         & xoffset .~ (int2Double $ mousePos^._1)
                         & yoffset .~ (int2Double $ mousePos^._2)
            , _options = opts}
        returnA -< result'
      Nothing -> do
        returnA -< Nothing
