{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

import Control.Concurrent ( MVar, newMVar, swapMVar, readMVar, forkIO)
import Control.Lens       ( toListOf, view, (^..), (^.), bimap)
import Control.Monad      ( when )
import Data.Set           ( fromList, toList )
import Data.Text          ( pack, unpack)
import Foreign.C          ( CInt)
import FRP.Yampa as FRP   ( (>>>), reactimate, Arrow((&&&)), Event(..), SF )
import SDL
    ( pollEvent
    , time
    , glSwapWindow
    , Event(eventPayload)
    , EventPayload
    , Window
    , delay)
import SDL.Input.Mouse
import SDL.Vect

import Graphics.Rendering.OpenGL ( PrimitiveMode(..)
                                 , Color4 (Color4)
                                 , clear
                                 , clearColor
                                 , ($=)
                                 , ClearBuffer (..)
                                 , Capability (..))
import System.Environment        ( getArgs )
import Unsafe.Coerce             ( unsafeCoerce )
import System.IO.Unsafe
    
import Graphics.RedViz.Project as P ( camMode, resy, resx, name, read )
import Graphics.RedViz.Input.FRP.Yampa.AppInput ( parseWinInput ) 
import Graphics.RedViz.Rendering as R hiding (renderIcons)
import Graphics.RedViz.Material as M
import qualified Graphics.RedViz.Texture  as T
import Graphics.RedViz.Drawable hiding (toDrawables)
import Graphics.RedViz.Texture
import Graphics.RedViz.Widget as W
import Graphics.RedViz.Object

import Application as A
import App hiding (debug)
import Object             as O
import ObjectTree         as OT
import GUI
import GHC.Float (int2Double)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Coerce (coerce)

import Debug.Trace    as DT
import Data.Aeson (object)

debug :: Bool
#ifdef DEBUGMAIN
debug = True
#else
debug = False
#endif

-- -- < Animate > ------------------------------------------------------------
type WinInput = FRP.Event SDL.EventPayload
type WinOutput = (Application, Bool)

animate :: SDL.Window
        -> SF WinInput WinOutput  -- ^ signal function to animate
        -> IO ()
animate window sf =
  do
    fps <- newMVar (replicate 500 0.002) :: IO (MVar [Double])
    reactimate (return NoEvent)
               senseInput
               (renderOutput fps)
               sf
    closeWindow window
    
      where
        senseInput _ =
          do
            lastInteraction <- newMVar =<< SDL.time
            ct     <- SDL.time                          
            dt <- (ct -) <$> swapMVar lastInteraction ct --dtime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent)
            
        renderOutput fps _ (appl, shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time

            output fps lastInteraction window appl

            return (shouldExit || (appl ^. quit))

output :: MVar [Double] -> MVar Double -> Window -> Application -> IO ()
output fps lastInteraction window application = do
-- | render FPS current
  --_  <- SDL.delay 100 -- shader preview hack to reduce memory leak on load
  ct <- SDL.time

  let
    icnObjs = concat $ toListOf (objects . icons)       app :: [Object]
    fntObjs = concat $ toListOf (objects . fonts)       app :: [Object]
    fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
    bgrObjs = concat $ toListOf (objects . background)  app :: [Object]
    icnsDrs = concatMap (toDrawables app ct) icnObjs :: [Drawable]
    fntsDrs = concatMap (toDrawables app ct) fntObjs :: [Drawable]
    objsDrs = concatMap (toDrawables app ct) fgrObjs :: [Drawable]
    bgrsDrs = concatMap (toDrawables app ct) bgrObjs :: [Drawable]
    wgts    = fromGUI $ app ^. App.gui  :: [Widget]
    app     = fromApplication application
    crsr    = _cursor $ app ^. App.gui  :: Maybe Widget
    gizmo   = _gizmo  $ app ^. App.gui  :: Maybe Widget
    icns    = maybeToList crsr ++ maybeToList gizmo :: [Widget]

  curvObj <- unsafeInterleaveIO $ toCurve fgrObjs; let curvObjs = [curvObj]
  let
    curvDrs = concatMap (toDrawables app ct) curvObjs :: [Drawable]
    txs  = concat 
           (concatMap
             (\obj -> obj ^.. base . materials . traverse . textures)
             (fgrObjs ++ fntObjs ++ icnObjs)
           ) :: [Texture]
    hmap = _hmap application

  clearColor $= Color4 0.0 0.0 0.0 1.0
  clear [ColorBuffer, DepthBuffer]

  dts <- readMVar fps
  ct  <- SDL.time -- current time
  dt  <- (ct -) <$> readMVar lastInteraction :: IO Double
  dts'<- swapMVar fps $ tail dts ++ [dt]

  let
    dt            = sum dts'/fromIntegral (length dts')
    render'       = render txs hmap                         :: Drawable -> IO ()
    renderWidgets = renderWidget dt fntsDrs icnsDrs render' :: Widget   -> IO ()

  mapM_ render' $ objsDrs ++ bgrsDrs
  mapM_ renderWidgets icns
  mapM_ renderWidgets wgts
  
  glSwapWindow window

-- < Main Function > -----------------------------------------------------------

initResources :: Application -> IO Application
initResources app0 =
  do
    let
      objs   = fntObjs ++ icnObjs ++ fgrObjs ++ bgrObjs
      txs    = concat $ objs ^.. traverse . base . materials . traverse . textures
      uuids  = fmap (view T.uuid) txs
      hmap   = toList . fromList $ zip uuids [0..]

    putStrLn "Initializing Resources..."
    putStrLn "Loading Textures..."
    mapM_ (bindTexture hmap) txs
    putStrLn "Finished loading textures."

    return app0 { _hmap = hmap }
      where
        fntObjs   = concat $ toListOf (App.objects . OT.fonts)       (_main app0)  :: [Object]
        icnObjs   = concat $ toListOf (App.objects . OT.icons)       (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . OT.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . OT.background)  (_main app0)  :: [Object]

main :: IO ()
main = do

  args      <- getArgs
  initPreApp  <- if debug then A.read ("./applications/solarsystem" :: FilePath)
                 else          A.read (unsafeCoerce (head args)     :: FilePath)
  
  let
    title   = pack $ view A.pname initPreApp
    resX    = (unsafeCoerce $ view A.resx initPreApp) :: CInt
    resY    = (unsafeCoerce $ view A.resy initPreApp) :: CInt

  window    <- openWindow
               title
               (resX, resY)

  -- | SDL Mouse Options
  _ <- setMouseLocationMode AbsoluteLocation --RelativeLocation
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX`div`2) (resY`div`2)))
  _ <- cursorVisible $= False

  putStrLn "\n Initializing Application"

  --initPreApp      <- A.read "./applications/solarsystem"
  initApplication <- fromPreApplication initPreApp
  app             <- initResources initApplication
  let res'        =  unsafeCoerce (resX, resY) :: (Int, Int)
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput res' >>> mainLoop app &&& handleExit)
  return ()
