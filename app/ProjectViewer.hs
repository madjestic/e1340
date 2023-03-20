{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

import Control.Concurrent ( MVar, newMVar, swapMVar, readMVar, forkIO)
import Control.Lens       ( toListOf, view, (^..), (^.), bimap)
import Control.Monad      ( when )
import Data.Set           ( fromList, toList )
import Data.Text          ( pack)
import Foreign.C          ( CInt )
import FRP.Yampa as FRP   ( (>>>), reactimate, Arrow((&&&)), Event(..), SF )
import SDL
    ( pollEvent
    , time
    , glSwapWindow
    , Event(eventPayload)
    , EventPayload
    , Window )
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
import Graphics.RedViz.Rendering as R
import Graphics.RedViz.Material as M
import qualified Graphics.RedViz.Texture  as T
import Graphics.RedViz.Drawable
import Graphics.RedViz.Texture
import Graphics.RedViz.Widget as W

import Application as A
import App hiding (debug)
import Object             as O
import ObjectTree         as OT
import GUI
import GHC.Float (int2Double)

--import Debug.Trace    as DT

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
            currentTime     <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime --dtime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent)
            
        renderOutput fps _ (app, shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time

            output fps lastInteraction window app

            return (shouldExit || (app ^. quit))

output :: MVar [Double] -> MVar Double -> Window -> Application -> IO ()
output fps lastInteraction window application = do
-- | render FPS current
  currentTime <- SDL.time

  let
    icnObjs = concat $ toListOf (objects . icons)       app :: [Object]
    fntObjs = concat $ toListOf (objects . fonts)       app :: [Object]
    fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
    bgrObjs = concat $ toListOf (objects . background)  app :: [Object]

    icnsDrs = toDrawable app icnObjs currentTime :: [Drawable]
    --icnsDrs = toDrawable app (DT.trace ("DEBUG : icnObjs length : " ++ show (length icnObjs)) icnObjs) currentTime :: [Drawable]
    fntsDrs = toDrawable app fntObjs currentTime :: [Drawable]
    objsDrs = toDrawable app fgrObjs currentTime :: [Drawable]
    bgrsDrs = toDrawable app bgrObjs currentTime :: [Drawable]
    wgts    = fromGUI $ app ^. App.gui  :: [Widget]
    crsr    = _cursor $ app ^. App.gui  ::  Widget
    gizmo   = _gizmo  $ app ^. App.gui  ::  Widget
    icns    = [crsr, gizmo]
    app  = fromApplication application

  curvObj <- unsafeInterleaveIO $ toCurve fgrObjs; let curvObjs = [curvObj]
  let
    curvDrs = toDrawable app curvObjs currentTime :: [Drawable]
    txs  = concat 
           (concatMap
             (\obj -> obj ^.. base . materials . traverse . textures)
             (fgrObjs ++ fntObjs ++ icnObjs)
           ) :: [Texture]
    hmap = _hmap application

    opts =
      BackendOptions
      { primitiveMode = Triangles
      , bgrColor      = Color4 0.0 0.0 0.0 1.0
      , ptSize        = 1.0
      , depthMsk      = Enabled
      }
    
  clearColor $= bgrColor opts
  clear [ColorBuffer, DepthBuffer]

  let
    mouseCoords = case (app ^. App.gui . cursor) of
      crs'@(Cursor {}) -> _coords crs'
      _ -> (0,0)

    (_, resy)  = app ^. options . App.res
    mouseCoords' = (\ (x,y) resy -> (x/resy, y/resy)) mouseCoords (fromIntegral resy)
 
    renderAsTriangles = render txs hmap (opts { primitiveMode = Triangles })   :: Drawable -> IO ()
    renderAsPoints    = render txs hmap (opts { primitiveMode = Points    })   :: Drawable -> IO ()
    renderAsIcons     = render txs hmap (opts { primitiveMode = Triangles       
                                              , depthMsk      = Disabled  })   :: Drawable -> IO ()
    renderWidgets     = renderWidget fps lastInteraction fntsDrs renderAsIcons :: Widget   -> IO ()
    renderCursorM     = renderCursor mouseCoords' icnsDrs renderAsTriangles    :: Widget   -> IO ()
    renderIconsM      = renderIcons' mouseCoords' icnsDrs renderAsTriangles    :: Widget   -> IO ()
    
    renderAsCurves    = render txs hmap (opts { primitiveMode = LineStrip })   :: Drawable -> IO ()

  mapM_ renderAsCurves    curvDrs
  mapM_ renderAsTriangles objsDrs
  mapM_ renderAsPoints    bgrsDrs
  mapM_ renderWidgets     wgts
  --renderCursorM           crsr
  mapM_ renderIconsM      icns
  
  glSwapWindow window

renderWidget :: MVar [Double] -> MVar Double -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderWidget fps lastInteraction drs cmds wgt =
  case wgt of
    TextField a t f _ ->
      when a $ renderString cmds drs f $ concat t
    Button a l _ _ _ f _ ->
      when a $ renderString cmds drs f l
    FPS a f _ ->
      when a $ do
        dts <- readMVar fps
        ct  <- SDL.time -- current time
        dt  <- (ct -) <$> readMVar lastInteraction :: IO Double
        dts'<- swapMVar fps $ tail dts ++ [dt]
        let dt' = (sum dts')/(fromIntegral $ length dts')
        renderString cmds drs f $ "fps:" ++ show (round (1.0/dt') :: Integer)
            
    Cursor {} -> return ()
    _ -> return ()

renderCursor :: (Double, Double) -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderCursor (x,y) drs cmds wgt =
  case wgt of
    Cursor a _ _ _ ->
      when a $ do
      let
        f = (Format TL (x) (-y) (0.0) 0.0 0.2)
      renderIcon cmds drs f 0 --"cursor"
    _ -> return ()

renderIcons' :: (Double, Double) -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderIcons' (x,y) drs cmds wgt =
  case wgt of
    Cursor a _ _ _ ->
      when a $ do
      let
        f = (Format TL (x) (-y) (0.0) 0.0 0.2)
      renderIcon cmds drs f 0 --"cursor"
    Icon a _ idx ->
      when a $ do
      let
        f = (Format TL (x) (-y) (0.0) 0.0 0.2)
      renderIcon cmds drs f idx --"icon"
    _ -> return ()

-- < Main Function > -----------------------------------------------------------

initResources :: Application -> IO Application
initResources app0 =
  do
    let
      fntObjs' = case fntObjs of
        [] -> []
        _  -> [head fntObjs]
      icnObjs' = case fntObjs of
        [] -> []
        _  -> [head fntObjs]
      objs   = fntObjs' ++ icnObjs' ++ fgrObjs ++ bgrObjs-- ++ testObjs
      txs    = concat $ objs ^.. traverse . base . materials . traverse . textures
      uuids  = fmap (view T.uuid) txs
      hmap   = toList . fromList $ zip uuids [0..]

    putStrLn "Initializing Resources..."
    putStrLn "Loading Textures..."
    mapM_ (bindTexture hmap) txs
    putStrLn "Finished loading textures."

    return app0 { _hmap = hmap }
      where
        --introObjs = concat $ toListOf (App.objects . OT.foreground)  (_intr app0)  :: [Object]
        fntObjs   = concat $ toListOf (App.objects . OT.fonts)       (_main app0)  :: [Object]
        --icnObjs   = concat $ toListOf (App.objects . OT.icons)       (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . OT.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . OT.background)  (_main app0)  :: [Object]

main :: IO ()
main = do

  args      <- getArgs
  --mainProj  <- if debug then P.read ("./projects/planetsputnik" :: FilePath)
  mainProj  <- if debug then P.read ("./projects/solarsystem" :: FilePath)
               else          P.read (unsafeCoerce (args!!0)   :: FilePath)
  
  let
    title   = pack $ view P.name mainProj
    resX    = (unsafeCoerce $ view P.resx mainProj) :: CInt
    resY    = (unsafeCoerce $ view P.resy mainProj) :: CInt

  window    <- openWindow
               title
               (resX, resY)

  -- | SDL Mouse Options
  let camMode' =
        case view P.camMode mainProj of
          "RelativeLocation" -> RelativeLocation
          "AbsoluteLocation" -> AbsoluteLocation
          _ -> error "wrong mouse mode"

  _ <- setMouseLocationMode camMode'
  _ <- warpMouse (WarpInWindow window) (P (V2 (resX`div`2) (resY`div`2)))

  putStrLn "\n Initializing Apps"
  mainApp <- mainApp mainProj
  --counter <- newMVar 0 :: IO (MVar Int)

  putStrLn "\n Initializing GUI"

  let
    res' = mainApp ^. options . App.res
    initApplication =
      Application
      {
        A._gui  = mainApp ^. App.gui
      , A._intr = mainApp
      , A._opts = mainApp
      , A._info = mainApp
      , A._main = mainApp
      , A._hmap = []
      --, A._counter = counter
      , A._quit = False
      }

  app <- initResources initApplication
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput res' >>> mainLoop app &&& handleExit)
  return ()
