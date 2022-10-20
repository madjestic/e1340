{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

--import Control.Concurrent ( swapMVar, newMVar, readMVar, MVar, putMVar, takeMVar )
import Control.Concurrent ( MVar, newMVar, swapMVar, readMVar, putMVar )
import Control.Lens       ( toListOf, view, (^..), (^.), (&), (.~) )
import Control.Monad      ( when )
import Data.Set           ( fromList, toList )
import Data.Text          ( pack)
import Foreign.C          ( CInt )
import FRP.Yampa as FRP   ( (>>>), (<<<), reactimate, Arrow((&&&)), Event(..), SF )
import SDL
    ( pollEvent
    , setMouseLocationMode
    , time
    , glSwapWindow
    , Event(eventPayload)
    , EventPayload
    , LocationMode(AbsoluteLocation, RelativeLocation)
    , Window
    , delay
    )
import SDL.Input.Mouse
import SDL.Vect

import Graphics.Rendering.OpenGL ( PrimitiveMode(..)
                                 , Color4 (Color4)
                                 , clear
                                 , clearColor
                                 , ($=)
                                 , ClearBuffer (..)
                                 , DataType (Double)
                                 , Capability (..))
import System.Environment        ( getArgs )
import Unsafe.Coerce             ( unsafeCoerce )
--import GHC.Float                 ( roundDouble )
    
import Graphics.RedViz.Project as P ( camMode, resy, resx, name, read )
import Graphics.RedViz.Input.FRP.Yampa.AppInput ( parseWinInput ) 
import Graphics.RedViz.Rendering as R
import Graphics.RedViz.Material as M
import qualified Graphics.RedViz.Texture  as T
import Graphics.RedViz.Drawable
import Graphics.RedViz.Texture
import Graphics.RedViz.Widget
import Graphics.RedViz.Camera
import Graphics.RedViz.Controllable
import Graphics.RedViz.Input.Mouse

import Application as A
import Application.Interface
import App hiding (debug)
import Object             as O
import ObjectTree         as OT
import GUI

import Debug.Trace    as DT

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
  -- ticks   <- SDL.ticks
  -- let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float

-- | render FPS current
  currentTime <- SDL.time
  --mmloc  <- SDL.Input.Mouse.getModalMouseLocation
  -- mloc -- TODO get mouse pos from AppInput, store it in App.gui?

  --dt <- (currentTime -) <$> readMVar lastInteraction

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
    --wgts    = [] -- app ^. objects . gui . widgets -- TODO: GUI
    wgts    = fromGUI $ app ^. App.gui  :: [Widget]
    crsr    = _cursor $ app ^. App.gui  ::  Widget

    app  = fromApplication application
    txs  = concat . concat
           $   (\obj -> obj ^.. base . materials . traverse . textures)
           <$> (fgrObjs ++ fntObjs ++ icnObjs) :: [Texture]
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
    playCam'    = app ^. playCam :: Camera
    --mouseCoords = app ^. playCam . controller . device . mouse . pos :: (Double, Double)
    mouseCoords = case (app ^. App.gui . cursor) of
      crs'@(Cursor {}) -> _coords crs'
      _ -> (0,0)

    (resx', resy')  = app ^. options . App.res
    mouseCoords' = (\ (x,y)(x',y') -> (x/x', y/y')) mouseCoords (fromIntegral resy',fromIntegral resy')
    --mouseCoords' = (\ (x,y)(x',y') -> (x/x', y/y')) (DT.trace ("DEBUG :: mouseCoords : " ++ show mouseCoords) mouseCoords) (resy'/1,resy'/1)
 
    renderAsTriangles = render txs hmap (opts { primitiveMode = Triangles })   :: Drawable -> IO ()
    renderAsPoints    = render txs hmap (opts { primitiveMode = Points    })   :: Drawable -> IO ()
    renderAsIcons     = render txs hmap (opts { primitiveMode = Triangles
                                              , depthMsk      = Disabled  })   :: Drawable -> IO ()
    renderWidgets     = renderWidget fps lastInteraction fntsDrs renderAsIcons     :: Widget   -> IO ()
    renderCursorM     = renderCursor mouseCoords'    icnsDrs renderAsTriangles :: Widget   -> IO ()

  mapM_ renderAsTriangles objsDrs
  mapM_ renderAsPoints    bgrsDrs
  mapM_ renderWidgets     wgts
  renderCursorM           crsr
  
  -- case app ^. objects . gui . fonts of
  --   [] -> return ()
  --   _  -> mapM_ renderWidgets wgts

  glSwapWindow window

renderWidget :: MVar [Double] -> MVar Double -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderWidget fps lastInteraction drs cmds wgt =
  case wgt of
    TextField a t f ->
      when a $ renderString cmds drs f $ concat t
    Button a l _ _ _ f->
      when a $ renderString cmds drs f l
    FPS a f ->
      when a $ do
        dts <- readMVar fps
        ct  <- SDL.time -- current time
        dt  <- (ct -) <$> readMVar lastInteraction :: IO Double
        dts'<- swapMVar fps $ tail dts ++ [dt]
        let dt' = (sum dts')/(fromIntegral $ length dts')
        renderString cmds drs f $ "fps:" ++ show (round (1.0/dt') :: Integer)
          where
            fps' = 30   :: Double
            msps = 1000 :: Double
            
    Cursor {} -> return ()

renderCursor :: (Double, Double) -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderCursor (x,y) drs cmds wgt =
  case wgt of
    Cursor a l (x',y') ->
      when a $ do
      let
        f = (Format TL (x) (-y) (0.0) 0.0 1.0)
      renderIcon cmds drs f "cursor"
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
        icnObjs   = concat $ toListOf (App.objects . OT.icons)       (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . OT.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . OT.background)  (_main app0)  :: [Object]

main :: IO ()
main = do

  args      <- getArgs
  --mainProj  <- if debug then P.read ("./projects/planetsputnik" :: FilePath)
  mainProj  <- if debug then P.read ("./projects/test" :: FilePath)
               else          P.read (unsafeCoerce (args!!0)     :: FilePath)  
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
  -- _ <- warpMouse WarpGlobal (P (V2 (resX`div`2) (resY`div`2)))

  putStrLn "\n Initializing Apps"
  mainApp' <- mainApp mainProj
  counter' <- newMVar 0 :: IO (MVar Int)

  putStrLn "\n Initializing GUI"

  let
    res = mainApp' ^. options . App.res
    initApp' =
      Application
      {
        A._gui  = mainApp' ^. App.gui
      , A._main = mainApp'
      , A._counter = counter'
      , A._quit = False
      }

  app <- initResources initApp'
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput res >>> mainLoop app &&& handleExit)
  return ()
