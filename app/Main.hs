{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

--import Control.Concurrent ( swapMVar, newMVar, readMVar, MVar, putMVar, takeMVar )
import Control.Concurrent ( MVar, newMVar, swapMVar, readMVar )
import Control.Lens       ( toListOf, view, (^..), (^.), (&), (.~) )
import Control.Monad      (when)
import Data.Set           ( fromList, toList )
import Data.Text          ( pack)
import Foreign.C          ( CInt )
import FRP.Yampa as FRP   ( (>>>), reactimate, Arrow((&&&)), Event(..), SF )
import SDL
    ( pollEvent,
      setMouseLocationMode,
      time,
      glSwapWindow,
      Event(eventPayload),
      EventPayload,
      LocationMode(AbsoluteLocation, RelativeLocation),
      Window )
import Graphics.Rendering.OpenGL ( PrimitiveMode(..), Color4 (Color4), clear, clearColor, ($=), ClearBuffer (..))
import System.Environment        ( getArgs )
import Unsafe.Coerce             ( unsafeCoerce )
    
import Graphics.RedViz.Project as P ( camMode, resy, resx, name, read )
import Graphics.RedViz.Input.FRP.Yampa.AppInput ( parseWinInput ) 
import Graphics.RedViz.Rendering as R
import Graphics.RedViz.Material as M
import qualified Graphics.RedViz.Texture  as T
import Graphics.RedViz.Drawable
import Graphics.RedViz.Texture
import Graphics.RedViz.Widget

import Application
import App hiding (debug)
import Object             as O
import ObjectTree         as OT

-- import Debug.Trace    as DT

debug :: Bool
#ifdef DEBUG
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
    reactimate (return NoEvent)
               senseInput
               renderOutput
               sf
    closeWindow window
    
      where
        senseInput _ =
          do
            lastInteraction <- newMVar =<< SDL.time
            currentTime <- SDL.time                          
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime --dtime
            mEvent <- SDL.pollEvent
            
            return (dt, Event . SDL.eventPayload <$> mEvent)
            
        renderOutput _ (app, shouldExit) =
          do
            lastInteraction <- newMVar =<< SDL.time

            output lastInteraction window app
            return shouldExit

-- App -> GUI -> Drawable

output :: MVar Double -> Window -> Application -> IO ()
output lastInteraction window application = do
  -- ticks   <- SDL.ticks
  -- let currentTime = fromInteger (unsafeCoerce ticks :: Integer) :: Float

-- | render FPS current
  currentTime <- SDL.time
  -- dt <- (currentTime -) <$> readMVar lastInteraction

  let
    fntObjs = concat $ toListOf (objects . gui . fonts) app :: [Object]
    fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
    bgrObjs = concat $ toListOf (objects . background)  app :: [Object]

    fntsDrs = toDrawable app fntObjs currentTime :: [Drawable]
    objsDrs = toDrawable app fgrObjs currentTime :: [Drawable]
    bgrsDrs = toDrawable app bgrObjs currentTime :: [Drawable]
    wgts    = app ^. objects . gui . widgets

    app  = fromApplication application
    txs  = concat . concat
           $   (\obj -> obj ^.. base . materials . traverse . textures)
           <$> (fgrObjs ++ fntObjs) :: [Texture]
    hmap = _hmap application

    opts =
      BackendOptions
      { primitiveMode = Triangles
      , bgrColor      = Color4 0.0 0.0 0.0 1.0
      , ptSize        = 1.0 }
    
  clearColor $= bgrColor opts
  clear [ColorBuffer, DepthBuffer]

  let renderAsTriangles = render txs hmap (opts { primitiveMode = Triangles }) :: Drawable -> IO ()
      renderAsPoints    = render txs hmap (opts { primitiveMode = Points })    :: Drawable -> IO ()
      renderWidgets     = renderWidget lastInteraction fntsDrs renderAsTriangles

  mapM_ renderAsTriangles objsDrs
  mapM_ renderAsPoints    bgrsDrs
  case app ^. objects . gui . fonts of
    [] -> return ()
    _  -> mapM_ renderWidgets     wgts

  glSwapWindow window

renderWidget :: MVar Double -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderWidget lastInteraction drs cmds wgt =
  case wgt of
    TextField a t f->
      when a $ renderString cmds drs f $ concat t
    FPS a f ->
      when a $ do
        ct <- SDL.time -- current time
        dt <- (ct -) <$> readMVar lastInteraction
        renderString cmds drs f $ "fps:" ++ show (round (1/dt) :: Integer)

-- < Main Function > -----------------------------------------------------------

initResources :: Application -> IO Application
initResources app0 =
  do
    let
      fntObjs' = case fntObjs of
        [] -> []
        _  -> [head fntObjs]
      objs   = introObjs ++ fntObjs' ++ fgrObjs ++ bgrObjs-- ++ testObjs
      txs    = concat $ objs ^.. traverse . base . materials . traverse . textures
      uuids  = fmap (view T.uuid) txs
      hmap   = toList . fromList $ zip uuids [0..]

    putStrLn "Initializing Resources..."
    putStrLn "Loading Textures..."
    mapM_ (bindTexture hmap) txs
    putStrLn "Finished loading textures."

    return app0 { _hmap = hmap }
      where
        introObjs = concat $ toListOf (App.objects . OT.foreground)  (_intro app0) :: [Object]
        fntObjs   = concat $ toListOf (App.objects . gui . OT.fonts) (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . OT.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . OT.background)  (_main app0)  :: [Object]

main :: IO ()
main = do


  --let argsDebug = return ["./projects/intro_XXII", "./projects/solar_system"]
  let argsDebug = return ["./projects/solarsystem", "./projects/solarsystem"]
  args <- if debug then argsDebug else getArgs

  introProj <- P.read (unsafeCoerce (args!!0) :: FilePath)
  mainProj  <- P.read (unsafeCoerce (args!!1) :: FilePath)
  pInfoProj <- P.read ("./projects/testred" :: FilePath)
  --pInfoProj <- P.read ("./projects/newtest" :: FilePath)
  
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

  putStrLn "\n Initializing App"
  introApp <- App.fromProject introProj
  mainApp  <- App.fromProject mainProj
  info'    <- App.fromProject pInfoProj
  counter' <- newMVar 0 :: IO (MVar Int)

  putStrLn "\n Initializing GUI"
  let mainAppUI
        = MainGUI
          { _fps      = FPS True (Format TC (-0.4) 0.0 0.085 1.0)
          , App._info = TextField True [""] (Format CC (-0.4) 0.0 0.085 1.0)}

  let
    initApp' =
      Application
      Intro
      introApp
      (mainApp & ui .~ mainAppUI)
      info'
      []
      counter'

  app <- initResources initApp'
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput >>> appLoop app &&& handleExit)
  return ()
