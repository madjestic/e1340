{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

import Control.Concurrent ( MVar, newMVar, swapMVar, readMVar )
import Control.Lens       ( toListOf, view, (^..), (^.), Bifunctor (bimap))
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
import Graphics.Rendering.OpenGL ( PrimitiveMode(..)
                                 , Color4 (Color4)
                                 , clear
                                 , clearColor
                                 , ($=)
                                 , ClearBuffer (..)
                                 , Capability (..))
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

import Application as A
import App hiding (debug)
import Object             as O
import ObjectTree         as OT
import GUI
import GHC.Float (int2Double)

--import Debug.Trace    as DT

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

            return (shouldExit || (app ^. quit))

output :: MVar Double -> Window -> Application -> IO ()
output lastInteraction window application = do
-- | render FPS current
  currentTime <- SDL.time
  let
    icnObjs = concat $ toListOf (objects . icons)       app :: [Object]
    fntObjs = concat $ toListOf (objects . fonts)       app :: [Object]
    fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
    bgrObjs = concat $ toListOf (objects . background)  app :: [Object]

    icnsDrs = toDrawable app icnObjs currentTime :: [Drawable]
    fntsDrs = toDrawable app fntObjs currentTime :: [Drawable]
    objsDrs = toDrawable app fgrObjs currentTime :: [Drawable]
    bgrsDrs = toDrawable app bgrObjs currentTime :: [Drawable]
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
    mouseCoords = case (app ^. App.gui . cursor) of
      crs'@(Cursor {}) -> _coords crs'
      _ -> (0,0)

    (_, resy')        = app ^. options . App.res
    mouseCoords'      = (\ (x,y)(x',y') -> (x/x', y/y')) mouseCoords (fromIntegral resy',fromIntegral resy')
 
    renderAsTriangles = render txs hmap (opts { primitiveMode = Triangles })   :: Drawable -> IO ()
    renderAsPoints    = render txs hmap (opts { primitiveMode = Points    })   :: Drawable -> IO ()
    renderAsIcons     = render txs hmap (opts { primitiveMode = Triangles
                                              , depthMsk      = Disabled  })   :: Drawable -> IO ()
    renderWidgets     = renderWidget lastInteraction fntsDrs renderAsIcons     :: Widget   -> IO ()
    renderCursorM     = renderCursor mouseCoords'    icnsDrs renderAsTriangles :: Widget   -> IO ()

  mapM_ renderAsTriangles objsDrs
  mapM_ renderAsPoints    bgrsDrs
  mapM_ renderWidgets     wgts
  renderCursorM           crsr
  
  glSwapWindow window

renderWidget :: MVar Double -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderWidget lastInteraction drs cmds wgt =
  case wgt of
    TextField a t f _ ->
      when a $ renderString cmds drs f $ concat t
    Button a l _ _ _ f _->
      when a $ renderString cmds drs f l
    FPS a f _ ->
      when a $ do
        ct <- SDL.time -- current time
        dt <- (ct -) <$> readMVar lastInteraction
        renderString cmds drs f $ "fps:" ++ show (round (1/dt) :: Integer)
    Cursor {} -> return ()
    _ -> return ()

renderCursor :: (Double, Double) -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderCursor (x,y) drs cmds wgt =
  case wgt of
    Cursor a _ _ _ ->
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
      objs   = introObjs ++ fntObjs' ++ icnObjs' ++ fgrObjs ++ bgrObjs-- ++ testObjs
      txs    = concat $ objs ^.. traverse . base . materials . traverse . textures
      uuids  = fmap (view T.uuid) txs
      hmap   = toList . fromList $ zip uuids [0..]

    putStrLn "Initializing Resources..."
    putStrLn "Loading Textures..."
    mapM_ (bindTexture hmap) txs
    putStrLn "Finished loading textures."

    return app0 { _hmap = hmap }
      where
        introObjs = concat $ toListOf (App.objects . OT.foreground)  (_intr app0)  :: [Object]
        fntObjs   = concat $ toListOf (App.objects . OT.fonts)       (_main app0)  :: [Object]
        --icnObjs   = concat $ toListOf (App.objects . OT.icons)       (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . OT.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . OT.background)  (_main app0)  :: [Object]

main :: IO ()
main = do

  args      <- getArgs
  introProj <- P.read (unsafeCoerce (args!!0)   :: FilePath)
  mainProj  <- P.read (unsafeCoerce (args!!1)   :: FilePath)
  optsProj  <- P.read (unsafeCoerce (args!!2)   :: FilePath)
  pInfoProj <- P.read (unsafeCoerce (args!!3)   :: FilePath)
  
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

  putStrLn "\n Initializing Apps"
  intrApp' <- intrApp introProj
  mainApp' <- mainApp mainProj
  optsApp' <- optsApp optsProj
  infoApp' <- mainApp pInfoProj
  counter' <- newMVar 0 :: IO (MVar Int)

  putStrLn "\n Initializing GUI"

  let
    res' = mainApp' ^. options . App.res
    initApp' =
      Application
      {
        A._gui  = (intrApp' ^. App.gui)
      , A._intr = intrApp'
      , A._main = mainApp'
      , A._opts = optsApp'
      , A._info = infoApp'
      , A._counter = counter'
      , A._hmap = []
      , A._quit = False
      }

  app <- initResources initApp'
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput res' >>> mainLoop app &&& handleExit)
  return ()
