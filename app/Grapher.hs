{-# LANGUAGE CPP    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Main where 

import Control.Concurrent ( MVar, newMVar, swapMVar, readMVar )
import Control.Lens       ( toListOf, view, (^..), (^.))
import Control.Monad      ( when )
import Data.List.Split    as DLS (chunksOf)
import Data.Set           ( fromList, toList )
import Data.Text          ( pack )

import Data.Massiv.Array.Manifest as AM (toByteString)
import Data.Massiv.Array as A hiding (tail, windowSize, Window, mapM_, mapM, zip, fromList, toList, replicate)

import Data.Word          ( Word8 )
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

import Graphics.Rendering.OpenGL 
import Graphics.GLUtil.Textures  ( loadTexture
                                 , texInfo
                                 , texture2DWrap
                                 , TexColor(TexRGBA))
import System.Environment        ( getArgs )
import Unsafe.Coerce             ( unsafeCoerce )
    
import Graphics.RedViz.Project as P ( camMode, resy, resx, name, read )
import Graphics.RedViz.Input.FRP.Yampa.AppInput ( parseWinInput ) 
import Graphics.RedViz.Rendering as R
import Graphics.RedViz.Material as M
import qualified Graphics.RedViz.Texture  as T
import Graphics.RedViz.Drawable
import Graphics.RedViz.Widget
import Graphics.RedViz.Texture  as Texture  (uuid, name, Texture)

import Grapher.Application as Application
import Grapher.App                as App hiding (debug) 
import Grapher.Object             as O
import Grapher.ObjectTree         as OT
import Grapher.GUI
import Grapher.Graph

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

output :: MVar Double -> Window -> Application -> IO ()
output lastInteraction window application = do
-- | render FPS current
  currentTime <- SDL.time

  let
    icnObjs = concat $ toListOf (objects . icons)       app :: [Object]
    fntObjs = concat $ toListOf (objects . fonts)       app :: [Object]
    fgrObjs = concat $ toListOf (objects . foreground)  app :: [Object]
    bgrObjs = concat $ toListOf (objects . background)  app :: [Object]

    --icnsDrs = toDrawable app icnObjs currentTime :: [Drawable]
    --icnsDrs = toDrawable app (DT.trace ("DEBUG : icnObjs length : " ++ show (length icnObjs)) icnObjs) currentTime :: [Drawable]
    fntsDrs = toDrawable app fntObjs currentTime :: [Drawable]
    objsDrs = toDrawable app fgrObjs currentTime :: [Drawable]
    bgrsDrs = toDrawable app bgrObjs currentTime :: [Drawable]
    --wgts    = [] -- app ^. objects . gui . widgets -- TODO: GUI
    wgts    = fromGUI $ app ^. App.gui  :: [Widget]
    --crsr    = _cursor $ app ^. App.gui  ::  Widget

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
    renderAsTriangles = render txs hmap (opts { primitiveMode = Triangles })   :: Drawable -> IO ()
    renderAsPoints    = render txs hmap (opts { primitiveMode = Points    })   :: Drawable -> IO ()
    renderAsIcons     = render txs hmap (opts { primitiveMode = Triangles
                                              , depthMsk      = Disabled  })   :: Drawable -> IO ()
    renderWidgets     = renderWidget lastInteraction fntsDrs renderAsIcons     :: Widget   -> IO ()
    --renderCursorM     = renderCursor mouseCoords'    icnsDrs renderAsTriangles :: Widget   -> IO ()

  mapM_ renderAsTriangles objsDrs
  mapM_ renderAsPoints    bgrsDrs
  mapM_ renderWidgets     wgts
  -- renderCursorM           crsr
  
  glSwapWindow window

renderWidget :: MVar Double -> [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderWidget lastInteraction drs cmds wgt =
  case wgt of
    TextField a t f _->
      when a $ renderString cmds drs f $ concat t
    Button a l _ _ _ f _->
      when a $ renderString cmds drs f l
    FPS a f _->
      when a $ do
        ct <- SDL.time -- current time
        dt <- (ct -) <$> readMVar lastInteraction
        renderString cmds drs f $ "fps:" ++ show (round (1/dt) :: Integer)
    Cursor {} -> return ()
    _ -> return ()

renderCursor :: [Drawable] -> (Drawable -> IO ()) -> Widget-> IO ()
renderCursor drs cmds wgt =
  case wgt of
    Cursor a _ fmt _ ->
      when a $ do
      let
        f = fmt --(Format TL (x) (-y) (0.0) 0.0 1.0)
      renderIcon cmds drs f 0 --"cursor"
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

genTexObject :: Graph -> IO TextureObject
genTexObject g = do
  let
    arr'  = view array g
    arr'' = AM.toByteString arr'
    Sz2 resx' resy' = view sz g
    txInfo = texInfo resx' resy' TexRGBA arr''
  t  <- loadTexture txInfo -- :: IO TextureObject
  --uuid <- nextUUID
  texture2DWrap            $= (Repeated, ClampToEdge)
  textureFilter  Texture2D $= ((Linear', Just Nearest), Linear')
  blend                    $= Enabled
  blendFunc                $= (SrcAlpha, OneMinusSrcAlpha)
  generateMipmap' Texture2D
  --return (fromMaybe nil uuid, t)
  return t

initResources' :: Application -> [Graph] -> IO Application
initResources' app0 gs =
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
      uuids  = fmap (view Texture.uuid) txs
      hmap   = toList . fromList $ zip uuids [0..]

    putStrLn "Initializing Resources..."
    putStrLn "Loading Textures..."
    mapM_ (bindTexture hmap) txs

    mapM_ (\grph -> putStrLn $ "Generating and binding texture size : " ++ show (view sz grph)) gs
    gtxs <- mapM genTexObject gs
    let
      txs' = filter (\tx -> (head . words $ view Texture.name tx) == "Graph") txs :: [Texture]
      ids  = Prelude.read <$> concatMap (tail . words . view (Texture.name)) txs'   :: [GLuint] -- TODO: tail is unsafe, replace with Maybe
    mapM_ (uncurry bindTextureObject) (zip ids gtxs)

    putStrLn "Finished loading textures."
    return app0 { _hmap = hmap }
      where
        introObjs = concat $ toListOf (App.objects . OT.foreground)  (_intr app0)  :: [Object]
        fntObjs   = concat $ toListOf (App.objects . OT.fonts)       (_main app0)  :: [Object]
        --icnObjs   = concat $ toListOf (App.objects . OT.icons)       (_main app0)  :: [Object]
        fgrObjs   = concat $ toListOf (App.objects . OT.foreground)  (_main app0)  :: [Object]
        bgrObjs   = concat $ toListOf (App.objects . OT.background)  (_main app0)  :: [Object]

genArray :: Int -> Int -> IO (Array S Ix2 Word8)
genArray m n = do
    let lol = unsafeCoerce $ DLS.chunksOf n [ x | x <- fmap (`div`4) [0..(4*m*n-1)]] :: [[Word8]] -- lol = list of lists
    --let lol = unsafeCoerce $ [ x | x <- [0..(m*n-1)]] :: [[Word8]] -- lol = list of lists
    fromListsM Seq lol :: IO (Array S Ix2 Word8)

main :: IO ()
main = do

  args      <- getArgs
  introProj <- if debug then P.read ("./projects/test" :: FilePath)
               else          P.read (unsafeCoerce (args!!0)   :: FilePath)
  mainProj  <- if debug then P.read ("./projects/test" :: FilePath)
               else          P.read (unsafeCoerce (args!!1)   :: FilePath)
  
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
  --counter' <- newMVar 0 :: IO (MVar Int)
  putStrLn "\n Initializing GUI"

  let
    resx' = view P.resx mainProj
    resy' = view P.resy mainProj
    sz'     = Sz2 resx' resy'

  graph' <- genArray resx' resy'    
  mArr   <- newMArray sz' 1 :: IO (MArray RealWorld S Ix2 Word8)

  let
    gr  = Graph sz' graph' mArr
    res' = intrApp' ^. options . App.res
    
    initApp' =
      Application
      (intrApp' ^. App.gui)
      intrApp'
      mainApp' 
      []
      --counter'

  --app <- initResources initApp'
  app <- initResources' initApp' [gr]
  
  putStrLn "Starting App."
  animate
    window
    (parseWinInput res' >>> mainLoop app &&& handleExit)
  return ()
