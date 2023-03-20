{-# language RecordWildCards #-}
{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language DeriveTraversable #-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad (when)
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL3
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import DearImGui.FontAtlas as FontAtlas
import Foreign.Ptr           (Ptr, nullPtr, plusPtr)
import Foreign.Marshal.Array (withArray)
--import Graphics.GL
import Graphics.Rendering.OpenGL as GL
import Foreign.Storable      (sizeOf)
import SDL

import Graphics.RedViz.LoadShaders
import qualified DearImGui.FontAtlas as FontAtlas
import Control.Arrow (ArrowLoop(loop))
import Data.IORef (newIORef, writeIORef, readIORef)

data Descriptor =
     Descriptor VertexArrayObject NumArrayIndices

indices :: [GLuint]
indices =
  [          -- Note that we start from 0!
    0, 1, 3, -- First Triangle
    1, 2, 3  -- Second Triangle
  ]

verts :: (Double, Double) -> [GLfloat]
verts p0 =
  [ -- | positions    -- | colors      -- | uv
    1.0,  1.0, 0.0,   1.0, 0.0, 0.0,   1.0 + tx, 1.0 + ty,
    1.0, -1.0, 0.0,   0.0, 1.0, 0.0,   1.0 + tx, 0.0 + ty,
   -1.0, -1.0, 0.0,   0.0, 0.0, 1.0,   0.0 + tx, 0.0 + ty,
   -1.0,  1.0, 0.0,   0.0, 0.0, 0.0,   0.0 + tx, 1.0 + ty
  ]
  where
    tx = (\ (x,y)-> realToFrac x) p0 :: GLfloat
    ty = (\ (x,y)-> realToFrac y) p0 :: GLfloat

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

initResources :: [GLfloat] -> [GLuint] -> Double -> IO Descriptor
initResources vs idx z0 =  
  do
    -- | VAO
    triangles <- genObjectName
    bindVertexArrayObject $= Just triangles

    -- | VBO
    vertexBuffer <- genObjectName
    bindBuffer ArrayBuffer $= Just vertexBuffer
    let numVertices = length vs
    withArray vs $ \ptr ->
      do
        let sizev = fromIntegral (numVertices * sizeOf (head vs))
        bufferData ArrayBuffer $= (sizev, ptr, StaticDraw)

    -- | EBO
    elementBuffer <- genObjectName
    bindBuffer ElementArrayBuffer $= Just elementBuffer
    let numIndices = length indices
    withArray idx $ \ptr ->
      do
        let indicesSize = fromIntegral (numIndices * (length indices))
        bufferData ElementArrayBuffer $= (indicesSize, ptr, StaticDraw)
        
    -- | Bind the pointer to the vertex attribute data
    let floatSize  = (fromIntegral $ sizeOf (0.0::GLfloat)) :: GLsizei
        stride     = 8 * floatSize

    -- | Positions
    let vPosition  = AttribLocation 0
        posOffset  = 0 * floatSize
    vertexAttribPointer vPosition $=
        (ToFloat, VertexArrayDescriptor 3 Float stride (bufferOffset posOffset))
    vertexAttribArray vPosition   $= Enabled

    -- | UV
    let uvCoords   = AttribLocation 1
        uvOffset   = 6 * floatSize
    vertexAttribPointer uvCoords  $=
        (ToFloat, VertexArrayDescriptor 2 Float stride (bufferOffset uvOffset))
    vertexAttribArray uvCoords    $= Enabled

    -- || Shaders
    program <- loadShaders [
        ShaderInfo VertexShader   (FileSource "mat/mandelbrot/shader.vert"),
        ShaderInfo FragmentShader (FileSource "mat/mandelbrot/shader.frag")]
    currentProgram $= Just program

    -- || Set Uniforms
    location <- get (uniformLocation program "fTime")
    uniform location $= (realToFrac z0 :: GLfloat)

    -- || Set Transform Matrix
    let tr :: [GLfloat]
        tr =
          [ 1, 0, 0, 0
          , 0, 1, 0, 0
          , 0, 0, 1, 0
          , 0, 0, 0, 1 ]
          
    transform <- GL.newMatrix ColumnMajor tr :: IO (GLmatrix GLfloat)
    location2 <- get (uniformLocation program "transform")
    uniform location2 $= (transform)

    -- || Unload buffers
    bindVertexArrayObject         $= Nothing

    return $ Descriptor triangles (fromIntegral numIndices)

data FontSet a = FontSet
  { droidFont :: a
  , defaultFont :: a
  , notoFont :: a
  }
  deriving (Functor, Foldable, Traversable)

fontSet :: FontSet FontSource
fontSet = FontSet
  { -- The first mentioned font is loaded first
    -- and set as a global default.
    droidFont =
      FontAtlas.FromTTF
        "./fonts/DroidSans.ttf"
        30
        Nothing
        FontAtlas.Cyrillic

    -- You also may use a default hardcoded font for
    -- some purposes (i.e. as fallback)
  , defaultFont =
      FontAtlas.DefaultFont

    -- To optimize atlas size, use ranges builder and
    -- provide source localization data.
  , notoFont =
      FontAtlas.FromTTF
        "./fonts/NotoSansJP-Regular.otf"
        20
        Nothing
        ( FontAtlas.RangesBuilder $ mconcat
            [ FontAtlas.addRanges FontAtlas.Latin
            , FontAtlas.addText "私をクリックしてください"
            , FontAtlas.addText "こんにちは"
            ]
        )
  }

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Hello, Dear ImGui!"
          sizex = 1280
          sizey = 720
          config = defaultWindow { windowGraphicsContext = OpenGLContext defaultOpenGL
                                 , SDL.windowInitialSize = V2 sizex sizey
                                 , SDL.windowResizable   = True }
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext window) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL3Init openGL3Shutdown

    fontSet' <- FontAtlas.rebuild fontSet

    liftIO $ do
      mainLoop window fontSet' initUI

mainLoop :: Window -> FontSet Font -> UI -> IO ()
mainLoop window fs = loop
  where
    loop ui0 = unlessQuit do
      -- Tell ImGui we're starting a new frame
      openGL3NewFrame
      sdl2NewFrame
      newFrame

      ui1 <- uiFrameAction fs ui0
              
      GL.clearColor $= Color4 0 0 0 1
      GL.clear [ColorBuffer]

      draw
              
      render
      openGL3RenderDrawData =<< getDrawData
       
      SDL.glSwapWindow window
      SDL.delay 100
       
      loop ui1
       
      where
        -- Process the event loop
        unlessQuit action = do
          shouldQuit <- checkEvents
          if shouldQuit then pure () else action
       
        checkEvents = do
          pollEventWithImGui >>= \case
            Nothing ->
              return False
            Just event ->
              (isQuit event ||) <$> checkEvents
       
        isQuit event =
          SDL.eventPayload event == SDL.QuitEvent

uiFrameAction :: FontSet Font -> UI -> IO UI
uiFrameAction fs ui = do
  switch' <- newIORef $ switch ui
  let FontSet{..} = fs
  withFullscreen do
    withFont droidFont do
      case switch ui of
        Main    -> do
          newLine
          button "New Game" >>= \case
            False -> return ()
            True  -> putStrLn "New Game!"
          button "Options" >>= \case
            False -> return ()
            True  -> writeIORef switch' Options
          button "Quit" >>= \case
            False -> return ()
            True  -> quit
          
        Options -> do
          text "Paraya Pre-Alpha"
          newLine
          button "Back" >>= \case
            False -> return ()
            True  -> writeIORef switch' Main
          
  action' <- readIORef switch'
  return ui { switch = action' }

data UIContext = Main | Options
  
data UI
  = UI
  {
    switch     :: UIContext
  -- , btnNewGame :: IO Bool
  -- , btnOptions :: IO Bool
  -- , btnQuit    :: IO Bool
  }

initUI :: UI
initUI = UI
  {
    switch = Main
  --   btnNewGame = button "New Game"
  -- , btnOptions = button "Options"
  -- , btnQuit    = button "Quit"
  }

draw :: IO ()
draw = do
  let p0 = (0,0) :: (Double, Double)
      z0 = 0     :: Double
  (Descriptor triangles numIndices) <- initResources (verts p0) indices z0
  
  bindVertexArrayObject $= Just triangles
  drawElements Triangles numIndices GL.UnsignedInt nullPtr
