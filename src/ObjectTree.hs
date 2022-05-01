{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ObjectTree
  ( ObjectTree (..)
  , ObjectTree.gui
  , ObjectTree.foreground
  , ObjectTree.background
  , fromProject
  , GUI (..)
  , Widget (..)
  , ObjectTree.fonts
  , ObjectTree.widgets
  ) where

import Control.Lens hiding (transform, pre)
import Linear.Matrix
import Linear (V3(..), V4(..))
import Graphics.Rendering.OpenGL (ShaderType (..))
import GHC.Float

import Graphics.RedViz.Project as Project
import Graphics.RedViz.Project as P
import Graphics.RedViz.Project.Model as Model
import Graphics.RedViz.Material as Material
import Graphics.RedViz.Descriptor
import Graphics.RedViz.PGeo (readBGeo, fromVGeo, fromSVGeo, VGeo(..), SVGeo(..), smp, sxf, svl)
import Graphics.RedViz.Utils as U
import Graphics.RedViz.Object as Obj
import Graphics.RedViz.Rendering (toDescriptor)
import Graphics.RedViz.VAO (VAO')
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.Widget as Widget

import Object
import Solvable

--import Debug.Trace as DT

data GUI
  =  GUI
     {
       _fonts   :: [Object]
     , _icons   :: [Object]
     , _widgets :: [Widget]
     } deriving Show
$(makeLenses ''GUI)

data ObjectTree =
  ObjectTree
  {
    _gui        :: GUI
  , _foreground :: [Object]
  , _background :: [Object]
  } deriving Show
$(makeLenses ''ObjectTree)

data ObjectClass = Foreground | Background | Font

toWidgets :: Project -> [Widget]
toWidgets prj0 = ws
  where
    ws' = prj0 ^. Project.gui . Project.widgets
    ws  = toWidget <$> ws'

toFormat :: Format' -> Format
toFormat f' =
  Format
  {
    Widget._alignment =
      case f'^.Project.alignment of
        "TL" -> TL
        "TC" -> TC
        "TR" -> TR
        "CL" -> CL
        "CC" -> CC
        "CR" -> CR
        "BL" -> BL
        "BC" -> BC
        "BR" -> BR
        _    -> CC
        
  , Widget._voffset = f'^.Project.voffset
  , Widget._hoffset = f'^.Project.hoffset
  , Widget._soffset = f'^.Project.soffset
  , Widget._ssize   = f'^.Project.ssize
  }

toWidget :: Project.Widget' -> Widget
toWidget ws' =
  case ws' of
    TextField' b t f -> TextField b t (toFormat f)
    FPS' b f         -> FPS b (toFormat f)

fromProject :: Project -> IO ObjectTree
fromProject prj0 = do
  let
    pobjs = concat $ toListOf Project.objects    prj0 :: [PreObject]
    pbgrs = concat $ toListOf Project.background prj0 :: [PreObject]
    -- pfnts = concat $ toListOf Project.fonts      prj0 :: [PreObject]
  
  objs <- mapM (fromPreObject prj0 Foreground) pobjs :: IO [Object]
  bgrs <- mapM (fromPreObject prj0 Background) pbgrs :: IO [Object]
  fnts <- initFontObject prj0 :: IO [Object] -- TODO
  let
    wgts   = toWidgets prj0
    result =
      ObjectTree
      (GUI fnts [] wgts)
      objs
      bgrs
  putStrLn "Finished loading objects."
  return result

-- | returns a list of model paths
modelPaths :: ObjectClass -> Project -> [String]
modelPaths cls project = modelList
  where
    modelSet  = toListOf (models . traverse . Model.path) project :: [String]
    modelList =
      case cls of
        Foreground -> (modelSet!!) <$> (concat $ toListOf ( objects . traverse . modelIDXs ) project)
        Background -> (modelSet!!) <$> (concat $ toListOf ( Project.background . traverse . modelIDXs ) project)
        Font       -> project ^.. Project.gui . Project.fonts . traverse . path

show' :: M44 Double -> String
show' (V4 x y z w) =
  show x ++ "\n" ++
  show y ++ "\n" ++
  show z ++ "\n" ++
  show w ++ "\n"

fromPreObject :: Project -> ObjectClass -> PreObject -> IO Object
fromPreObject prj0 cls pObj0 = do
  (ds', svgeos') <- toDescriptorSVGeo prj0 pObj0 :: IO ([Descriptor], [SVGeo])
  materials'     <- mapM Material.read $ toListOf (traverse . smp) svgeos' :: IO [Material]
  programs'      <- mapM
    (\mat -> loadShaders
             [ ShaderInfo VertexShader   (FileSource (_vertShader mat ))
             , ShaderInfo FragmentShader (FileSource (_fragShader mat )) ])
    materials'

  let
    name'           = view pname pObj0        :: String
    
    presolvers'     = pObj0 ^. P.presolvers   :: [String]
    psolvers'       = pObj0 ^. P.solvers      :: [String]
    
    presolverAttrs' = pObj0 ^. presolverAttrs :: [[Double]]
    solverAttrs'    = pObj0 ^. solverAttrs    :: [[Double]]

    presolversF  = case presolvers' of
                     [] -> [""]
                     _  -> presolvers'        :: [String]
    solversF     = case psolvers' of
                     [] -> [""]
                     _  -> psolvers'          :: [String]

    preattrsF    = case presolverAttrs' of
                     [] -> [[]]
                     _  -> presolverAttrs'  :: [[Double]]
    attrsF       = case solverAttrs' of
                     [] -> [[]]
                     _  -> solverAttrs'  :: [[Double]]

    presolvers''  = toSolver <$> zip presolversF preattrsF :: [Solver]                     
    solvers''     = toSolver <$> zip solversF    attrsF    :: [Solver]
    
    xforms'      = U.fromList <$> toListOf (traverse . sxf) svgeos' :: [M44 Double]
    ypr0s        = repeat (V3 0 0 0 :: V3 Double)
    
    (transforms', ypr')  =
      case cls of
        Font -> unzip $ zip xforms' ypr0s :: ([M44 Double], [V3 Double])
        _ -> unzip $ (\ (mtx, _ypr) -> foldPretransformers (mtx, _ypr) (reverse presolvers'')) <$> zip xforms' ypr0s
          where
            foldPretransformers :: (M44 Double, V3 Double) -> [Solver] -> (M44 Double, V3 Double)
            foldPretransformers (mtx0, _ypr0) []     = (mtx0, _ypr0)
            foldPretransformers (mtx0, _ypr0) [s]    = preTransformer s (foldPretransformers (mtx0, _ypr0) [])
            foldPretransformers (mtx0, _ypr0) (s:ss) = preTransformer s (foldPretransformers (mtx0, _ypr0) ss)
  
    vels         = toListOf (traverse . svl) svgeos' :: [[Float]]
    velocity'    = toV3 (fmap float2Double (head vels)) :: V3 Double -- TODO: replace with something more sophisticated?
    avelocity'   = V3 0 0 0 :: V3 Double
    mass'        = 1.0 :: Double
    density'     = 1.0 :: Double
    time'        = 0.0 :: Double

  case cls of
    Font -> return $
      Object.Sprite
      (Object'
       ds'
       materials'
       programs'
       transforms'
       transforms' -- []
       (identity::M44 Double)
       (sum ypr')
       (V3 0 0 0 :: V3 Double)
       time')
    _ ->
      case _ptype pObj0 of
        "planet" -> return $
          Planet
          (Object'
           ds'
           materials'
           programs'
           transforms'
           transforms' -- []
           (identity::M44 Double)
           (sum ypr')
           (V3 0 0 0 :: V3 Double)
           time')
          name'
          velocity'
          avelocity'
          mass'
          density'
          solvers''
        "sprite" -> return $
          Sprite
          (Object'
           ds'
           materials'
           programs'
           transforms'
           transforms' -- []
           (identity::M44 Double)
           (sum ypr')
           (V3 0 0 0 :: V3 Double)
           time')
        ""       -> return emptyObj :: IO Object
        _        -> return emptyObj :: IO Object

initFontObject :: Project -> IO [Object]
initFontObject prj0 = do
  fntVGeos  <- mapM readBGeo $ modelPaths Font prj0 :: IO [VGeo]
                               
  if not (null fntVGeos)
    then
      mapM initFontObject' fntVGeos
    else
      pure []  :: IO [Object]

initFontObject' :: VGeo -> IO Object
initFontObject' vgeo = do
  let (VGeo is_ st_ vs_ _ _ _ xf_) = vgeo
      vaoArgs       = (,,) <$.> is_ <*.> st_ <*.> vs_
      preTransforms = U.fromList <$> xf_
      
  mats' <- mapM Material.read $ mts vgeo :: IO [Material]
  ds    <- mapM toDescriptor vaoArgs
  progs <- mapM (\mat -> loadShaders
                         [ ShaderInfo VertexShader   (FileSource (_vertShader mat ))
                         , ShaderInfo FragmentShader (FileSource (_fragShader mat )) ]) mats'
  return $
    Object.Sprite
    {
      _base =
        Object'
        {
          _descriptors = ds
        , _materials   = mats'
        , _programs    = progs
        , _transforms0 = preTransforms
        , _transforms1 = preTransforms
        , _transformC  = (identity::M44 Double)
        , Obj._ypr     = V3 0 0 0 :: V3 Double
        , Obj._ypr0    = V3 0 0 0 :: V3 Double
        , _time        = 0.1
        }
    } 

toVGeo :: Project -> PreObject -> IO [VGeo]
toVGeo prj0 pObj0 = do
  let
    modelSet    = toListOf (models . traverse . Model.path) prj0 :: [String]
    modelPaths' = (modelSet!!) <$> view modelIDXs pObj0
    vgeos       = readBGeo <$> modelPaths'
  sequence vgeos

toDescriptorSVGeo :: Project -> PreObject -> IO ([Descriptor], [SVGeo])
toDescriptorSVGeo prj0 pObj0 = do
  vgeos' <- toVGeo prj0 pObj0
  let
    svgeos = concat $ fromVGeo <$> vgeos' :: [SVGeo] 
    vao'   = fromSVGeo <$> svgeos :: VAO'
  ds' <- mapM toDescriptor vao'
  return (ds', svgeos) :: IO ([Descriptor], [SVGeo])
