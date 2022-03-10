{-# LANGUAGE TemplateHaskell #-}

module ObjectTree
  ( ObjectTree (..)
  , gui
  , ObjectTree.foreground
  , ObjectTree.background
  , fromProject
  , GUI (..)
  , ObjectTree.fonts
  ) where

import Control.Lens hiding (transform, pre)
import Linear.Matrix
import Linear (V3(..))
import Graphics.Rendering.OpenGL (ShaderType (..))
import GHC.Float

import Graphics.RedViz.Project as Project
    ( solverAttrs,
      solvers,
      pname,
      fonts,
      modelIDXs,
      models,
      background,
      objects,
      PreObject(_ptype),
      Project )
import Graphics.RedViz.Project.Model as Model
import Graphics.RedViz.Material as Material
import Graphics.RedViz.Descriptor
import Graphics.RedViz.PGeo (readBGeo, fromVGeo, fromSVGeo, VGeo(..), SVGeo(..), smp, sxf, svl)
import Graphics.RedViz.Utils as U
import Graphics.RedViz.Object
import Graphics.RedViz.Rendering (toDescriptor)
import Graphics.RedViz.VAO (VAO')
import Graphics.RedViz.LoadShaders

import Object
import Solvable

data GUI =
     GUI
     {
       _fonts   :: [Object]
     , _icons   :: [Object]
--     , _widgets :: [Object] -- TODO: think about a widget set?     
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

fromProject :: Project -> IO ObjectTree
fromProject prj0 = do
  let
    pobjs = concat $ toListOf Project.objects    prj0 :: [PreObject]
    pbgrs = concat $ toListOf Project.background prj0 :: [PreObject]
    -- pfnts = concat $ toListOf Project.fonts      prj0 :: [PreObject]
  
  objs <- mapM (fromPreObject prj0 Foreground) pobjs :: IO [Object]
  bgrs <- mapM (fromPreObject prj0 Background) pbgrs :: IO [Object]
  fnts <- initFontObject prj0 :: IO [Object]
  let result =
        ObjectTree
        (GUI fnts [])
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
        Font       -> (toListOf (Project.fonts . traverse . Model.path) project)

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
    name'        = view pname pObj0     :: String
    psolvers'    = view Project.solvers pObj0     :: [String]
    solverAttrs' = view Project.solverAttrs pObj0 :: [[Double]]
    solversF     = case psolvers' of
                     [] -> [""]
                     _  -> psolvers'     :: [String]
    attrsF       = case solverAttrs' of
                     [] -> [[]]
                     _  -> solverAttrs'  :: [[Double]]
    solvers'     = toSolver <$> zip solversF attrsF :: [Solver]
    xforms'      = U.fromList <$> toListOf (traverse . sxf) svgeos' :: [M44 Double]
    transforms'  =
      case cls of
        Font -> xforms'
        _    -> uncurry preTransformer <$> (zip solvers' xforms' ::[(Solver, M44 Double)]) :: [M44 Double]
                  
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
           time')
          name'
          velocity'
          avelocity'
          mass'
          density'
          solvers'
        "sprite" -> return $
          Sprite
          (Object'
           ds'
           materials'
           programs'
           transforms'
           time')
        ""       -> return Object.Empty :: IO Object
        _        -> return Object.Empty :: IO Object    

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
        , _transforms  = preTransforms
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
