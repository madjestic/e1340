{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ObjectTree
  ( ObjectTree (..)
  , ObjectTree.foreground
  , ObjectTree.background
  , fromProject
  , Widget (..)
  , ObjectTree.fonts
  , ObjectTree.icons
  ) where

import Control.Lens hiding (transform, pre)
import Linear.Matrix
import Linear (V3(..), V4(..))
import Graphics.Rendering.OpenGL (ShaderType (..))
import GHC.Float

import Graphics.RedViz.Project as Project hiding (_solvers)
import Graphics.RedViz.Project as P       hiding (_solvers)
import Graphics.RedViz.Project.Model as Model
import Graphics.RedViz.Material as Material
import Graphics.RedViz.Descriptor
import Graphics.RedViz.PGeo ( readBGeo
                            , readPGeo
                            , fromVGeo
                            , fromSVGeo
                            , VGeo(..)
                            , SVGeo(..)
                            , smp
                            , sxf
                            , svl
                            , savl
                            , sms )
import Graphics.RedViz.Utils as U
import Graphics.RedViz.Object as Obj
import Graphics.RedViz.Rendering (toDescriptor)
import Graphics.RedViz.VAO (VAO')
import Graphics.RedViz.LoadShaders
import Graphics.RedViz.Widget as Widget

import Object
import Solvable hiding (_ypr)
import Object.Update (updateOnce)

import Debug.Trace as DT

data ObjectTree =
  ObjectTree
  {
    _foreground :: [Object]    
  , _background :: [Object]
  , _fonts      :: [Object] 
  , _icons      :: [Object] 
  } deriving Show
$(makeLenses ''ObjectTree)

data ObjectClass = Foreground | Background | Font | Icon

fromProject :: Project -> IO ObjectTree
fromProject prj0 = do
  let
    pobjs = concat $ toListOf Project.objects    prj0 :: [PreObject]
    pbgrs = concat $ toListOf Project.background prj0 :: [PreObject]
  
  objs <- mapM (fromPreObject prj0 Foreground) pobjs :: IO [Object]
  bgrs <- mapM (fromPreObject prj0 Background) pbgrs :: IO [Object]
  fnts <- initFontObjects prj0 :: IO [Object]
  icns <- initIconObjects prj0 :: IO [Object]
  let
    result =
      ObjectTree
        objs
        bgrs
        fnts
        icns
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
        Icon       -> project ^.. Project.gui . Project.icons . traverse . path

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
                     _  -> presolverAttrs'    :: [[Double]]
    attrsF       = case solverAttrs' of
                     [] -> [[]]
                     _  -> solverAttrs'       :: [[Double]]

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
    velocity'    = toV3 (fmap float2Double (head vels)) :: V3 Double -- TODO: replace with something more sophisticated, like a sum or average?
    avels        = toListOf (traverse . savl) svgeos' :: [[Float]]
    avelocity'   = toV3 (fmap float2Double (head avels)) :: V3 Double -- TODO: replace with something more sophisticated, like a sum or average?
    --velocity'    = V3 0 0 0 -- toV3 (fmap float2Double (head vels)) :: V3 Double -- TODO: replace with something more sophisticated, like a sum or average?
    --avelocity'   = V3 0 0 0 :: V3 Double
    ms           = toListOf (traverse . sms) svgeos' :: [Float]
    mass'        = (float2Double (head ms)) --1.0 :: Double
    --mass'        = 1.0 :: Double
    density'     = 1.0 :: Double
    time'        = 0.0 :: Double

  case cls of
    Font -> return $ updateOnce $
      Object.Sprite
      (Object'
       ds'
       materials'
       programs'
       transforms'
       (head transforms')
       --(identity :: M44 Double)
       (sum ypr')
       (V3 0 0 0 :: V3 Double)
       time')
    _ ->
      case _ptype pObj0 of
        "planet" -> return $ updateOnce $
          Planet
          {
            _base = 
            ( Object'
              {
               _descriptors = ds'
              ,_materials   = materials'
              ,_programs    = programs'
              ,_transforms  = transforms'
              ,_transform0  = identity :: M44 Double --(head transforms')
              ,_ypr0        = (sum ypr')
              ,_ypr         = (V3 0 0 0 :: V3 Double)
              ,_time        = time'
              }
            )
          ,_nameP     = name'
          ,_velocity  = velocity'
          ,_avelocity = avelocity'
          ,_mass      = mass'
          ,_density   = density'
          ,_solvers   = solvers''
          }
        "rbd" -> return $ updateOnce $
          RBD
          {
            _base = 
            ( Object'
              {
               _descriptors = ds'
              ,_materials   = materials'
              ,_programs    = programs'
              ,_transforms  = transforms'
              ,_transform0  = (head transforms')
              ,_ypr0        = (sum ypr')
              ,_ypr         = (V3 0 0 0 :: V3 Double)
              ,_time        = time'
              }
            )
          ,_nameP     = name'
          ,_velocity  = velocity'
          ,_avelocity = avelocity'
          ,_mass      = mass'
          ,_density   = density'
          ,_solvers   = solvers''
          }
        "sprite" -> return $ updateOnce $
          Sprite
          (Object'
           ds'
           materials'
           programs'
           transforms'
           (head transforms')
           (sum ypr')
           (V3 0 0 0 :: V3 Double)
           time')
        ""       -> return emptyObj :: IO Object
        _        -> return emptyObj :: IO Object

initFontObjects :: Project -> IO [Object]
initFontObjects prj0 = do
  fntVGeos  <- mapM readBGeo $ modelPaths Font prj0 :: IO [VGeo]
                               
  if not (null fntVGeos)
    then
      mapM initObject' fntVGeos
    else
      pure []  :: IO [Object]

initIconObjects :: Project -> IO [Object]
initIconObjects prj0 = do
  icnVGeos  <- mapM readBGeo $ modelPaths Icon prj0 :: IO [VGeo]
                               
  if not (null icnVGeos)
    then
      mapM initObject' icnVGeos
    else
      pure []  :: IO [Object]
               
initObject' :: VGeo -> IO Object
initObject' vgeo = do
  let (VGeo is_ st_ vs_ _ _ _ _ xf_) = vgeo
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
        , _transform0  = (identity::M44 Double)
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
    --vgeos       = readPGeo <$> ["models/box.pgeo"]
  sequence vgeos

toDescriptorSVGeo :: Project -> PreObject -> IO ([Descriptor], [SVGeo])
toDescriptorSVGeo prj0 pObj0 = do
  vgeos' <- toVGeo prj0 pObj0
  let
    svgeos = concat $ fromVGeo <$> vgeos' :: [SVGeo] 
    vao'   = fromSVGeo <$> svgeos :: VAO'
  ds' <- mapM toDescriptor vao'
  return (ds', svgeos) :: IO ([Descriptor], [SVGeo])
