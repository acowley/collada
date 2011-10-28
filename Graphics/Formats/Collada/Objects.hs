module Graphics.Formats.Collada.Objects
    ( Dict, ID
    , Model(..), Object(..), Matrix(..)
    , Accessor(..), Input(..), InputSemantic(..), Primitive(..)
    , Mesh(..), Parameter(..), Technique(..)
    , ColorOrTexture(..), Node(..), NodeRef(..), NodeInstance(..)
    , MaterialBinding(..), parseCollada
    )
where

import Prelude hiding ((.), id)
import Text.XML.HXT.Core
--import qualified Text.XML.HXT.Arrow.ParserInterface as X
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, isJust, fromJust)
import Control.Category
import Control.Arrow
import Foreign.Ptr
import Foreign.Marshal.Array
import Data.List

type Dict = Map.Map String Object
type ID = String

-- This corresponds to the notion of a Collada @scene@. The identifier
-- used to refer to a scene is drawn from the @url@ attribute of the
-- @instance_visual_scene@ element.
data Model = Model { modelScale :: Float
                   , modelScene :: ID
                   , modelDict  :: Dict }

data Object = OVisualScene [NodeRef]
            | OFloatArray [Float]
            | OSource Accessor
            | OVertices [Input]
            | OGeometry Mesh
            | OImage FilePath
            | OParam Parameter
            | OEffect Technique
            | OMaterial ID -- ^instance_effect
            | ONode Node
              deriving Show

data Matrix = Matrix [Float] deriving Show

identityMatrix :: Matrix
identityMatrix = Matrix [ 1, 0, 0, 0
                        , 0, 1, 0, 0
                        , 0, 0, 1, 0
                        , 0, 0, 0, 1 ]

-- |array components count stride offset
data Accessor = Accessor ID Int Int Int Int deriving Show

-- |offset semantic source
data Input = Input Int InputSemantic ID deriving Show

data InputSemantic = SemPosition
                   | SemNormal
                   | SemVertex
                   | SemTexCoord
                     deriving (Eq,Show)

-- |material inputs indices
data Primitive = PrimTriangles String [Input] [Int] deriving Show

data Mesh = Mesh [Primitive] deriving Show

data Parameter = ParamSurface2D ID
               | ParamSampler2D ID
                 deriving Show

data Technique = TechLambert ColorOrTexture -- ^diffuse
               | TechConstant ColorOrTexture Float 
                 -- ^transparent transparency
                 deriving Show

data ColorOrTexture = COTColor Float Float Float Float -- ^RGBA
                    | COTTexture ID String   -- ^source texcoord
                      deriving Show

data Node = Node Matrix [NodeInstance] deriving Show

data NodeRef = NRNode Node
             | NRInstance ID
               deriving Show

data NodeInstance = NINode NodeRef
                  | NIGeometry ID [MaterialBinding]
                    deriving Show

-- |symbol target semantic input_semantic
data MaterialBinding = MaterialBinding String ID String String deriving Show

parseCollada :: ArrowXml a => a XmlTree Model
parseCollada = hasName "COLLADA" >>>
               (massage ^<< mainScale &&& mainScene &&& (multi objects >. Map.unions))
  where massage (x,(y,z)) = Model x y z

mainScale :: ArrowXml a => a XmlTree Float
-- mainScale = read ^<< getAttrValue0 "meter" <<< child (hasName "unit") <<< child (hasName "asset")
mainScale = getChildren >>> hasName "asset" /> hasName "unit" >>> getAttrValue0 "meter" >>^ read

-- Get a named attribute, stipping off the @#@ prefix if found.
refAttr :: ArrowXml a => String -> a XmlTree ID
refAttr = (>>^ stripHash) . getAttrValue0
  where stripHash ('#':x) = x
        stripHash x = x

objects :: ArrowXml a => a XmlTree Dict
objects = asum [ float_array, source, vertices, geometry, image,
                 newparam, effect, material, visual_scene ]
          

-- objects = asum [ float_array, source, vertices, geometry, image,
--                  newparam, effect, material, node, visual_scene ] >.
--           first (Map.fromList . map (first fromJust)) . partition (isJust . fst)

mainScene :: ArrowXml a => a XmlTree ID
mainScene = getChildren >>> hasName "scene" /> hasName "instance_visual_scene" >>> refAttr "url"
-- refAttr "url" <<< child (hasName "instance_visual_scene") <<< child (hasName "scene")

asum :: ArrowPlus a => [a b c] -> a b c
asum = foldr1 (<+>)

newparam = undefined
effect = undefined
material = undefined
node = undefined
visual_scene = undefined

-- Build a singleton 'Map' from an element with the given name using
-- the supplied arrow to build an 'Object' from the element. The
-- element is assumed to have an @id@ attribute that is used as the
-- key in the 'Map'.
object :: ArrowXml a => String -> a XmlTree Object -> a XmlTree Dict
object name mkObj = hasName name >>>
                    (getAttrValue0 "id" &&& mkObj) >>> arr2 Map.singleton

-- We assume every @float_array@ has an id
float_array :: ArrowXml a => a XmlTree Dict
float_array = object "float_array" $ getChildren >>> getText >>^ toArray
  where toArray = OFloatArray . map read . words

arr5 :: Arrow a => (b1 -> b2 -> b3 -> b4 -> b5 -> c) -> a (b1, (b2, (b3, (b4, b5)))) c
arr5 f = arr (\ ~(x1, ~(x2, ~(x3, ~(x4, x5)))) -> f x1 x2 x3 x4 x5)

accessor :: ArrowXml a => a XmlTree Accessor
accessor = hasName "accessor" >>> 
             refAttr "source" &&& 
             (getChildren >>> hasName "param" >. length)
             &&& (getAttrValue0 "count" >>^ read)
             &&& ((getAttrValue0 "stride" >>^ read) `withDefault` 1)
             &&& ((getAttrValue0 "offset" >>^ read) `withDefault` 0)
           >>> arr5 Accessor

source :: ArrowXml a => a XmlTree Dict
source = object "source" $ 
         getChildren >>> hasName "technique_common" /> accessor >>^ OSource

-- Note that unshared inputs do not have an @offset@ attribute. For
-- those inputs, the offset is determined by the positional order of
-- the @input@ element within its parent's scope.
input :: ArrowXml a => a XmlTree (Maybe Int, (InputSemantic, String))
input = hasName "input" >>>
          ((getAttrValue0 "offset" >>^ read) `withDefault` Nothing)
          &&& (getAttrValue0 "semantic" >>^ massageSemantic) 
          &&& refAttr "source"
  where massageSemantic "POSITION" = SemPosition
        massageSemantic "NORMAL"   = SemNormal
        massageSemantic "VERTEX"   = SemVertex
        massageSemantic "TEXCOORD" = SemTexCoord
        massageSemantic s = error $ "Unknown semantic: " ++ s

-- Fill in the offset for unshared inputs using the positional
-- ordering of @input@ elements within a parent's scope.
fixedInputs :: ArrowXml a => ([Input] -> d) -> a XmlTree d
fixedInputs f = input >. f . fixups
  where fixups = zipWith fixup [0..]
        fixup n (Nothing, (sem, source)) = Input n sem source
        fixup _ (Just n, (sem, source)) = Input n sem source

vertices :: ArrowXml a => a XmlTree Dict
vertices = object "vertices" $ getChildren >>> fixedInputs OVertices

triangles :: ArrowXml a => a XmlTree Primitive
triangles = hasName "triangles" >>>
              getAttrValue "material" 
              &&& (getChildren >>> fixedInputs id)
              &&& (getChildren >>> hasName "p" /> getText >>^ map read . words)
            >>> arr3 PrimTriangles

mesh :: ArrowXml a => a XmlTree Mesh
mesh = hasName "mesh" /> triangles >. Mesh

geometry :: ArrowXml a => a XmlTree Dict
geometry = object "geometry" $ getChildren >>> mesh >>^ OGeometry

image :: ArrowXml a => a XmlTree Dict
image = object "image" $ 
        getChildren >>> hasName "init_from" /> getText >>^ OImage


{-

child n = n <<< X.getChildren


newparam :: X.LA X.XmlTree Dict
newparam = objectWithIDAttr "sid" "newparam" $ OParam ^<< asum [surface, sampler2D] <<< X.getChildren
    where
    surface = ParamSurface2D ^<< child X.getText <<< child (X.hasName "init_from") <<< X.hasAttrValue "type" (== "2D") <<< X.hasName "surface"
    sampler2D = ParamSampler2D ^<< child X.getText <<< child (X.hasName "source") <<< X.hasName "sampler2D"

colorOrTexture :: X.LA X.XmlTree ColorOrTexture
colorOrTexture = texture X.<+> color
    where
    texture = uncurry COTTexture ^<< X.getAttrValue0 "texture" &&& X.getAttrValue0 "texcoord" <<< X.hasName "texture"
    color = colorify . map read . words ^<< child X.getText <<< X.hasName "color"
    colorify [r,g,b,a] = COTColor r g b a
    colorify s = error "Malformed color"

lambert :: X.LA X.XmlTree Technique
lambert = TechLambert ^<< child colorOrTexture <<< child (X.hasName "diffuse") <<< X.hasName "lambert"

constant :: X.LA X.XmlTree Technique
constant = uncurry TechConstant ^<< (child colorOrTexture <<< child (X.hasName "transparent")) &&& (read ^<< child (X.getText) <<< child (X.hasName "float") <<< child (X.hasName "transparency")) <<< X.hasName "constant"

technique :: X.LA X.XmlTree Technique
technique = asum [lambert, constant] <<< X.getChildren <<< X.hasName "technique"

effect :: X.LA X.XmlTree Dict
effect = object "effect" $ OEffect ^<< child technique <<< child (X.hasName "profile_COMMON")

material :: X.LA X.XmlTree Dict
material = object "material" $ OMaterial ^<< refAttr "url" <<< child (X.hasName "instance_effect")

nodeRef :: X.LA X.XmlTree NodeRef
nodeRef = asum [inline, instance_node] 
    where
    inline = (arr NRInstance ||| (NRNode ^<< rawNode)) <<< switch <<< X.hasName "node"
    switch = convid ^<< X.getAttrValue "id" &&& id
    convid ("", xml) = Right xml
    convid (x, _)    = Left x

instance_node :: X.LA X.XmlTree NodeRef
instance_node = NRInstance ^<< refAttr "url" <<< X.hasName "instance_node"

nodeInstance :: X.LA X.XmlTree NodeInstance
nodeInstance = asum [NINode ^<< nodeRef, instance_geometry]

instance_geometry :: X.LA X.XmlTree NodeInstance
instance_geometry = uncurry NIGeometry ^<< refAttr "url" &&& bindings <<< X.hasName "instance_geometry"
    where
    bindings = id .< (child instance_material <<< child (X.hasName "technique_common") <<< child (X.hasName "bind_material"))

matrix :: X.LA X.XmlTree Matrix
matrix = Matrix . map read . words ^<< child X.getText <<< X.hasName "matrix"

rawNode :: X.LA X.XmlTree Node
rawNode = uncurry Node ^<< (child matrix `X.withDefault` identityMatrix) &&& (id .< child nodeInstance) <<< X.hasName "node"

node :: X.LA X.XmlTree Dict
node = object "node" $ ONode ^<< rawNode

instance_material :: X.LA X.XmlTree MaterialBinding
instance_material = conv ^<< myAttrs &&& bindAttrs <<< X.hasName "instance_material"
    where
    conv ((symbol, target), (semantic, input_semantic)) = MaterialBinding symbol target semantic input_semantic
    myAttrs = X.getAttrValue0 "symbol" &&& refAttr "target"
    bindAttrs = X.getAttrValue0 "semantic" &&& X.getAttrValue0 "input_semantic" <<< child (X.hasName "bind_vertex_input")

visual_scene :: X.LA X.XmlTree Dict
visual_scene = object "visual_scene" $ OVisualScene ^<< id .< child nodeRef
-}