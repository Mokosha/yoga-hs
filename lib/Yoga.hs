module Yoga (
  -- Initialization
  Layout, payload, children, node, container, rightToLeftContainer,

  -- Style attributes
  Direction(..), Justify(..), Align(..), PositionType(..), Overflow(..),
  Edge(..),

  -- Rendering
  render, foldRender,

) where
--------------------------------------------------------------------------------
import Bindings.Yoga
import Bindings.Yoga.Enums

import Data.Foldable
import Data.Traversable()

import Foreign.C.Types (CFloat)
import Foreign.ForeignPtr

import GHC.Ptr (Ptr)

import Numeric.IEEE

import System.IO.Unsafe
--------------------------------------------------------------------------------

type NativeNode = ForeignPtr C'YGNode
type NativeNodePtr = Ptr C'YGNode
data Layout a =
  Layout { payload :: a,
           children :: [Layout a],
           internalPtr :: NativeNode
         }

type RenderFn m a b = (Float, Float) -> (Float, Float) -> a -> m b

instance Functor Layout where
  fmap f (Layout x cs ptr) = Layout (f x) (fmap (fmap f) cs) ptr

instance Foldable Layout where
  foldMap f (Layout x cs _) = f x `mappend` (foldMap (foldMap f) cs)
  foldl f z (Layout x cs _) = foldl (foldl f) (f z x) cs
  foldr f z (Layout x cs _) = f x $ foldr (flip $ foldr f) z cs

instance Traversable Layout where
  traverse f (Layout x cs ptr) =
    Layout <$> f x <*> (sequenceA $ traverse f <$> cs) <*> pure ptr
  sequenceA (Layout x cs ptr) =
    Layout <$> x <*> sequenceA (sequenceA <$> cs) <*> pure ptr

data Direction
  = Column
  | ColumnReverse
  | Row
  | RowReverse

data Justify
  = Justify'FlexStart
  | Justify'Center
  | Justify'FlexEnd
  | Justify'SpaceBetween
  | Justify'SpaceAround

data Align
  = Align'Auto
  | Align'FlexStart
  | Align'Center
  | Align'FlexEnd
  | Align'Stretch

data PositionType
  = Relative
  | Absolute

data Overflow
  = Visible
  | Hidden
  | Scroll

data Edge
  = Edge'Left
  | Edge'Top
  | Edge'Right
  | Edge'Bottom
  | Edge'Start
  | Edge'End
  | Edge'Horizontal
  | Edge'Vertical
  | Edge'All

node :: Float -> Float -> a -> Layout a
node width height x = unsafePerformIO $ do
  ptr <- c'YGNodeNew
  c'YGNodeStyleSetWidth ptr (realToFrac width)
  c'YGNodeStyleSetHeight ptr (realToFrac height)
  Layout x [] <$> newForeignPtr p'YGNodeFree ptr

addChildrenToContainer :: NativeNodePtr -> a -> [Layout a] -> IO (Layout a)
addChildrenToContainer ptr x cs = do
  forM_ (zip cs [0..]) $ \(child, idx) ->
    withForeignPtr (internalPtr child) $ \childPtr ->
    c'YGNodeInsertChild ptr childPtr idx
  Layout x cs <$> newForeignPtr p'YGNodeFree ptr

container :: a -> [Layout a] -> Layout a
container x cs = unsafePerformIO $ do
  ptr <- c'YGNodeNew
  addChildrenToContainer ptr x cs

rightToLeftContainer :: a -> [Layout a] -> Layout a
rightToLeftContainer x cs = unsafePerformIO $ do
  ptr <- c'YGNodeNew
  c'YGNodeStyleSetDirection ptr c'YGDirectionRTL
  addChildrenToContainer ptr x cs

calculateLayout :: Monad m => NativeNode -> m ()
calculateLayout ptr = do
  return $ unsafePerformIO $ withForeignPtr ptr $ \cptr ->
    let n = (nan :: CFloat)
    in c'YGNodeStyleGetDirection cptr >>= c'YGNodeCalculateLayout cptr n n

layoutBounds :: NativeNode -> (Float, Float, Float, Float)
layoutBounds ptr =
  (getLayout c'YGNodeLayoutGetLeft,
   getLayout c'YGNodeLayoutGetTop,
   getLayout c'YGNodeLayoutGetWidth,
   getLayout c'YGNodeLayoutGetHeight)
  where
    cf = realToFrac :: CFloat -> Float
    getLayout lytFn = cf $ unsafePerformIO $ withForeignPtr ptr lytFn

renderTree :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
renderTree (Layout x cs ptr) f = do
  (left, top, width, height) <- return $ layoutBounds ptr
  Layout
    <$> f (left, top) (width, height) x
    <*> mapM (flip renderTree f) cs
    <*> pure ptr

render :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
render lyt@(Layout _ _ ptr) f = do
  _ <- calculateLayout ptr
  renderTree lyt f

foldRenderTree :: (Monad m, Monoid b) =>
                  Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRenderTree (Layout x cs ptr) f = do
  (left, top, width, height) <- return $ layoutBounds ptr
  (m, y) <- f (left, top) (width, height) x
  cs' <- mapM (flip foldRenderTree f) cs
  return (mappend m . foldr mappend mempty . map fst $ cs',
          Layout y (map snd cs') ptr)

foldRender :: (Monad m, Monoid b) =>
              Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRender lyt@(Layout _ _ ptr) f = do
  _ <- calculateLayout ptr
  foldRenderTree lyt f
