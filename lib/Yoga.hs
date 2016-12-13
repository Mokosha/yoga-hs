module Yoga (
  -- Initialization
  Layout, payload, children, mkNode, mkContainer,

  -- Style attributes
  Direction(..), FlexDirection(..), Justify(..), Align(..),
  PositionType(..), Overflow(..), Edge(..),

  -- Rendering
  render,

) where
--------------------------------------------------------------------------------
import Bindings.Yoga
import Bindings.Yoga.Enums

import Data.Foldable
import Data.Traversable()

import Foreign.C.Types (CFloat)
import Foreign.ForeignPtr

import Numeric.IEEE

import System.IO.Unsafe
--------------------------------------------------------------------------------

data Layout a =
  Layout { children :: [Layout a],
           payload :: a,
           internalPtr :: ForeignPtr C'YGNode
         }

instance Functor Layout where
  fmap f (Layout cs x ptr) = Layout (fmap (fmap f) cs) (f x) ptr

instance Foldable Layout where
  foldMap f (Layout cs x _) = f x `mappend` (foldMap (foldMap f) cs)
  foldl f z (Layout cs x _) = foldl (foldl f) (f z x) cs
  foldr f z (Layout cs x _) = f x $ foldr (flip $ foldr f) z cs

instance Traversable Layout where
  traverse f (Layout cs x ptr) =
    Layout <$> (sequenceA $ traverse f <$> cs) <*> f x <*> pure ptr
  sequenceA (Layout cs x ptr) =
    pure (\x' cs' -> Layout cs' x' ptr) <*> x <*> sequenceA (sequenceA <$> cs)

data Direction
  = Inherit
  | LeftToRight
  | RightToLeft

data FlexDirection
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

mkNode :: a -> Layout a
mkNode x = unsafePerformIO $ do
  ptr <- c'YGNodeNew
  Layout [] x <$> newForeignPtr p'YGNodeFree ptr

mkContainer :: a -> [Layout a] -> Layout a
mkContainer x cs = unsafePerformIO $ do
  ptr <- c'YGNodeNew
  forM_ (zip cs [0..]) $ \(child, idx) ->
    withForeignPtr (internalPtr child) $ \childPtr ->
    c'YGNodeInsertChild ptr childPtr idx
  Layout cs x <$> newForeignPtr p'YGNodeFree ptr

type RenderFn m a b = (Float, Float) -> (Float, Float) -> a -> m b

renderTree :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
renderTree (Layout cs x ptr) f = do
  let cf = realToFrac :: CFloat -> Float
  let getLayout lytFn = return $ cf $ unsafePerformIO $ withForeignPtr ptr lytFn
  left <- getLayout c'YGNodeLayoutGetLeft
  top <- getLayout c'YGNodeLayoutGetTop
  width <- getLayout c'YGNodeLayoutGetWidth
  height <- getLayout c'YGNodeLayoutGetHeight
  Layout
    <$> mapM (flip renderTree f) cs
    <*> f (left, top) (width, height) x
    <*> pure ptr

render :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
render lyt@(Layout _ _ ptr) f = do
  _ <- return $ unsafePerformIO $ withForeignPtr ptr $ \cptr ->
        let n = (nan :: CFloat)
        in c'YGNodeCalculateLayout cptr n n c'YGDirectionLTR
  renderTree lyt f     
