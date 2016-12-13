module Yoga (
  -- Initialization
  Layout, mkNode, mkContainer,

  -- Style attributes
  Direction(..), FlexDirection(..), Justify(..), Align(..),
  PositionType(..), Overflow(..), Edge(..),

) where
--------------------------------------------------------------------------------
import Bindings.Yoga

import Data.Foldable
import Data.Traversable

import Foreign.ForeignPtr
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
  foldl f init (Layout cs x _) = foldl (foldl f) (f init x) cs
  foldr f init (Layout cs x _) = f x $ foldr (flip $ foldr f) init cs

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
  Layout cs x <$> newForeignPtr p'YGNodeFree ptr
