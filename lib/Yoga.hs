{-|
Module      : Yoga
Description : Bindings to Facebook's Yoga layout engine
Copyright   : (c) Pavel Krajcevski, 2017
License     : MIT
Maintainer  : Krajcevski@gmail.com
Stability   : experimental
Portability : POSIX

This module holds a high-level interface to the bindings associated with this
library that are maintained in "Bindings.Yoga". Application developers will
likely want to use this module to interface with the library, but are available
to use the C-level bindings if more control is desired.

These bindings are not affiliated with Facebook in any way, and have been
developed separately for the sole purpose of interfacing with their open source
library.

Full documentation can be found at <http://facebook.github.io/yoga>
-}
module Yoga (
  -- ** Main datatype
  Layout,

  -- ** Children layouts
  -- | These layouts describe the way that children are ordered and spaced
  -- within their parent.
  Children, startToEnd, endToStart, centered, spaceBetween, spaceAround,
  wrapped,
  
  -- ** Containers
  hbox, vbox,
  hboxLeftToRight, hboxRightToLeft,
  vboxTopToBottom, vboxBottomToTop,

  -- ** Leaf nodes
  Size(..),
  shrinkable, growable, exact, withDimensions,

  -- ** Attributes
  Edge(..),
  stretched, withMargin, withPadding,

  -- ** Rendering
  LayoutInfo(..), RenderFn, render, foldRender,

) where
--------------------------------------------------------------------------------
import Bindings.Yoga
import Bindings.Yoga.Enums

import Control.Applicative
import Control.Monad hiding (mapM, forM_)

import Data.Foldable
import Data.Traversable
import Data.Monoid

import Foreign.C.Types (CFloat, CUInt)
import Foreign.ForeignPtr

import GHC.Ptr (Ptr)

import Numeric.IEEE

import System.IO.Unsafe

-- Last to avoid compiler warnings due to the Foldable/Traversable Proposal
import Prelude hiding (foldl, foldr, mapM)
--------------------------------------------------------------------------------

-- | The main datatype in the high level bindings is a 'Layout'. Layouts are
-- used to store a tree of nodes that represent the different components of a
-- layout. Layouts can be composed by adding one as a sub-tree to another. The
-- parent-child relationship dictates different parameters such as width, height
-- and position. This type is opaque to the user in order to facilitate updates
-- to the library. For more control, use the C-level bindings in Yoga.Bindings.
data Layout a
  = Root { _payload :: a,
           _children :: [Layout a],
           _rootPtr :: ForeignPtr C'YGNode }
  | Container { _payload :: a,
                _children :: [Layout a] }
  | Leaf { _payload :: a }
  deriving (Show, Eq, Ord)

withNativePtr :: Layout a -> (Ptr C'YGNode -> IO b) -> IO b
withNativePtr (Root _ _ ptr) f = withForeignPtr ptr f
withNativePtr _ _ = error "Internal: Only root nodes have pointers"

-- | Children are a list of layouts annotated with a style for how they should
-- be laid out in their container.
data Children a
  = StartToEnd [Layout a]
  | EndToStart [Layout a]
  | Centered [Layout a]
  | SpaceBetween [Layout a]
  | SpaceAround [Layout a]
  | Wrap (Children a)
  deriving (Show, Eq, Ord)

instance Functor Layout where
  fmap f (Root x cs ptr) = Root (f x) (fmap (fmap f) cs) ptr
  fmap f (Container x cs) = Container (f x) (fmap (fmap f) cs)
  fmap f (Leaf x) = Leaf (f x)

instance Foldable Layout where
  foldMap f (Root x cs _) = f x `mappend` (foldMap (foldMap f) cs)
  foldMap f (Container x cs) = f x `mappend` (foldMap (foldMap f) cs)
  foldMap f (Leaf x) = f x

  foldl f z (Root x cs _) = foldl (foldl f) (f z x) cs
  foldl f z (Container x cs) = foldl (foldl f) (f z x) cs
  foldl f z (Leaf x) = f z x

  foldr f z (Root x cs _) = f x $ foldr (flip $ foldr f) z cs
  foldr f z (Container x cs) = f x $ foldr (flip $ foldr f) z cs
  foldr f z (Leaf x) = f x z

instance Traversable Layout where
  traverse f (Root x cs ptr) =
    Root <$> f x <*> (sequenceA $ traverse f <$> cs) <*> pure ptr
  traverse f (Container x cs) =
    Container <$> f x <*> (sequenceA $ traverse f <$> cs)
  traverse f (Leaf x) = Leaf <$> f x

  sequenceA (Root x cs ptr) =
    Root <$> x <*> sequenceA (sequenceA <$> cs) <*> pure ptr
  sequenceA (Container x cs) =
    Container <$> x <*> sequenceA (sequenceA <$> cs)
  sequenceA (Leaf x) = Leaf <$> x

mkNode :: a -> IO (Layout a)
mkNode x = do
  ptr <- c'YGNodeNew
  Root x [] <$> newForeignPtr p'YGNodeFree ptr

-- | Collects a list of layouts and orients them from start to end depending
-- on the orientation of the parent (LTR vs RTL).
startToEnd :: [Layout a] -> Children a
startToEnd = StartToEnd

-- | Collects a list of layouts and orients them from end to start depending
-- on the orientation of the parent (LTR vs RTL).
endToStart :: [Layout a] -> Children a
endToStart = EndToStart

-- | Collects a list of children and centers them within the parent.
centered :: [Layout a] -> Children a
centered = Centered

-- | Collects a list of children that span the parent such that the space
-- between each child is equal
spaceBetween :: [Layout a] -> Children a
spaceBetween = SpaceBetween

-- | Collects a list of children that span the parent such that the space to the
-- left and right of each child is equal.
spaceAround :: [Layout a] -> Children a
spaceAround = SpaceAround

-- | Permits children to wrap to a new line if they exceed the bounds of their
-- parent
wrapped :: Children a -> Children a
wrapped (Wrap xs) = Wrap xs
wrapped childs = childs

justifiedContainer :: CUInt -> [Layout a] -> a -> IO (Layout a)
justifiedContainer just cs x = do
  ptr <- c'YGNodeNew
  c'YGNodeStyleSetJustifyContent ptr just
  c'YGNodeStyleSetFlexWrap ptr c'YGWrapNoWrap

  cs' <- flip mapM (zip cs [0..]) $ \((Root p children fptr), idx) -> do
    withForeignPtr fptr $ \oldptr -> do
      newptr <- c'YGNodeClone oldptr
      c'YGNodeInsertChild ptr newptr idx
      return $ if null children
               then Leaf p
               else Container p children
  Root x cs' <$> newForeignPtr p'YGNodeFreeRecursive ptr

assembleChildren :: Children a -> a -> IO (Layout a)
assembleChildren (StartToEnd cs) x = justifiedContainer c'YGJustifyFlexStart cs x
assembleChildren (EndToStart cs) x = justifiedContainer c'YGJustifyFlexEnd cs x
assembleChildren (Centered cs) x = justifiedContainer c'YGJustifyCenter cs x
assembleChildren (SpaceBetween cs) x =
  justifiedContainer c'YGJustifySpaceBetween cs x
assembleChildren (SpaceAround cs) x =
  justifiedContainer c'YGJustifySpaceAround cs x
assembleChildren (Wrap cs) x = assembleChildren cs x >>= wrapContainer
  where
    wrapContainer :: Layout a -> IO (Layout a)
    wrapContainer lyt = do
      withNativePtr lyt $ \ptr ->
        c'YGNodeStyleSetFlexWrap ptr c'YGWrapWrap
      return lyt

setContainerDirection :: CUInt -> CUInt -> Layout a -> IO ()
setContainerDirection dir flexDir lyt =
  withNativePtr lyt $ \ptr -> do
    c'YGNodeStyleSetDirection ptr dir
    c'YGNodeStyleSetFlexDirection ptr flexDir

-- | Generates a layout from a group of children and a payload such that the
-- children are laid out horizontally. The orientation (RTL vs LTR) is
-- inherited from the parent
hbox :: Children a -> a -> Layout a
hbox cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionInherit c'YGFlexDirectionRow node
  return node

-- | Generates a layout from a group of children and a payload such that the
-- children are laid out vertically. The orientation (top to bottom vs bottom to
-- top) is inherited from the parent.
vbox :: Children a -> a -> Layout a
vbox cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionInherit c'YGFlexDirectionColumn node
  return node

-- | Generates a layout from a group of children and a payload such that the
-- children are laid out horizontally from left to right.
hboxLeftToRight :: Children a -> a -> Layout a
hboxLeftToRight cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionLTR c'YGFlexDirectionRow node
  return node

-- | Generates a layout from a group of children and a payload such that the
-- children are laid out horizontally from right to left.
hboxRightToLeft :: Children a -> a -> Layout a
hboxRightToLeft cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionRTL c'YGFlexDirectionRow node
  return node

-- | Generates a layout from a group of children and a payload such that the
-- children are laid out vertically from top to bottom.
vboxTopToBottom :: Children a -> a -> Layout a
vboxTopToBottom cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionLTR c'YGFlexDirectionColumn node
  return node

-- | Generates a layout from a group of children and a payload such that the
-- children are laid out vertically from bottom to top.
vboxBottomToTop :: Children a -> a -> Layout a
vboxBottomToTop cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionRTL c'YGFlexDirectionColumn node
  return node

-- | A 'Size' is used to set properties about given layouts. In general, the
-- width and height of a node along with its position are laid out by Yoga's
-- internal layout engine. However, the user may decide to set limits on how
-- much internal nodes can shrink or grow. This datatype controls those
-- properties
data Size
  = Exact Float
  | Min Float
  | Max Float
  | Range Float Float
    deriving (Read, Show, Eq, Ord)

setWidth :: Size -> Layout a -> IO ()
setWidth (Exact w) lyt =
  withNativePtr lyt $ \ptr -> c'YGNodeStyleSetWidth ptr $ realToFrac w
setWidth (Min w) lyt =
  withNativePtr lyt $ \ptr -> c'YGNodeStyleSetMinWidth ptr $ realToFrac w
setWidth (Max w) lyt =
  withNativePtr lyt $ \ptr -> c'YGNodeStyleSetMaxWidth ptr $ realToFrac w
setWidth (Range minWidth maxWidth) lyt =
  withNativePtr lyt $ \ptr -> do
    c'YGNodeStyleSetMinWidth ptr $ realToFrac minWidth
    c'YGNodeStyleSetMaxWidth ptr $ realToFrac maxWidth

setHeight :: Size -> Layout a -> IO ()
setHeight (Exact h) lyt =
  withNativePtr lyt $ \ptr -> c'YGNodeStyleSetHeight ptr $ realToFrac h
setHeight (Min h) lyt =
  withNativePtr lyt $ \ptr -> c'YGNodeStyleSetMinHeight ptr $ realToFrac h
setHeight (Max h) lyt =
  withNativePtr lyt $ \ptr -> c'YGNodeStyleSetMaxHeight ptr $ realToFrac h
setHeight (Range minHeight maxHeight) lyt =
  withNativePtr lyt $ \ptr -> do
    c'YGNodeStyleSetMinHeight ptr $ realToFrac minHeight
    c'YGNodeStyleSetMaxHeight ptr $ realToFrac maxHeight

-- | Specifies layout may shrink up to the given size. The weight parameter
-- is used to determine how much this layout will shrink in relation to any
-- siblings.
shrinkable :: Float -> Size -> Size -> (b -> Layout a) -> b -> Layout a
shrinkable weight width height mkNodeFn x = unsafePerformIO $ do
  let n = mkNodeFn x
  setWidth width n
  setHeight height n
  withNativePtr n $ \ptr -> c'YGNodeStyleSetFlexShrink ptr $ realToFrac weight
  return n

-- | Specifies layout may grow up to the given size. The weight parameter
-- is used to determine how much this layout will grow in relation to any
-- siblings.
growable :: Float -> Size -> Size -> (b -> Layout a) -> b -> Layout a
growable weight width height mkNodeFn x = unsafePerformIO $ do
  let n = mkNodeFn x
  setWidth width n
  setHeight height n
  withNativePtr n $ \ptr -> c'YGNodeStyleSetFlexGrow ptr $ realToFrac weight
  return n

-- | Creates a layout with the exact width and height for the given payload.
exact :: Float -> Float -> a -> Layout a
exact width height x = unsafePerformIO $ do
  n <- mkNode x
  setWidth (Exact width) n
  setHeight (Exact height) n
  return n

-- | Specifies the exact dimensions expected for a layout. Can be used for
-- containers and such when there is not necessarily any rendering involved. 
withDimensions :: Float -> Float -> (a -> Layout b) -> a -> Layout b
withDimensions width height mkNodeFn x = unsafePerformIO $ do
  let n = mkNodeFn x
  setWidth (Exact width) n
  setHeight (Exact height) n
  return n

-- | Allows a container to stretch to fit its parent
stretched :: (b -> Layout a) -> b -> Layout a
stretched mkNodeFn x =
  let node = mkNodeFn x
  in unsafePerformIO $ do
    withNativePtr node $ \ptr -> c'YGNodeStyleSetAlignSelf ptr c'YGAlignStretch
    return node

-- | Edges are used to describe the direction from which we want to alter an
-- attribute of a node. They are currently only being used with 'withMargin' and
-- 'withPadding'.
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
  deriving (Eq, Ord, Bounded, Enum, Read, Show)

setMargin :: CUInt -> Float -> Layout a -> IO (Layout a)
setMargin edge px node = do
  withNativePtr node $ \ptr -> c'YGNodeStyleSetMargin ptr edge $ realToFrac px
  return node

-- | Transforms a layout generator to one which applies the given margin using
-- continuation passing style. In this way we maintain the const-ness of layout
-- nodes. E.g.:
--
-- > let lyt = ($ payload) (withMargin Edge'Left 10.0 $ exact 200.0 300.0)
withMargin :: Edge -> Float -> (b -> Layout a) -> b -> Layout a
withMargin Edge'Left px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeLeft px (mkNodeFn x)
withMargin Edge'Top px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeTop px (mkNodeFn x)
withMargin Edge'Right px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeRight px (mkNodeFn x)
withMargin Edge'Bottom px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeBottom px (mkNodeFn x)
withMargin Edge'Start px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeStart px (mkNodeFn x)
withMargin Edge'End px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeEnd px (mkNodeFn x)
withMargin Edge'Horizontal px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeHorizontal px (mkNodeFn x)
withMargin Edge'Vertical px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeVertical px (mkNodeFn x)
withMargin Edge'All px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeAll px (mkNodeFn x)

setPadding :: CUInt -> Float -> Layout a -> IO (Layout a)
setPadding edge px node = do
  withNativePtr node $ \ptr ->
    c'YGNodeStyleSetPadding ptr edge $ realToFrac px
  return node

-- | Transforms a layout generator to one which applies the given padding using
-- continuation passing style. In this way we maintain the const-ness of layout
-- nodes. E.g.:
--
-- > let lyt = ($ payload) (withPadding Edge'Left 10.0 $ exact 200.0 300.0)
withPadding :: Edge -> Float -> (b -> Layout a) -> b -> Layout a
withPadding Edge'Left px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeLeft px (mkNodeFn x)
withPadding Edge'Top px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeTop px (mkNodeFn x)
withPadding Edge'Right px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeRight px (mkNodeFn x)
withPadding Edge'Bottom px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeBottom px (mkNodeFn x)
withPadding Edge'Start px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeStart px (mkNodeFn x)
withPadding Edge'End px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeEnd px (mkNodeFn x)
withPadding Edge'Horizontal px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeHorizontal px (mkNodeFn x)
withPadding Edge'Vertical px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeVertical px (mkNodeFn x)
withPadding Edge'All px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeAll px (mkNodeFn x)

--------------------------------------------------------------------------------
-- Rendering

-- | Stores the calculated layout information for a given node. During
-- rendering, the rendering function will take the payload and layout info to
-- facilitate the renderer to do whatever it needs to with the given layout
-- calculations.
data LayoutInfo = LayoutInfo {
  nodeTop :: Float,     -- ^ The y-coordinate of this node
  nodeLeft :: Float,    -- ^ The x-coordinate of this node
  nodeWidth :: Float,   -- ^ The width of this node
  nodeHeight :: Float   -- ^ The height of this node
} deriving (Eq, Show)

emptyInfo :: LayoutInfo
emptyInfo = LayoutInfo 0 0 0 0

layoutWithParent :: LayoutInfo -> LayoutInfo -> LayoutInfo
layoutWithParent parent child =
  let (x, y) = (nodeLeft parent, nodeTop parent)
      (x', y') = (nodeLeft child, nodeTop child)
  in child { nodeLeft = x + x', nodeTop = y + y' }

-- | A 'RenderFn' takes a top-left position and a width and height of a node
-- with the given payload. The function is expected to perform some monadic
-- action in the 'Monad' m, and return a new payload of type b. This function is
-- called on each node in order during a call to render.
type RenderFn m a b = LayoutInfo -> a -> m b

calculateLayout :: Ptr C'YGNode -> IO ()
calculateLayout ptr =
  let n = (nan :: CFloat)
  in c'YGNodeStyleGetDirection ptr >>= c'YGNodeCalculateLayout ptr n n

layoutInfo :: Ptr C'YGNode -> IO LayoutInfo
layoutInfo ptr = do
  left <- realToFrac <$> c'YGNodeLayoutGetLeft ptr
  top <- realToFrac <$> c'YGNodeLayoutGetTop ptr
  width <- realToFrac <$> c'YGNodeLayoutGetWidth ptr
  height <- realToFrac <$> c'YGNodeLayoutGetHeight ptr
  return $ LayoutInfo top left width height

renderNodeWithChildren :: (Functor m, Applicative m, Monad m, Monoid b) =>
                          LayoutInfo -> a -> [Layout a] -> Ptr C'YGNode
                          -> RenderFn m a (b, c)
                          -> m (b, c, [Layout c])
renderNodeWithChildren parentInfo x children ptr f = do
  info <- return $ unsafePerformIO $ layoutInfo ptr
  let thisInfo = layoutWithParent parentInfo info
  (m, y) <- f thisInfo x
  cs <- flip mapM (zip children [0..]) $ \(child, childIdx) -> do
    childPtr <- return $ unsafePerformIO $ c'YGNodeGetChild ptr childIdx
    foldRenderTree thisInfo child childPtr f
  return (mappend m . foldr mappend mempty . map fst $ cs, y, map snd cs)

foldRenderTree :: (Functor m, Applicative m, Monad m, Monoid b) =>
                  LayoutInfo -> Layout a -> Ptr C'YGNode -> RenderFn m a (b, c) ->
                  m (b, Layout c)
foldRenderTree parentInfo (Root x children fptr) ptr f = do
  (result, y, cs) <- renderNodeWithChildren parentInfo x children ptr f
  return $ (result, Root y cs fptr)
foldRenderTree parentInfo (Container x children) ptr f = do
  (result, y, cs) <- renderNodeWithChildren parentInfo x children ptr f
  return $ (result, Container y cs)
foldRenderTree parentInfo (Leaf x) ptr f = do
  (result, y, cs) <- renderNodeWithChildren parentInfo x [] ptr f
  return $ cs `seq` (result, Leaf y)

-- | Renders a layout with the user-supplied function. For each return value
-- of type '(b, c)', we append the first result to the output of the previous
-- node. The second result is stored as the new payload for the given node.
-- Hence, the resulting monadic action produces a 'mappend'-ed set of 'b's and
-- a new layout with payloads of type 'c'.
foldRender :: (Functor m, Applicative m, Monad m, Monoid b) =>
              Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRender lyt@(Root _ _ fptr) f = do
  rootPtr <- return $ unsafePerformIO $ withForeignPtr fptr $ \ptr -> do
    calculateLayout ptr
    return ptr
  foldRenderTree emptyInfo lyt rootPtr f
foldRender _ _ = error "Internal: Rendering must be done from the root node"

-- | Renders a layout with the user-supplied function. The renderer traverses
-- the tree from root node to children and transforms each payload using the
-- user-supplied function.
render :: (Functor m, Applicative m, Monad m) =>
          Layout a -> RenderFn m a b -> m (Layout b)
render lyt f =
  let f' lytInfo x = ((,) ()) <$> f lytInfo x
  in snd <$> foldRender lyt f'
