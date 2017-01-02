module Yoga (
  -- Initialization
  Layout,

  -- Children layouts
  startToEnd, endToStart, centered, spaceBetween, spaceAround, wrapped,
  
  -- Containers
  hbox, vbox,
  hboxLeftToRight, hboxRightToLeft,
  vboxTopToBottom, vboxBottomToTop,

  -- Leaf nodes
  Size(..),
  shrinkable, growable, exact,

  -- Attributes
  Edge(..),
  stretched, marginSetWith, paddingSetWith,

  -- Rendering
  render, foldRender,

) where
--------------------------------------------------------------------------------
import Bindings.Yoga
import Bindings.Yoga.Enums

import Data.Foldable
import Data.Traversable()

import Foreign.C.Types (CFloat, CInt)
import Foreign.ForeignPtr

import GHC.Ptr (Ptr)

import Numeric.IEEE

import System.IO.Unsafe
--------------------------------------------------------------------------------

data Layout a
  = Root { _payload :: a,
           _children :: [Layout a],
           _rootPtr :: ForeignPtr C'YGNode }
  | Container { _payload :: a,
                _children :: [Layout a],
                _childPtr :: Ptr C'YGNode }
  | Leaf { _payload :: a,
           _childPtr :: Ptr C'YGNode }
  deriving (Show, Eq, Ord)

withNativePtr :: Layout a -> (Ptr C'YGNode -> IO b) -> IO b
withNativePtr (Root _ _ ptr) f = withForeignPtr ptr f
withNativePtr (Container _ _ ptr) f = f ptr
withNativePtr (Leaf _ ptr) f = f ptr

data Children a
  = StartToEnd [Layout a]
  | EndToStart [Layout a]
  | Centered [Layout a]
  | SpaceBetween [Layout a]
  | SpaceAround [Layout a]
  | Wrap (Children a)
  deriving (Show, Eq, Ord)

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
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

type RenderFn m a b = (Float, Float) -> (Float, Float) -> a -> m b

instance Functor Layout where
  fmap f (Root x cs ptr) = Root (f x) (fmap (fmap f) cs) ptr
  fmap f (Container x cs ptr) = Container (f x) (fmap (fmap f) cs) ptr
  fmap f (Leaf x ptr) = Leaf (f x) ptr

instance Foldable Layout where
  foldMap f (Root x cs _) = f x `mappend` (foldMap (foldMap f) cs)
  foldMap f (Container x cs _) = f x `mappend` (foldMap (foldMap f) cs)
  foldMap f (Leaf x _) = f x

  foldl f z (Root x cs _) = foldl (foldl f) (f z x) cs
  foldl f z (Container x cs _) = foldl (foldl f) (f z x) cs
  foldl f z (Leaf x _) = f z x

  foldr f z (Root x cs _) = f x $ foldr (flip $ foldr f) z cs
  foldr f z (Container x cs _) = f x $ foldr (flip $ foldr f) z cs
  foldr f z (Leaf x _) = f x z

instance Traversable Layout where
  traverse f (Root x cs ptr) =
    Root <$> f x <*> (sequenceA $ traverse f <$> cs) <*> pure ptr
  traverse f (Container x cs ptr) =
    Container <$> f x <*> (sequenceA $ traverse f <$> cs) <*> pure ptr
  traverse f (Leaf x ptr) = Leaf <$> f x <*> pure ptr

  sequenceA (Root x cs ptr) =
    Root <$> x <*> sequenceA (sequenceA <$> cs) <*> pure ptr
  sequenceA (Container x cs ptr) =
    Container <$> x <*> sequenceA (sequenceA <$> cs) <*> pure ptr
  sequenceA (Leaf x ptr) = Leaf <$> x <*> pure ptr

calculateLayout :: Ptr C'YGNode -> IO ()
calculateLayout ptr =
  let n = (nan :: CFloat)
  in c'YGNodeStyleGetDirection ptr >>= c'YGNodeCalculateLayout ptr n n

layoutBounds :: Ptr C'YGNode -> IO (Float, Float, Float, Float)
layoutBounds ptr = do
  left <- realToFrac <$> c'YGNodeLayoutGetLeft ptr
  top <- realToFrac <$> c'YGNodeLayoutGetTop ptr
  width <- realToFrac <$> c'YGNodeLayoutGetWidth ptr
  height <- realToFrac <$> c'YGNodeLayoutGetHeight ptr
  return (left, top, width, height)

renderTree :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
renderTree (Leaf x ptr) f = do
  (left, top, width, height) <- return $ unsafePerformIO $ layoutBounds ptr
  Leaf <$> f (left, top) (width, height) x <*> pure ptr
renderTree (Container x cs ptr) f = do
  (left, top, width, height) <- return $ unsafePerformIO $ layoutBounds ptr
  Container
    <$> f (left, top) (width, height) x
    <*> mapM (flip renderTree f) cs
    <*> pure ptr
renderTree (Root x cs ptr) f = do
  (left, top, width, height) <-
    return $ unsafePerformIO $ withForeignPtr ptr layoutBounds
  Root
    <$> f (left, top) (width, height) x
    <*> mapM (flip renderTree f) cs
    <*> pure ptr

render :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
render lyt f = do
  _ <- (unsafePerformIO $ withNativePtr lyt calculateLayout) `seq` return ()
  renderTree lyt f

foldRenderTree :: (Monad m, Monoid b) =>
                  Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRenderTree (Leaf x ptr) f = do
  (left, top, width, height) <- return $ unsafePerformIO $ layoutBounds ptr
  (m, y) <- f (left, top) (width, height) x
  return (mappend m mempty, Leaf y ptr)
foldRenderTree (Container x cs ptr) f = do
  (left, top, width, height) <- return $ unsafePerformIO $ layoutBounds ptr
  (m, y) <- f (left, top) (width, height) x
  cs' <- mapM (flip foldRenderTree f) cs
  return (mappend m . foldr mappend mempty . map fst $ cs',
          Container y (map snd cs') ptr)
foldRenderTree (Root x cs ptr) f = do
  (left, top, width, height) <-
    return $ unsafePerformIO $ withForeignPtr ptr layoutBounds
  (m, y) <- f (left, top) (width, height) x
  cs' <- mapM (flip foldRenderTree f) cs
  return (mappend m . foldr mappend mempty . map fst $ cs',
          Root y (map snd cs') ptr)

foldRender :: (Monad m, Monoid b) =>
              Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRender lyt f = do
  _ <- (unsafePerformIO $ withNativePtr lyt calculateLayout) `seq` return ()
  foldRenderTree lyt f

mkNode :: a -> IO (Layout a)
mkNode x = do
  ptr <- c'YGNodeNew
  Root x [] <$> newForeignPtr p'YGNodeFree ptr

startToEnd :: [Layout a] -> Children a
startToEnd = StartToEnd

endToStart :: [Layout a] -> Children a
endToStart = EndToStart

centered :: [Layout a] -> Children a
centered = Centered

spaceBetween :: [Layout a] -> Children a
spaceBetween = SpaceBetween

spaceAround :: [Layout a] -> Children a
spaceAround = SpaceAround

wrapped :: Children a -> Children a
wrapped (Wrap xs) = Wrap xs
wrapped childs = childs

justifiedContainer :: CInt -> [Layout a] -> a -> IO (Layout a)
justifiedContainer just cs x = do
  ptr <- c'YGNodeNew
  c'YGNodeStyleSetJustifyContent ptr just
  c'YGNodeStyleSetFlexWrap ptr c'YGWrapNoWrap

  cs' <- flip mapM (zip cs [0..]) $ \(child, idx) ->
    case child of
      (Root p [] fptr) -> withForeignPtr fptr $ \oldptr -> do
        newptr <- c'YGNodeNew
        c'YGNodeCopyStyle newptr oldptr
        c'YGNodeInsertChild ptr newptr idx
        return $ Leaf p newptr
      (Root p childs fptr) -> withForeignPtr fptr $ \oldptr -> do
        newptr <- c'YGNodeNew
        forM_ (zip childs [0..]) $ \(oldChild, oldChildIdx) -> do
          case oldChild of
            (Container _ _ oldChildPtr) -> do
              c'YGNodeRemoveChild oldptr oldChildPtr
              c'YGNodeInsertChild newptr oldChildPtr oldChildIdx
            (Leaf _ oldChildPtr) -> do
              c'YGNodeRemoveChild oldptr oldChildPtr
              c'YGNodeInsertChild newptr oldChildPtr oldChildIdx
            _ -> error "Removing native tree structure of root children!"
        c'YGNodeCopyStyle newptr oldptr
        c'YGNodeInsertChild ptr newptr idx
        return $ Container p childs newptr
      _ -> error "Adding non-root children to container!"
  Root x cs' <$> newForeignPtr p'YGNodeFreeRecursive ptr

assembleChildren :: Children a -> a -> IO (Layout a)
assembleChildren (StartToEnd cs) x = justifiedContainer c'YGJustifyFlexStart cs x
assembleChildren (EndToStart cs) x = justifiedContainer c'YGJustifyFlexEnd cs x
assembleChildren (Centered cs) x = justifiedContainer c'YGJustifyCenter cs x
assembleChildren (SpaceBetween cs) x = justifiedContainer c'YGJustifySpaceBetween cs x
assembleChildren (SpaceAround cs) x = justifiedContainer c'YGJustifySpaceAround cs x
assembleChildren (Wrap cs) x = assembleChildren cs x >>= wrapContainer
  where
    wrapContainer :: Layout a -> IO (Layout a)
    wrapContainer lyt = do
      withNativePtr lyt $ \ptr ->
        c'YGNodeStyleSetFlexWrap ptr c'YGWrapWrap
      return lyt

setContainerDirection :: CInt -> CInt -> Layout a -> IO ()
setContainerDirection dir flexDir lyt =
  withNativePtr lyt $ \ptr -> do
    c'YGNodeStyleSetDirection ptr dir
    c'YGNodeStyleSetFlexDirection ptr flexDir

hbox :: Children a -> a -> Layout a
hbox cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionInherit c'YGFlexDirectionRow node
  return node

vbox :: Children a -> a -> Layout a
vbox cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionInherit c'YGFlexDirectionColumn node
  return node

hboxLeftToRight :: Children a -> a -> Layout a
hboxLeftToRight cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionLTR c'YGFlexDirectionRow node
  return node

hboxRightToLeft :: Children a -> a -> Layout a
hboxRightToLeft cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionRTL c'YGFlexDirectionRow node
  return node

vboxTopToBottom :: Children a -> a -> Layout a
vboxTopToBottom cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionLTR c'YGFlexDirectionColumn node
  return node

vboxBottomToTop :: Children a -> a -> Layout a
vboxBottomToTop cs x = unsafePerformIO $ do
  node <- assembleChildren cs x
  setContainerDirection c'YGDirectionRTL c'YGFlexDirectionColumn node
  return node

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

shrinkable :: Float -> Size -> Size -> a -> Layout a
shrinkable weight width height x = unsafePerformIO $ do
  n <- mkNode x
  setWidth width n
  setHeight height n
  withNativePtr n $ \ptr -> c'YGNodeStyleSetFlexShrink ptr $ realToFrac weight
  return n

growable :: Float -> Size -> Size -> a -> Layout a
growable weight width height x = unsafePerformIO $ do
  n <- mkNode x
  setWidth width n
  setHeight height n
  withNativePtr n $ \ptr -> c'YGNodeStyleSetFlexGrow ptr $ realToFrac weight
  return n

exact :: Float -> Float -> a -> Layout a
exact width height x = unsafePerformIO $ do
  n <- mkNode x
  setWidth (Exact width) n
  setHeight (Exact height) n
  return n

stretched :: (a -> Layout a) -> a -> Layout a
stretched mkNodeFn x =
  let node = mkNodeFn x
  in unsafePerformIO $ do
    withNativePtr node $ \ptr -> c'YGNodeStyleSetAlignSelf ptr c'YGAlignStretch
    return node

setMargin :: CInt -> Float -> Layout a -> IO (Layout a)
setMargin edge px node = do
  withNativePtr node $ \ptr -> c'YGNodeStyleSetMargin ptr edge $ realToFrac px
  return node

marginSetWith :: Edge -> Float -> (a -> Layout a) -> a -> Layout a
marginSetWith Edge'Left px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeLeft px (mkNodeFn x)
marginSetWith Edge'Top px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeTop px (mkNodeFn x)
marginSetWith Edge'Right px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeRight px (mkNodeFn x)
marginSetWith Edge'Bottom px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeBottom px (mkNodeFn x)
marginSetWith Edge'Start px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeStart px (mkNodeFn x)
marginSetWith Edge'End px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeEnd px (mkNodeFn x)
marginSetWith Edge'Horizontal px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeHorizontal px (mkNodeFn x)
marginSetWith Edge'Vertical px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeVertical px (mkNodeFn x)
marginSetWith Edge'All px mkNodeFn x =
  unsafePerformIO $ setMargin c'YGEdgeAll px (mkNodeFn x)

setPadding :: CInt -> Float -> Layout a -> IO (Layout a)
setPadding edge px node = do
  withNativePtr node $ \ptr ->
    c'YGNodeStyleSetPadding ptr edge $ realToFrac px
  return node

paddingSetWith :: Edge -> Float -> (a -> Layout a) -> a -> Layout a
paddingSetWith Edge'Left px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeLeft px (mkNodeFn x)
paddingSetWith Edge'Top px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeTop px (mkNodeFn x)
paddingSetWith Edge'Right px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeRight px (mkNodeFn x)
paddingSetWith Edge'Bottom px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeBottom px (mkNodeFn x)
paddingSetWith Edge'Start px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeStart px (mkNodeFn x)
paddingSetWith Edge'End px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeEnd px (mkNodeFn x)
paddingSetWith Edge'Horizontal px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeHorizontal px (mkNodeFn x)
paddingSetWith Edge'Vertical px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeVertical px (mkNodeFn x)
paddingSetWith Edge'All px mkNodeFn x =
  unsafePerformIO $ setPadding c'YGEdgeAll px (mkNodeFn x)
