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

import Numeric.IEEE

import System.IO.Unsafe
--------------------------------------------------------------------------------

type NativeNode = ForeignPtr C'YGNode
data Layout a
  = Container { _payload :: a,
                _children :: [Layout a],
                internalPtr :: NativeNode }
  | Leaf { _payload :: a,
           internalPtr :: NativeNode }

data Children a
  = StartToEnd [Layout a]
  | EndToStart [Layout a]
  | Centered [Layout a]
  | SpaceBetween [Layout a]
  | SpaceAround [Layout a]
  | Wrap (Children a)

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
  fmap f (Container x cs ptr) = Container (f x) (fmap (fmap f) cs) ptr
  fmap f (Leaf x ptr) = Leaf (f x) ptr

instance Foldable Layout where
  foldMap f (Container x cs _) = f x `mappend` (foldMap (foldMap f) cs)
  foldMap f (Leaf x _) = f x

  foldl f z (Container x cs _) = foldl (foldl f) (f z x) cs
  foldl f z (Leaf x _) = f z x

  foldr f z (Container x cs _) = f x $ foldr (flip $ foldr f) z cs
  foldr f z (Leaf x _) = f x z

instance Traversable Layout where
  traverse f (Container x cs ptr) =
    Container <$> f x <*> (sequenceA $ traverse f <$> cs) <*> pure ptr
  traverse f (Leaf x ptr) = Leaf <$> f x <*> pure ptr

  sequenceA (Container x cs ptr) =
    Container <$> x <*> sequenceA (sequenceA <$> cs) <*> pure ptr
  sequenceA (Leaf x ptr) = Leaf <$> x <*> pure ptr

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
renderTree (Leaf x ptr) f = do
  (left, top, width, height) <- return $ layoutBounds ptr
  Leaf <$> f (left, top) (width, height) x <*> pure ptr
renderTree (Container x cs ptr) f = do
  (left, top, width, height) <- return $ layoutBounds ptr
  Container
    <$> f (left, top) (width, height) x
    <*> mapM (flip renderTree f) cs
    <*> pure ptr

render :: Monad m => Layout a -> RenderFn m a b -> m (Layout b)
render lyt f = do
  _ <- calculateLayout (internalPtr lyt)
  renderTree lyt f

foldRenderTree :: (Monad m, Monoid b) =>
                  Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRenderTree (Leaf x ptr) f = do
  (left, top, width, height) <- return $ layoutBounds ptr
  (m, y) <- f (left, top) (width, height) x
  return (mappend m mempty, Leaf y ptr)
foldRenderTree (Container x cs ptr) f = do
  (left, top, width, height) <- return $ layoutBounds ptr
  (m, y) <- f (left, top) (width, height) x
  cs' <- mapM (flip foldRenderTree f) cs
  return (mappend m . foldr mappend mempty . map fst $ cs',
          Container y (map snd cs') ptr)

foldRender :: (Monad m, Monoid b) =>
              Layout a -> RenderFn m a (b, c) -> m (b, Layout c)
foldRender lyt f = do
  _ <- calculateLayout (internalPtr lyt)
  foldRenderTree lyt f

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

  forM_ (zip cs [0..]) $ \(child, idx) ->
    withForeignPtr (internalPtr child) $ \childPtr ->
    c'YGNodeInsertChild ptr childPtr idx
  Container x cs <$> newForeignPtr p'YGNodeFree ptr

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
      withForeignPtr (internalPtr lyt) $ \ptr ->
        c'YGNodeStyleSetFlexWrap ptr c'YGWrapWrap
      return lyt

setContainerDirection :: CInt -> CInt -> Layout a -> IO ()
setContainerDirection dir flexDir lyt =
  withForeignPtr (internalPtr lyt) $ \ptr -> do
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

mkNode :: a -> IO (Layout a)
mkNode x = do
  ptr <- c'YGNodeNew
  Leaf x <$> newForeignPtr p'YGNodeFree ptr
  
setWidth :: Size -> Layout a -> IO ()
setWidth (Exact w) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr ->
    c'YGNodeStyleSetWidth ptr $ realToFrac w
setWidth (Min w) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr ->
    c'YGNodeStyleSetMinWidth ptr $ realToFrac w
setWidth (Max w) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr ->
    c'YGNodeStyleSetMaxWidth ptr $ realToFrac w
setWidth (Range minWidth maxWidth) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr -> do
    c'YGNodeStyleSetMinWidth ptr $ realToFrac minWidth
    c'YGNodeStyleSetMaxWidth ptr $ realToFrac maxWidth

setHeight :: Size -> Layout a -> IO ()
setHeight (Exact h) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr ->
    c'YGNodeStyleSetHeight ptr $ realToFrac h
setHeight (Min h) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr ->
    c'YGNodeStyleSetMinHeight ptr $ realToFrac h
setHeight (Max h) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr ->
    c'YGNodeStyleSetMaxHeight ptr $ realToFrac h
setHeight (Range minHeight maxHeight) lyt =
  withForeignPtr (internalPtr lyt) $ \ptr -> do
    c'YGNodeStyleSetMinHeight ptr $ realToFrac minHeight
    c'YGNodeStyleSetMaxHeight ptr $ realToFrac maxHeight

shrinkable :: Float -> Size -> Size -> a -> Layout a
shrinkable weight width height x = unsafePerformIO $ do
  n <- mkNode x
  setWidth width n
  setHeight height n
  withForeignPtr (internalPtr n) $ \ptr ->
    c'YGNodeStyleSetFlexShrink ptr $ realToFrac weight
  return n

growable :: Float -> Size -> Size -> a -> Layout a
growable weight width height x = unsafePerformIO $ do
  n <- mkNode x
  setWidth width n
  setHeight height n
  withForeignPtr (internalPtr n) $ \ptr ->
    c'YGNodeStyleSetFlexGrow ptr $ realToFrac weight
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
    withForeignPtr (internalPtr node) $ \ptr ->
      c'YGNodeStyleSetAlignSelf ptr c'YGAlignStretch
    return node

setMargin :: CInt -> Float -> Layout a -> IO (Layout a)
setMargin edge px node = do
  withForeignPtr (internalPtr node) $ \ptr ->
    c'YGNodeStyleSetMargin ptr edge $ realToFrac px
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
  withForeignPtr (internalPtr node) $ \ptr ->
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
