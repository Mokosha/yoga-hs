{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_HADDOCK hide #-}

--------------------------------------------------------------------------------

#include <Yoga.h>
#include <bindings.dsl.h>

--------------------------------------------------------------------------------

module Bindings.Yoga where

import Data.Data        (Data)
import Data.Typeable    (Typeable)
import Foreign.C.Types  (CFloat(..), CInt(..), CUInt(..))
import Foreign.Ptr      (FunPtr, Ptr, plusPtr)
import Foreign.Storable (Storable(..))

import Prelude (Eq, IO, Show)
import Prelude (($), return)
--------------------------------------------------------------------------------

#starttype struct YGSize
#field width       , CFloat
#field height      , CFloat
#stoptype
deriving instance Typeable C'YGSize

#opaque_t YGNode
deriving instance Typeable C'YGNode
deriving instance Data     C'YGNode

#callback_t YGMeasureFunc, Ptr <YGNode> -> CFloat -> CInt -> CFloat -> CInt -> IO (Ptr <YGSize>)

#callback_t YGPrintFunc, Ptr <YGNode> -> IO ()
{--
typedef int (*YGLogger)(YGLogLevel level, const char *format, va_list args);
WIN_EXPORT void YGSetLogger(YGLogger logger);
WIN_EXPORT void YGLog(YGLogLevel level, const char *message, ...);

typedef void *(*YGMalloc)(size_t size);
typedef void *(*YGCalloc)(size_t count, size_t size);
typedef void *(*YGRealloc)(void *ptr, size_t size);
typedef void (*YGFree)(void *ptr);
WIN_EXPORT void
YGSetMemoryFuncs(YGMalloc ygmalloc, YGCalloc yccalloc, YGRealloc ygrealloc, YGFree ygfree);
-}

#ccall YGNodeNew, IO (Ptr <YGNode>)
#ccall YGNodeFree, Ptr <YGNode> -> IO ()
#ccall YGNodeFreeRecursive, Ptr <YGNode> -> IO ()
#ccall YGNodeReset, Ptr <YGNode> -> IO ()
#ccall YGNodeGetInstanceCount, IO (CInt)

#ccall YGNodeInsertChild, Ptr <YGNode> -> Ptr <YGNode> -> CUInt -> IO ()
#ccall YGNodeRemoveChild, Ptr <YGNode> -> Ptr <YGNode> -> IO ()
#ccall YGNodeGetChild, Ptr <YGNode> -> CUInt -> IO (Ptr <YGNode>)

#ccall YGNodeCalculateLayout, Ptr <YGNode> -> CFloat -> CFloat -> CInt -> IO ()

-- Mark a node as dirty. Only valid for nodes with a custom measure function
-- set.
-- YG knows when to mark all other nodes as dirty but because nodes with
-- measure functions
-- depends on information not known to YG they must perform this dirty
-- marking manually.

#ccall YGNodeMarkDirty, Ptr <YGNode> -> IO ()
#ccall YGNodeIsDirty, Ptr <YGNode> -> IO CInt

#ccall YGNodePrint, Ptr <YGNode> -> CInt -> IO ()

#ccall YGNodeCanUseCachedMeasurement, CInt -> CFloat -> CInt -> CFloat -> CInt -> CFloat -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO CInt

#ccall YGNodeCopyStyle, Ptr <YGNode> -> Ptr <YGNode> -> IO ()

#ccall YGNodeSetContext, Ptr <YGNode> -> Ptr () -> IO ()
#ccall YGNodeGetContext, Ptr <YGNode> -> IO (Ptr ())

#ccall YGNodeSetMeasureFunc, Ptr <YGNode> -> Ptr <YGMeasureFunc> -> IO ()
#ccall YGNodeGetMeasureFunc, Ptr <YGNode> -> IO (Ptr <YGMeasureFunc>)

#ccall YGNodeSetPrintFunc, Ptr <YGNode> -> Ptr <YGPrintFunc> -> IO ()
#ccall YGNodeGetPrintFunc, Ptr <YGNode> -> IO (Ptr <YGPrintFunc>)

#ccall YGNodeSetHasNewLayout, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeGetHasNewLayout, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetDirection, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetDirection, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetFlexDirection, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetFlexDirection, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetJustifyContent, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetJustifyContent, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetAlignContent, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetAlignContent, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetAlignItems, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetAlignItems, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetAlignSelf, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetAlignSelf, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetPositionType, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetPositionType, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetFlexWrap, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetFlexWrap, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetOverflow, Ptr <YGNode> -> CInt -> IO ()
#ccall YGNodeStyleGetOverflow, Ptr <YGNode> -> IO CInt

#ccall YGNodeStyleSetFlex, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleSetFlexGrow, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetFlexGrow, Ptr <YGNode> -> IO CFloat
#ccall YGNodeStyleSetFlexShrink, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetFlexShrink, Ptr <YGNode> -> IO CFloat
#ccall YGNodeStyleSetFlexBasis, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetFlexBasis, Ptr <YGNode> -> IO CFloat

#ccall YGNodeStyleSetPosition, Ptr <YGNode> -> CInt -> CFloat -> IO ()
#ccall YGNodeStyleGetPosition, Ptr <YGNode> -> CInt -> IO CFloat

#ccall YGNodeStyleSetMargin, Ptr <YGNode> -> CInt -> CFloat -> IO ()
#ccall YGNodeStyleGetMargin, Ptr <YGNode> -> CInt -> IO CFloat

#ccall YGNodeStyleSetPadding, Ptr <YGNode> -> CInt -> CFloat -> IO ()
#ccall YGNodeStyleGetPadding, Ptr <YGNode> -> CInt -> IO CFloat

#ccall YGNodeStyleSetBorder, Ptr <YGNode> -> CInt -> CFloat -> IO ()
#ccall YGNodeStyleGetBorder, Ptr <YGNode> -> CInt -> IO CFloat

#ccall YGNodeStyleSetWidth, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetWidth, Ptr <YGNode> -> IO CFloat
#ccall YGNodeStyleSetHeight, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetHeight, Ptr <YGNode> -> IO CFloat

#ccall YGNodeStyleSetMinWidth, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetMinWidth, Ptr <YGNode> -> IO CFloat
#ccall YGNodeStyleSetMinHeight, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetMinHeight, Ptr <YGNode> -> IO CFloat

#ccall YGNodeStyleSetMaxWidth, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetMaxWidth, Ptr <YGNode> -> IO CFloat
#ccall YGNodeStyleSetMaxHeight, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetMaxHeight, Ptr <YGNode> -> IO CFloat

-- Yoga specific properties, not compatible with flexbox specification
-- Aspect ratio control the size of the undefined dimension of a node.
-- - On a node with a set width/height aspect ratio control the size of the unset dimension
-- - On a node with a set flex basis aspect ratio controls the size of the node in the cross axis if
-- unset
-- - On a node with a measure function aspect ratio works as though the measure function measures
-- the flex basis
-- - On a node with flex grow/shrink aspect ratio controls the size of the node in the cross axis if
-- unset
-- - Aspect ratio takes min/max dimensions into account
#ccall YGNodeStyleSetAspectRatio, Ptr <YGNode> -> CFloat -> IO ()
#ccall YGNodeStyleGetAspectRatio, Ptr <YGNode> -> IO CFloat

#ccall YGNodeLayoutGetLeft, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetTop, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetRight, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetBottom, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetWidth, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetHeight, Ptr <YGNode> -> IO CFloat
#ccall YGNodeLayoutGetDirection, Ptr <YGNode> -> IO CInt
