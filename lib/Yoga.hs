module Yoga (
  Node(..), newNode, resetNode
) where
--------------------------------------------------------------------------------
import Bindings.Yoga

import Foreign.ForeignPtr
--------------------------------------------------------------------------------

newtype Node = Node (ForeignPtr C'YGNode)

newNode :: IO Node
newNode = do
  ptr <- c'YGNodeNew
  Node <$> newForeignPtr p'YGNodeFree ptr

resetNode :: Node -> IO ()
resetNode (Node fptr) = withForeignPtr fptr c'YGNodeReset
