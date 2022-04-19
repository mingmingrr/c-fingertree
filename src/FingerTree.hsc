{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FingerTree where

#include "libfingertree.h"

import Foreign
import Foreign.C
import Foreign.C.Types

data Node = Node
  { nodeRefs :: CSize
  , nodeSize :: CSize
  , nodeItems :: [Ptr Node]
  }

instance Storable Node where
  sizeOf _ = #{size Node}
  alignment _ = #{alignment Node}
  peek ptr = do
    nodeRefs <- #{peek Node, refs} ptr
    nodeSize <- #{peek Node, size} ptr
    nodeItems <- peekArray 3 $ #{ptr Node, items} ptr
    return Node{..}
  poke ptr Node{..} = do
    #{poke Node, refs} ptr nodeRefs
    #{poke Node, size} ptr nodeSize
    pokeArray (#{ptr Node, items} ptr) nodeItems

data Digit = Digit
  { digitRefs :: CSize
  , digitSize :: CSize
  , digitCount :: CUInt
  , digitItems :: [Ptr Node]
  }

instance Storable Digit where
  sizeOf _ = #{size Digit}
  alignment _ = #{alignment Digit}
  peek ptr = do
    digitRefs  <- #{peek Digit, refs}  ptr
    digitSize  <- #{peek Digit, size}  ptr
    digitCount <- #{peek Digit, count} ptr
    digitItems <- peekArray 3 $ #{ptr Digit, items} ptr
    return Digit{..}
  poke ptr Digit{..} = do
    #{poke Digit, refs}  ptr digitRefs
    #{poke Digit, size}  ptr digitSize
    #{poke Digit, count} ptr digitCount
    pokeArray (#{ptr Digit, items} ptr) digitItems

data TreeType = EmptyT | SingleT | DeepT
  deriving (Eq, Show, Enum, Bounded)

instance Storable TreeType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral (fromEnum val))

data Deep = Deep
  { deepSize   :: CSize
  , deepLeft   :: Ptr Digit
  , deepMiddle :: Ptr Tree
  , deepRight  :: Ptr Digit
  }

instance Storable Deep where
  sizeOf _ = #{size Deep}
  alignment _ = #{alignment Deep}
  peek ptr = do
    deepSize   <- #{peek Deep, size}   ptr
    deepLeft   <- #{peek Deep, left}   ptr
    deepMiddle <- #{peek Deep, middle} ptr
    deepRight  <- #{peek Deep, right}  ptr
    return Deep{..}
  poke ptr Deep{..} = do
    #{poke Deep, size}   ptr deepSize
    #{poke Deep, left}   ptr deepLeft
    #{poke Deep, middle} ptr deepMiddle
    #{poke Deep, right}  ptr deepRight

data Tree = Tree
  { treeRefs :: CSize
  , treeType :: TreeType
  , treeValue :: Ptr ()
  }

instance Storable Tree where
  sizeOf _ = #{size Tree}
  alignment _ = #{alignment Tree}
  peek ptr = do
    treeRefs  <- #{peek Tree, refs} ptr
    treeType  <- #{peek Tree, type} ptr
    treeValue <- #{peek Tree, single} ptr
    return Tree{..}
  poke ptr Tree{..} = do
    #{poke Tree, refs}   ptr treeRefs
    #{poke Tree, type}   ptr treeType
    #{poke Tree, single} ptr treeValue

data IterType = TreeI | NodeI | DigitI
  deriving (Eq, Show, Enum, Bounded)

instance Storable IterType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral (fromEnum val))

data IterCons = IterCons
  { iterType  :: IterType
  , iterIndex :: CSize
  , iterValue :: Ptr ()
  , iterNext  :: Ptr IterCons
  }

instance Storable IterCons where
  sizeOf _ = #{size IterCons}
  alignment _ = #{alignment IterCons}
  peek ptr = do
    iterType  <- #{peek IterCons, type}  ptr
    iterIndex <- #{peek IterCons, index} ptr
    iterValue <- #{peek IterCons, node}  ptr
    iterNext  <- #{peek IterCons, next}  ptr
    return IterCons{..}
  poke ptr IterCons{..} = do
    #{poke IterCons, type}  ptr iterType
    #{poke IterCons, index} ptr iterIndex
    #{poke IterCons, node}  ptr iterValue
    #{poke IterCons, next}  ptr iterNext

newtype Iter = Iter { iterStack :: Ptr IterCons } deriving Storable

data View = View
  { viewItem :: Ptr ()
  , viewTree :: Ptr Tree
  }

instance Storable View where
  sizeOf _ = #{size View}
  alignment _ = #{alignment View}
  peek ptr = do
    viewItem <- #{peek View, item} ptr
    viewTree <- #{peek View, tree} ptr
    return View{..}
  poke ptr View{..} = do
    #{poke View, item} ptr viewItem
    #{poke View, tree} ptr viewTree

foreign import ccall unsafe "libfingertree.h refCountGet"
  refCount :: IO CLong

foreign import ccall unsafe "libfingertree.h Tree_incRef"
  tree_incRef :: Ptr Tree -> IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Digit_incRef"
  digit_incRef :: Ptr Digit -> IO (Ptr Digit)

foreign import ccall unsafe "libfingertree.h Node_incRef"
  node_incRef :: Ptr Node -> IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h IterCons_incRef"
  iterCons_incRef :: Ptr IterCons -> IO (Ptr IterCons)

foreign import ccall unsafe "libfingertree.h Node_decRef"
  node_decRef :: Ptr Node -> IO ()

foreign import ccall unsafe "libfingertree.h Digit_decRef"
  digit_decRef :: Ptr Digit -> IO ()

foreign import ccall unsafe "libfingertree.h Tree_decRef"
  tree_decRef :: Ptr Tree -> IO ()

foreign import ccall unsafe "libfingertree.h IterCons_decRef"
  iterCons_decRef :: Ptr IterCons -> IO ()

foreign import ccall unsafe "libfingertree.h Tree_alloc"
  tree_alloc :: IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Deep_alloc"
  deep_alloc :: IO (Ptr Deep)

foreign import ccall unsafe "libfingertree.h Digit_alloc"
  digit_alloc :: IO (Ptr Digit)

foreign import ccall unsafe "libfingertree.h Node_alloc"
  node_alloc :: IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h IterCons_alloc"
  iterCons_alloc :: IO (Ptr IterCons)

foreign import ccall unsafe "libfingertree.h Iter_alloc"
  iter_alloc :: IO (Ptr Iter)

foreign import ccall unsafe "libfingertree.h Empty_make"
  empty_make :: IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Single_make"
  single_make :: Ptr Node -> IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Deep_make"
  deep_make :: CSize -> Ptr Digit -> Ptr Tree -> Ptr Digit -> IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Digit_make"
  digit_make :: CSize -> CSize -> Ptr Node -> Ptr Node -> Ptr Node -> Ptr Node -> IO (Ptr Digit)

foreign import ccall unsafe "libfingertree.h Digit_makeN"
  digit_makeN :: CSize -> CSize -> Ptr (Ptr Node) -> IO (Ptr Digit)

foreign import ccall unsafe "libfingertree.h Digit_makeNS"
  digit_makeNS :: CSize -> Ptr (Ptr Node) -> IO (Ptr Digit)

foreign import ccall unsafe "libfingertree.h Digit_fromNode"
  digit_fromNode :: Ptr Node -> IO (Ptr Digit)

foreign import ccall unsafe "libfingertree.h Node_make"
  node_make :: CSize -> Ptr Node -> Ptr Node -> Ptr Node -> IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h Node_makeS"
  node_makeS :: Ptr Node -> Ptr Node -> Ptr Node -> IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h Node_makeNS"
  node_makeNS :: CSize -> Ptr (Ptr Node) -> IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h Node_make1"
  node_make1 :: Ptr () -> IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h IterCons_make"
  iterCons_make :: CInt -> Ptr () -> Ptr IterCons -> IO (Ptr IterCons)

foreign import ccall unsafe "libfingertree.h Iter_replace"
  iter_replace :: Ptr Iter -> CInt -> Ptr () -> IO (Ptr Iter)

foreign import ccall unsafe "libfingertree.h Iter_make"
  iter_make :: Ptr IterCons -> IO (Ptr Iter)

foreign import ccall unsafe "libfingertree.h Digit_print"
  digit_print :: Ptr Digit -> IO ()

foreign import ccall unsafe "libfingertree.h Node_print"
  node_print :: Ptr Node  -> IO ()

foreign import ccall unsafe "libfingertree.h Tree_print"
  tree_print :: Ptr Tree  -> IO ()

foreign import ccall unsafe "libfingertree.h Iter_print"
  iter_print :: Ptr Iter  -> IO ()

foreign import ccall unsafe "libfingertree.h Tree_empty"
  tree_empty :: Ptr Tree -> IO Bool

foreign import ccall unsafe "libfingertree.h Tree_size"
  tree_size :: Ptr Tree -> IO CSize

foreign import ccall unsafe "libfingertree.h Tree_appendLeft"
  tree_appendLeft :: Ptr Tree -> Ptr () -> IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Tree_appendRight"
  tree_appendRight :: Ptr Tree -> Ptr () -> IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Tree_viewLeftPtr"
  tree_viewLeft :: Ptr Tree -> IO (Ptr View)

foreign import ccall unsafe "libfingertree.h Tree_viewRightPtr"
  tree_viewRight :: Ptr Tree -> IO (Ptr View)

foreign import ccall unsafe "libfingertree.h Tree_fromArray"
  tree_fromArray :: CSize -> Ptr (Ptr ()) -> IO (Ptr Tree)

foreign import ccall unsafe "libfingertree.h Iter_empty"
  iter_empty :: Ptr Iter -> IO Bool

foreign import ccall unsafe "libfingertree.h Iter_next"
  iter_next :: Ptr Iter -> IO (Ptr Node)

foreign import ccall unsafe "libfingertree.h Iter_fromTree"
  iter_fromTree :: Ptr Tree -> IO (Ptr Iter)

foreign import ccall unsafe "libfingertree.h Tree_toArray"
  tree_toArray :: Ptr Tree -> IO (Ptr (Ptr ()))

