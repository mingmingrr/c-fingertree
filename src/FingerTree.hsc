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

data FRefType = FTreeR | FNodeR | FDigitR
  deriving (Eq, Show, Enum, Bounded)

instance Storable FRefType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral (fromEnum val))

data FNode = FNode
  { fNodeRefs :: CSize
  , fNodeSize :: CSize
  , fNodeItems :: [Ptr FNode]
  }

instance Storable FNode where
  sizeOf _ = #{size FNode}
  alignment _ = #{alignment FNode}
  peek ptr = do
    fNodeRefs <- #{peek FNode, refs} ptr
    fNodeSize <- #{peek FNode, size} ptr
    fNodeItems <- peekArray 3 $ #{ptr FNode, items} ptr
    return FNode{..}
  poke ptr FNode{..} = do
    #{poke FNode, refs} ptr fNodeRefs
    #{poke FNode, size} ptr fNodeSize
    pokeArray (#{ptr FNode, items} ptr) fNodeItems

data FDigit = FDigit
  { fDigitRefs :: CSize
  , fDigitSize :: CSize
  , fDigitCount :: CChar
  , fDigitItems :: [Ptr FNode]
  }

instance Storable FDigit where
  sizeOf _ = #{size FDigit}
  alignment _ = #{alignment FDigit}
  peek ptr = do
    fDigitRefs  <- #{peek FDigit, refs}  ptr
    fDigitSize  <- #{peek FDigit, size}  ptr
    fDigitCount <- #{peek FDigit, count} ptr
    fDigitItems <- peekArray 3 $ #{ptr FDigit, items} ptr
    return FDigit{..}
  poke ptr FDigit{..} = do
    #{poke FDigit, refs}  ptr fDigitRefs
    #{poke FDigit, size}  ptr fDigitSize
    #{poke FDigit, count} ptr fDigitCount
    pokeArray (#{ptr FDigit, items} ptr) fDigitItems

data FTreeType = FEmptyT | FSingleT | FDeepT
  deriving (Eq, Show, Enum, Bounded)

instance Storable FTreeType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral (fromEnum val))

data FDeep = FDeep
  { fDeepSize   :: CSize
  , fDeepLeft   :: Ptr FDigit
  , fDeepMiddle :: Ptr FTree
  , fDeepRight  :: Ptr FDigit
  }

instance Storable FDeep where
  sizeOf _ = #{size FDeep}
  alignment _ = #{alignment FDeep}
  peek ptr = do
    fDeepSize   <- #{peek FDeep, size}   ptr
    fDeepLeft   <- #{peek FDeep, left}   ptr
    fDeepMiddle <- #{peek FDeep, middle} ptr
    fDeepRight  <- #{peek FDeep, right}  ptr
    return FDeep{..}
  poke ptr FDeep{..} = do
    #{poke FDeep, size}   ptr fDeepSize
    #{poke FDeep, left}   ptr fDeepLeft
    #{poke FDeep, middle} ptr fDeepMiddle
    #{poke FDeep, right}  ptr fDeepRight

data FTree = FTree
  { fTreeRefs :: CSize
  , fTreeType :: FTreeType
  , fTreeValue :: Ptr ()
  }

instance Storable FTree where
  sizeOf _ = #{size FTree}
  alignment _ = #{alignment FTree}
  peek ptr = do
    fTreeRefs  <- #{peek FTree, refs} ptr
    fTreeType  <- #{peek FTree, type} ptr
    fTreeValue <- #{peek FTree, single} ptr
    return FTree{..}
  poke ptr FTree{..} = do
    #{poke FTree, refs}   ptr fTreeRefs
    #{poke FTree, type}   ptr fTreeType
    #{poke FTree, single} ptr fTreeValue

data FIterType = FTreeI | FNodeI | FDigitI
  deriving (Eq, Show, Enum, Bounded)

instance Storable FIterType where
  sizeOf _ = sizeOf (undefined :: CInt)
  alignment _ = alignment (undefined :: CInt)
  peek ptr = toEnum . fromIntegral <$> peek (castPtr ptr :: Ptr CInt)
  poke ptr val = poke (castPtr ptr :: Ptr CInt) (fromIntegral (fromEnum val))

data FIterCons = FIterCons
  { fIterType  :: FIterType
  , fIterIndex :: CSize
  , fIterValue :: Ptr ()
  , fIterNext  :: Ptr FIterCons
  }

instance Storable FIterCons where
  sizeOf _ = #{size FIterCons}
  alignment _ = #{alignment FIterCons}
  peek ptr = do
    fIterType  <- #{peek FIterCons, type}  ptr
    fIterIndex <- #{peek FIterCons, index} ptr
    fIterValue <- #{peek FIterCons, node}  ptr
    fIterNext  <- #{peek FIterCons, next}  ptr
    return FIterCons{..}
  poke ptr FIterCons{..} = do
    #{poke FIterCons, type}  ptr fIterType
    #{poke FIterCons, index} ptr fIterIndex
    #{poke FIterCons, node}  ptr fIterValue
    #{poke FIterCons, next}  ptr fIterNext

newtype FIter = FIter { iterStack :: Ptr FIterCons } deriving Storable

data FView = FView
  { fViewItem :: Ptr ()
  , fViewTree :: Ptr FTree
  }

instance Storable FView where
  sizeOf _ = #{size FView}
  alignment _ = #{alignment FView}
  peek ptr = do
    fViewItem <- #{peek FView, item} ptr
    fViewTree <- #{peek FView, tree} ptr
    return FView{..}
  poke ptr FView{..} = do
    #{poke FView, item} ptr fViewItem
    #{poke FView, tree} ptr fViewTree

data FSplit = FSplit
  { fSplitLeft :: Ptr FTree
  , fSplitItem :: Ptr ()
  , fSplitRight :: Ptr FTree
  }

instance Storable FSplit where
  sizeOf _ = #{size FSplit}
  alignment _ = #{alignment FSplit}
  peek ptr = do
    fSplitLeft  <- #{peek FSplit, left}  ptr
    fSplitItem  <- #{peek FSplit, item}  ptr
    fSplitRight <- #{peek FSplit, right} ptr
    return FSplit{..}
  poke ptr FSplit{..} = do
    #{poke FSplit, left}  ptr fSplitLeft
    #{poke FSplit, node}  ptr fSplitItem
    #{poke FSplit, right} ptr fSplitRight

foreign import ccall unsafe "libfingertree.h refCountGet"
  refCount :: CInt -> IO CLong

foreign import ccall unsafe "libfingertree.h FTree_incRef"
  tree_incRef :: Ptr FTree -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FDigit_incRef"
  digit_incRef :: Ptr FDigit -> IO (Ptr FDigit)

foreign import ccall unsafe "libfingertree.h FNode_incRef"
  node_incRef :: Ptr FNode -> IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FIterCons_incRef"
  iterCons_incRef :: Ptr FIterCons -> IO (Ptr FIterCons)

foreign import ccall unsafe "libfingertree.h FNode_decRef"
  node_decRef :: Ptr FNode -> IO ()

foreign import ccall unsafe "libfingertree.h FDigit_decRef"
  digit_decRef :: Ptr FDigit -> IO ()

foreign import ccall unsafe "libfingertree.h FTree_decRef"
  tree_decRef :: Ptr FTree -> IO ()

foreign import ccall unsafe "libfingertree.h FIterCons_decRef"
  iterCons_decRef :: Ptr FIterCons -> IO ()

foreign import ccall unsafe "libfingertree.h FTree_alloc"
  tree_alloc :: IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FDeep_alloc"
  deep_alloc :: IO (Ptr FDeep)

foreign import ccall unsafe "libfingertree.h FDigit_alloc"
  digit_alloc :: IO (Ptr FDigit)

foreign import ccall unsafe "libfingertree.h FNode_alloc"
  node_alloc :: IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FIterCons_alloc"
  iterCons_alloc :: IO (Ptr FIterCons)

foreign import ccall unsafe "libfingertree.h FIter_alloc"
  iter_alloc :: IO (Ptr FIter)

foreign import ccall unsafe "libfingertree.h FEmpty_make"
  empty_make :: IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FSingle_make"
  single_make :: Ptr FNode -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FDeep_make"
  deep_make :: CSize -> Ptr FDigit -> Ptr FTree -> Ptr FDigit -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FDigit_make"
  digit_make :: CSize -> CSize -> Ptr FNode -> Ptr FNode
    -> Ptr FNode -> Ptr FNode -> IO (Ptr FDigit)

foreign import ccall unsafe "libfingertree.h FDigit_makeN"
  digit_makeN :: CSize -> CSize -> Ptr (Ptr FNode) -> IO (Ptr FDigit)

foreign import ccall unsafe "libfingertree.h FDigit_makeNS"
  digit_makeNS :: CSize -> Ptr (Ptr FNode) -> IO (Ptr FDigit)

foreign import ccall unsafe "libfingertree.h FDigit_fromNode"
  digit_fromNode :: Ptr FNode -> IO (Ptr FDigit)

foreign import ccall unsafe "libfingertree.h FNode_make"
  node_make :: CSize -> Ptr FNode -> Ptr FNode -> Ptr FNode -> IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FNode_makeS"
  node_makeS :: Ptr FNode -> Ptr FNode -> Ptr FNode -> IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FNode_makeNS"
  node_makeNS :: CSize -> Ptr (Ptr FNode) -> IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FNode_make1"
  node_make1 :: Ptr () -> IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FIterCons_make"
  iterCons_make :: CInt -> Ptr () -> Ptr FIterCons -> IO (Ptr FIterCons)

foreign import ccall unsafe "libfingertree.h FIter_replace"
  iter_replace :: Ptr FIter -> CInt -> Ptr () -> IO (Ptr FIter)

foreign import ccall unsafe "libfingertree.h FIter_make"
  iter_make :: Ptr FIterCons -> IO (Ptr FIter)

foreign import ccall unsafe "libfingertree.h FDigit_print"
  digit_print :: Ptr FDigit -> IO ()

foreign import ccall unsafe "libfingertree.h FNode_print"
  node_print :: Ptr FNode  -> IO ()

foreign import ccall unsafe "libfingertree.h FTree_print"
  tree_print :: Ptr FTree  -> IO ()

foreign import ccall unsafe "libfingertree.h FIter_print"
  iter_print :: Ptr FIter  -> IO ()

foreign import ccall unsafe "libfingertree.h FTree_empty"
  tree_empty :: Ptr FTree -> IO CBool

foreign import ccall unsafe "libfingertree.h FTree_size"
  tree_size :: Ptr FTree -> IO CSize

foreign import ccall unsafe "libfingertree.h FTree_appendLeft"
  tree_appendLeft :: Ptr FTree -> Ptr () -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FTree_appendRight"
  tree_appendRight :: Ptr FTree -> Ptr () -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FTree_viewLeftPtr"
  tree_viewLeft :: Ptr FTree -> IO (Ptr FView)

foreign import ccall unsafe "libfingertree.h FTree_viewRightPtr"
  tree_viewRight :: Ptr FTree -> IO (Ptr FView)

foreign import ccall unsafe "libfingertree.h FTree_fromArray"
  tree_fromArray :: CSize -> Ptr (Ptr ()) -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FIter_empty"
  iter_empty :: Ptr FIter -> IO CBool

foreign import ccall unsafe "libfingertree.h FIter_next"
  iter_next :: Ptr FIter -> IO (Ptr FNode)

foreign import ccall unsafe "libfingertree.h FIter_fromTree"
  iter_fromTree :: Ptr FTree -> IO (Ptr FIter)

foreign import ccall unsafe "libfingertree.h FTree_toArray"
  tree_toArray :: Ptr FTree -> IO (Ptr (Ptr ()))

foreign import ccall unsafe "libfingertree.h FTree_extend"
  tree_extend :: Ptr FTree -> Ptr FTree -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FTree_index"
  tree_index :: Ptr FTree -> CSize -> IO (Ptr ())

foreign import ccall unsafe "libfingertree.h FTree_update"
  tree_update :: Ptr FTree -> CSize -> Ptr () -> IO (Ptr FTree)

foreign import ccall unsafe "libfingertree.h FTree_splitAtPtr"
  tree_splitAt :: Ptr FTree -> CSize -> IO (Ptr FSplit)

foreign import ccall unsafe "libfingertree.h FTree_replicate"
  tree_replicate :: CSize -> Ptr () -> IO (Ptr FTree)

