{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

#include "libfingertree.h"

module FingerTreeSpec where

-- {{{ import

import Debug.Trace

import qualified Data.Sequence as Seq
import qualified Data.Sequence.Internal as Seq

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import qualified Test.Hspec.Core.Runner as HSpec

import Text.Pretty.Simple

import System.IO.Unsafe

import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Class
import Control.Exception

import Data.Kind
import Data.Functor
import Data.Foldable

import FingerTree

-- }}}

-- {{{ CEquiv

class CEquiv a where
  type family Target (a :: Type) :: Type
  toTarget :: a -> IO (Ptr (Target a))
  toSource :: Ptr (Target a) -> IO a

instance CEquiv Int where
  type instance Target Int = CSize
  toTarget = pure . intToPtr
  toSource = pure . ptrToInt

instance CEquiv a => CEquiv (Seq.Elem a) where
  type instance Target (Seq.Elem a) = Node
  toTarget (Seq.Elem x) = toTarget x >>= \ptr ->
    node_make 1 (castPtr ptr) nullPtr nullPtr
  toSource ptr = toSource (#{ptr Node, items} ptr)

instance (CEquiv a, Target a ~ Node) => CEquiv (Seq.Node a) where
  type instance Target (Seq.Node a) = Node
  toTarget n = case n of
    Seq.Node2 n a b -> join $
      node_make (fromIntegral n) <$> toTarget a <*> toTarget b <*> pure nullPtr
    Seq.Node3 n a b c -> join $
      node_make (fromIntegral n) <$> toTarget a <*> toTarget b <*> toTarget c
  toSource ptr = peek ptr >>= \(Node _ n xs) -> case xs of
    [a, b] -> Seq.Node2 (fromIntegral n) <$> toSource a <*> toSource b
    [a, b, c] -> Seq.Node3 (fromIntegral n) <$> toSource a <*> toSource b <*> toSource c

instance (Seq.Sized a, CEquiv a, Target a ~ Node) => CEquiv (Seq.Digit a) where
  type instance Target (Seq.Digit a) = Digit
  toTarget n = case n of
    Seq.One a -> join $ digit_make (fromIntegral (Seq.size n)) 1 <$>
      toTarget a <*> pure nullPtr <*> pure nullPtr <*> pure nullPtr
    Seq.Two a b -> join $ digit_make (fromIntegral (Seq.size n)) 2 <$>
      toTarget a <*> toTarget b <*> pure nullPtr <*> pure nullPtr
    Seq.Three a b c -> join $ digit_make (fromIntegral (Seq.size n)) 3 <$>
      toTarget a <*> toTarget b <*> toTarget c <*> pure nullPtr
    Seq.Four a b c d -> join $ digit_make (fromIntegral (Seq.size n)) 4 <$>
      toTarget a <*> toTarget b <*> toTarget c <*> toTarget d
  toSource ptr = peek ptr >>= \(Digit _ _ _ xs) -> case xs of
    [a] -> Seq.One <$> toSource a
    [a, b] -> Seq.Two <$> toSource a <*> toSource b
    [a, b, c] -> Seq.Three <$> toSource a <*> toSource b <*> toSource c
    [a, b, c, d] -> Seq.Four <$> toSource a <*> toSource b <*> toSource c <*> toSource d

instance (Seq.Sized a, CEquiv a, Target a ~ Node) => CEquiv (Seq.FingerTree a) where
  type instance Target (Seq.FingerTree a) = Tree
  toTarget Seq.EmptyT = empty_make
  toTarget (Seq.Single a) = toTarget a >>= single_make
  toTarget (Seq.Deep n l m r) = join $
    deep_make (fromIntegral n) <$> toTarget l <*> toTarget m <*> toTarget r
  toSource ptr = peek ptr >>= \(Tree _ typ val) -> case typ of
    EmptyT -> pure Seq.EmptyT
    SingleT -> Seq.Single <$> toSource (castPtr val :: Ptr Node)
    DeepT -> peek (castPtr val) >>= \(Deep n l m r) ->
      Seq.Deep (fromIntegral n) <$> toSource l <*> toSource m <*> toSource r

-- }}}

-- {{{ Valid

class Valid a where
  valid :: a -> Bool

instance Valid (Seq.Elem a) where
  valid _ = True

instance Valid (Seq.Seq a) where
  valid (Seq.Seq xs) = valid xs

instance (Seq.Sized a, Valid a) => Valid (Seq.FingerTree a) where
  valid Seq.EmptyT = True
  valid (Seq.Single x) = valid x
  valid (Seq.Deep s pr m sf) =
    s == Seq.size pr + Seq.size m + Seq.size sf && valid pr && valid m && valid sf

instance (Seq.Sized a, Valid a) => Valid (Seq.Node a) where
  valid node = Seq.size node == sum (fmap Seq.size node) && all valid node

instance Valid a => Valid (Seq.Digit a) where
  valid = all valid

-- }}}

withTree :: Seq.Seq Int -> (Ptr Tree -> IO a) -> IO a
withTree (Seq.Seq tree) = bracket (toTarget tree) tree_decRef

equalTree :: Seq.Seq Int -> Ptr Tree -> IO Bool
equalTree seq ptr = evalContT . callCC $ \cont -> do
  liftIO (tree_size ptr) >>= \size ->
    when (fromIntegral size /= length seq) (cont False)
  arr <- ContT $ bracket (tree_toArray ptr) free
  (xs :: [CSize]) <- liftIO $ peekArray (length seq) (castPtr arr)
  return $ toList seq == map fromIntegral xs

ptrToInt :: Ptr a -> Int
ptrToInt ptr = let IntPtr x = ptrToIntPtr ptr in x

intToPtr :: Int -> Ptr a
intToPtr = intPtrToPtr . IntPtr

freeView :: Ptr View -> IO ()
freeView ptr = do
  View{..} <- peek ptr
  when (viewTree /= nullPtr) $ do
    tree_decRef viewTree
    free ptr

checkRefs :: IO Bool -> IO Bool
checkRefs m = refCount >>= \r -> m >>= \res ->
  if res then (== r) <$> refCount else pure False

spec :: Spec
spec = describe "FingerTree" $ do
  prop "toArray" $ \(fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . withTree xs $ equalTree xs 
  prop "size" $ \(fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . withTree xs $ \tree ->
      (== length xs) . fromIntegral <$> tree_size tree
  prop "viewLeft" $ \(fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . evalContT $ do
      tree <- ContT $ withTree xs
      View y ys <- (liftIO . peek =<<)
        . ContT $ bracket (tree_viewLeft tree) freeView
      case Seq.viewl xs of
        Seq.EmptyL -> return $ y == nullPtr && ys == nullPtr
        z Seq.:< zs -> liftIO $ (z == ptrToInt y &&) <$> equalTree zs ys
  prop "viewRight" $ \(fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . evalContT $ do
      tree <- ContT $ withTree xs
      View y ys <- (liftIO . peek =<<)
        . ContT $ bracket (tree_viewRight tree) freeView
      case Seq.viewr xs of
        Seq.EmptyR -> return $ y == nullPtr && ys == nullPtr
        zs Seq.:> z -> liftIO $ (z == ptrToInt y &&) <$> equalTree zs ys
  prop "appendLeft" $ \(Positive x) (fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . evalContT $ do
      tree <- ContT $ withTree xs
      tree' <- ContT $ bracket (tree_appendLeft tree (intToPtr x)) tree_decRef
      liftIO $ equalTree (x Seq.<| xs) tree'
  prop "appendRight" $ \(Positive x) (fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . evalContT $ do
      tree <- ContT $ withTree xs
      tree' <- ContT $ bracket (tree_appendRight tree (intToPtr x)) tree_decRef
      liftIO $ equalTree (xs Seq.|> x) tree'
  prop "iter" $ \(fmap getPositive -> xs) ->
    unsafePerformIO . checkRefs . evalContT . callCC $ \cont -> do
      tree <- ContT $ withTree xs
      iter <- ContT $ bracket (iter_fromTree tree) free
      let run [] = iter_empty iter
          run (x:xs) = evalContT . callCC $ \cont -> do
            liftIO (iter_empty iter) >>= \empty -> when empty (cont False)
            liftIO (iter_next iter) >>= \item -> when (ptrToInt item /= x) (cont False)
            liftIO (run xs)
      liftIO $ run (toList xs)

main :: IO ()
main = HSpec.hspecWith HSpec.defaultConfig
  { HSpec.configQuickCheckMaxSuccess = Just 100
  } spec

-- vim: set foldlevel=0 foldmethod=marker :
