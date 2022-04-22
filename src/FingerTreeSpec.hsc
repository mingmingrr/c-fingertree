{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

#include "libfingertree.h"

module FingerTreeSpec where

-- {{{ import

import Debug.Trace

import qualified Data.Sequence as S
import qualified Data.Sequence.Internal as S

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Test.HUnit.Lang
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

instance CEquiv a => CEquiv (S.Elem a) where
  type instance Target (S.Elem a) = FNode
  toTarget (S.Elem x) = toTarget x >>= \ptr ->
    node_make 1 (castPtr ptr) nullPtr nullPtr
  toSource ptr = toSource (#{ptr FNode, items} ptr)

instance (CEquiv a, Target a ~ FNode) => CEquiv (S.Node a) where
  type instance Target (S.Node a) = FNode
  toTarget n = case n of
    S.Node2 n a b -> join $
      node_make (fromIntegral n) <$> toTarget a <*> toTarget b <*> pure nullPtr
    S.Node3 n a b c -> join $
      node_make (fromIntegral n) <$> toTarget a <*> toTarget b <*> toTarget c
  toSource ptr = peek ptr >>= \(FNode _ n xs) -> case xs of
    [a, b] -> S.Node2 (fromIntegral n) <$> toSource a <*> toSource b
    [a, b, c] -> S.Node3 (fromIntegral n) <$> toSource a <*> toSource b <*> toSource c

instance (S.Sized a, CEquiv a, Target a ~ FNode) => CEquiv (S.Digit a) where
  type instance Target (S.Digit a) = FDigit
  toTarget n = case n of
    S.One a -> join $ digit_make (fromIntegral (S.size n)) 1 <$>
      toTarget a <*> pure nullPtr <*> pure nullPtr <*> pure nullPtr
    S.Two a b -> join $ digit_make (fromIntegral (S.size n)) 2 <$>
      toTarget a <*> toTarget b <*> pure nullPtr <*> pure nullPtr
    S.Three a b c -> join $ digit_make (fromIntegral (S.size n)) 3 <$>
      toTarget a <*> toTarget b <*> toTarget c <*> pure nullPtr
    S.Four a b c d -> join $ digit_make (fromIntegral (S.size n)) 4 <$>
      toTarget a <*> toTarget b <*> toTarget c <*> toTarget d
  toSource ptr = peek ptr >>= \(FDigit _ _ _ xs) -> case xs of
    [a] -> S.One <$> toSource a
    [a, b] -> S.Two <$> toSource a <*> toSource b
    [a, b, c] -> S.Three <$> toSource a <*> toSource b <*> toSource c
    [a, b, c, d] -> S.Four <$> toSource a <*> toSource b <*> toSource c <*> toSource d

instance (S.Sized a, CEquiv a, Target a ~ FNode) => CEquiv (S.FingerTree a) where
  type instance Target (S.FingerTree a) = FTree
  toTarget S.EmptyT = empty_make
  toTarget (S.Single a) = toTarget a >>= single_make
  toTarget (S.Deep n l m r) = join $
    deep_make (fromIntegral n) <$> toTarget l <*> toTarget m <*> toTarget r
  toSource ptr = peek ptr >>= \(FTree _ typ val) -> case typ of
    FEmptyT -> pure S.EmptyT
    FSingleT -> S.Single <$> toSource (castPtr val :: Ptr FNode)
    FDeepT -> peek (castPtr val) >>= \(FDeep n l m r) ->
      S.Deep (fromIntegral n) <$> toSource l <*> toSource m <*> toSource r

-- }}}

-- {{{ Valid

class Valid a where
  valid :: a -> Bool

instance Valid (S.Elem a) where
  valid _ = True

instance Valid (S.Seq a) where
  valid (S.Seq xs) = valid xs

instance (S.Sized a, Valid a) => Valid (S.FingerTree a) where
  valid S.EmptyT = True
  valid (S.Single x) = valid x
  valid (S.Deep s pr m sf) =
    s == S.size pr + S.size m + S.size sf && valid pr && valid m && valid sf

instance (S.Sized a, Valid a) => Valid (S.Node a) where
  valid node = S.size node == sum (fmap S.size node) && all valid node

instance Valid a => Valid (S.Digit a) where
  valid = all valid

-- }}}

-- {{{ utils

withTree :: S.Seq Int -> (Ptr FTree -> IO a) -> IO a
withTree (S.Seq tree) = bracket (toTarget tree) tree_decRef

equalTree :: S.Seq Int -> Ptr FTree -> IO ()
equalTree seq ptr = do
  tree_size ptr >>= assertEqual "equal size" (length seq) . fromIntegral
  bracket (tree_toArray ptr) free $ \arr ->
    peekArray (length seq) (castPtr arr) >>= \(xs :: [CSize]) ->
      assertEqual "equal items" (toList seq) (map fromIntegral xs)

ptrToInt :: Ptr a -> Int
ptrToInt ptr = let IntPtr x = ptrToIntPtr ptr in x

intToPtr :: Int -> Ptr a
intToPtr = intPtrToPtr . IntPtr

freeView :: Ptr FView -> IO ()
freeView ptr = do
  FView{..} <- peek ptr
  when (fViewTree /= nullPtr) $ do
    tree_decRef fViewTree
    free ptr

freeSplit :: Ptr FSplit -> IO ()
freeSplit ptr = do
  FSplit{..} <- peek ptr
  tree_decRef fSplitLeft
  -- node_decRef fSplitNode
  tree_decRef fSplitRight
  free ptr

-- }}}

-- {{{ check

checkRefs :: IO a -> IO a
checkRefs m = foldr check m [minBound..maxBound] where
  check (r :: FRefType) m = do
    let r' = fromIntegral (fromEnum r)
    (x, a, y) <- (,,) <$> refCount r' <*> m <*> refCount r'
    assertEqual ("ref count " ++ show r) x y
    return a

checkView :: Maybe (Int, S.Seq Int) -> Ptr FView -> IO ()
checkView expect view = peek view >>= \FView{..} -> case expect of
  Nothing -> do
    assertEqual "view item" nullPtr fViewItem
    assertEqual "view tree" nullPtr fViewTree
  Just (x, xs) -> do
    assertEqual "view item" x (ptrToInt fViewItem)
    equalTree xs fViewTree

checkingTree :: Testable p
  => (S.Seq Int -> Ptr FTree -> IO p)
  -> S.Seq Int -> p
checkingTree f xs = unsafePerformIO . checkRefs . withTree xs $ f xs

checkingTree2 :: Testable p
  => (S.Seq Int -> Ptr FTree -> S.Seq Int -> Ptr FTree -> IO p)
  -> S.Seq (Int) -> S.Seq (Int) -> p
checkingTree2 f xs ys = unsafePerformIO . checkRefs
  . withTree xs $ \xt -> withTree ys $ \yt -> f xs xt ys yt

checkingIndex :: Testable p
  => (Int -> S.Seq Int -> Ptr FTree -> IO p)
  -> NonEmptyList Int -> Property {- Int -> p -}
checkingIndex f (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \n -> checkingTree (f n) (S.fromList xs)

-- }}}

instance Show a => Show (S.Elem a) where
  showsPrec p (S.Elem x) = showsPrec p x
deriving instance Show a => Show (S.Node a)
deriving instance Show a => Show (S.Digit a)
deriving instance Show a => Show (S.FingerTree a)

spec :: Spec
spec = describe "FingerTree" $ do
  prop "toArray" $ checkingTree equalTree
  prop "size" . checkingTree $ \xs ts ->
    tree_size ts >>= assertEqual "tree_size" (length xs) . fromIntegral
  prop "viewLeft" . checkingTree $ \xs ts ->
    bracket (tree_viewLeft ts) freeView $ \view ->
      case S.viewl xs of
        S.EmptyL -> checkView Nothing view
        z S.:< zs -> checkView (Just (z, zs)) view
  prop "viewLeft" . checkingTree $ \xs ts ->
    bracket (tree_viewRight ts) freeView $ \view ->
      case S.viewr xs of
        S.EmptyR -> checkView Nothing view
        zs S.:> z -> checkView (Just (z, zs)) view
  prop "appendLeft" $ \x -> checkingTree $ \xs ts ->
    bracket (tree_appendLeft ts (intToPtr x)) tree_decRef $ \tree' ->
      liftIO $ equalTree (x S.<| xs) tree'
  prop "appendRight" $ \x -> checkingTree $ \xs ts ->
    bracket (tree_appendRight ts (intToPtr x)) tree_decRef $ \tree' ->
      liftIO $ equalTree (xs S.|> x) tree'
  prop "iterNext" . checkingTree $ \xs ts ->
    bracket (iter_fromTree ts) free $ \iter -> do
      liftIO . forM_ (toList xs) $ \x -> do
        iter_empty iter >>= assertEqual "iter_empty" (CBool 0)
        iter_next iter >>= assertEqual "iter_next" x . ptrToInt
      liftIO $ iter_empty iter >>= assertEqual "iter_empty" (CBool 1)
  prop "extend" . checkingTree2 $ \xs tree1 ys tree2 ->
    bracket (tree_extend tree1 tree2) tree_decRef $ equalTree (xs <> ys)
  prop "index" . checkingIndex $ \n xs ts -> tree_index ts (fromIntegral n)
    >>= assertEqual "index" (xs `S.index` n) . ptrToInt
  prop "update" $ \v -> checkingIndex $ \n xs ts -> do
    let ts' = tree_update ts (fromIntegral n) (intToPtr v)
     in bracket ts' tree_decRef $ equalTree (S.update n v xs)
  prop "splitAt" . checkingIndex $ \n xs ts ->
    bracket (tree_splitAt ts (fromIntegral n)) freeSplit $ \split -> do
      let (ls, v S.:<| rs) = S.splitAt n xs
      FSplit{..} <- peek split
      assertEqual "value" v (ptrToInt fSplitItem)
      equalTree ls fSplitLeft >> equalTree rs fSplitRight
  prop "replicate" $ \(Positive n) x -> ioProperty . checkRefs $
    bracket (tree_replicate (fromIntegral n) (intToPtr x)) tree_decRef $
      equalTree (S.replicate n x)

main :: IO ()
main = HSpec.hspecWith HSpec.defaultConfig
  { HSpec.configQuickCheckMaxSuccess = Just 100
  } spec

-- vim: set foldlevel=0 foldmethod=marker :
