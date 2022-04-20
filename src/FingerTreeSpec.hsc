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

equalTree :: Seq.Seq Int -> Ptr Tree -> IO ()
equalTree seq ptr = do
  tree_size ptr >>= assertEqual "equal size" (length seq) . fromIntegral
  bracket (tree_toArray ptr) free $ \arr ->
    peekArray (length seq) (castPtr arr) >>= \(xs :: [CSize]) ->
      assertEqual "equal items" (toList seq) (map fromIntegral xs)

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

checkRefs :: IO a -> IO a
checkRefs m = do
  x <- refCount
  m' <- m
  y <- refCount
  assertEqual "ref count" x y
  return m'

checkView :: Maybe (Int, Seq.Seq Int) -> Ptr View -> IO ()
checkView expect view = peek view >>= \View{..} -> case expect of
  Nothing -> do
    assertEqual "view item" nullPtr viewItem
    assertEqual "view tree" nullPtr viewTree
  Just (x, xs) -> do
    assertEqual "view item" x (ptrToInt viewItem)
    equalTree xs viewTree

checkingTree :: Testable p
  => (Seq.Seq Int -> Ptr Tree -> IO p)
  -> Seq.Seq (Positive Int) -> p
checkingTree f xs = unsafePerformIO . checkRefs . withTree xs' $ f xs'
  where xs' = fmap getPositive xs

checkingTree2 :: Testable p
  => (Seq.Seq Int -> Ptr Tree -> Seq.Seq Int -> Ptr Tree -> IO p)
  -> Seq.Seq (Positive Int) -> Seq.Seq (Positive Int) -> p
checkingTree2 f xs ys = unsafePerformIO . checkRefs
  . withTree xs' $ \xt -> withTree ys' $ \yt -> f xs' xt ys' yt
  where xs' = fmap getPositive xs ; ys' = fmap getPositive ys

checkingIndex :: Testable p
  => (Int -> Seq.Seq Int -> Ptr Tree -> IO p)
  -> NonEmptyList (Positive Int) -> Property {- Int -> p -}
checkingIndex f (NonEmpty xs) = forAll (choose (0, length xs - 1)) $
  \n -> checkingTree (f n) (Seq.fromList xs)

spec :: Spec
spec = describe "FingerTree" $ do
  -- specify "splitAt" $ do
  prop "toArray" $ checkingTree equalTree
  prop "size" . checkingTree $ \xs ts ->
    tree_size ts >>= assertEqual "tree_size" (length xs) . fromIntegral
  prop "viewLeft" . checkingTree $ \xs ts ->
    bracket (tree_viewLeft ts) freeView $ \view ->
      case Seq.viewl xs of
        Seq.EmptyL -> checkView Nothing view
        z Seq.:< zs -> checkView (Just (z, zs)) view
  prop "viewLeft" . checkingTree $ \xs ts ->
    bracket (tree_viewRight ts) freeView $ \view ->
      case Seq.viewr xs of
        Seq.EmptyR -> checkView Nothing view
        zs Seq.:> z -> checkView (Just (z, zs)) view
  prop "appendLeft" $ \(Positive x) -> checkingTree $ \xs ts ->
    bracket (tree_appendLeft ts (intToPtr x)) tree_decRef $ \tree' ->
      liftIO $ equalTree (x Seq.<| xs) tree'
  prop "appendRight" $ \(Positive x) -> checkingTree $ \xs ts ->
    bracket (tree_appendRight ts (intToPtr x)) tree_decRef $ \tree' ->
      liftIO $ equalTree (xs Seq.|> x) tree'
  prop "iter" . checkingTree $ \xs ts ->
    bracket (iter_fromTree ts) free $ \iter -> do
      liftIO . forM_ (toList xs) $ \x -> do
        iter_empty iter >>= assertEqual "iter_empty" False
        iter_next iter >>= assertEqual "iter_next" x . ptrToInt
      liftIO $ iter_empty iter >>= assertEqual "iter_empty" True
  prop "extend" . checkingTree2 $ \xs tree1 ys tree2 ->
    bracket (tree_extend tree1 tree2) tree_decRef $ equalTree (xs <> ys)
  prop "index" . checkingIndex $ \n xs ts -> tree_index ts (fromIntegral n)
    >>= assertEqual "index" (xs `Seq.index` n) . ptrToInt
  prop "update" $ \(Positive v) -> checkingIndex $ \n xs ts -> do
    let ts' = tree_update ts (fromIntegral n) (intToPtr v)
     in bracket ts' tree_decRef $ equalTree (Seq.update n v xs)

main :: IO ()
main = HSpec.hspecWith HSpec.defaultConfig
  { HSpec.configQuickCheckMaxSuccess = Just 100
  } spec

-- vim: set foldlevel=0 foldmethod=marker :
