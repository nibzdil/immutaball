{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances, DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}  -- BinTree language extensions (TODO: move to new module)

module Immutaball.Share.Utils
	(
		Fixed(..), fixed,
		getFixed,
		cata,
		RCompose(..), rcompose,
		getRCompose,
		if',
		deconsBool,
		voidA,
		safeHead,
		safeTail,
		mfix',
		joinMaybeResult,
		chunksOfI,
		chunksOf,
		closeFirstO,
		closeSecondO,
		closeFirstI,
		closeSecondI,
		openFirstO,
		openSecondO,
		openFirstI,
		openSecondI,
		openFirstIO,
		openSecondIO,
		closeFirstIO,
		closeSecondIO,
		withOpenFirstIO,
		withOpenSecondIO,
		concatFirst,
		swap,
		split,
		trueAsIntegralI,
		falseAsIntegralI,
		trueAsIntegral,
		falseAsIntegral,
		deconsMaybe,

		morElse,

		--AssumeEOS,

		BinTree,
		BinTreeF(..),
		BinTreeLabeled,
		LabeledBinTree(..), labeledBinTree,
		Tree,
		deconsLabeledBinTree,
		mkLabeledEmpty,
		mkLabeledLeaf,
		mkLabeledFork,
		fmapLabeledBinTree,
		foldrLabeledBinTree,
		simplifyLeavesLabeledBinTree,
		simplifyEmptiesLabeledBinTree,
		normalizeLabeledBinTree,
		joinLabeledBinTree,
		singletonLabeledBin,
		repeatLabeledBinTree,
		pureLabeledBinTreeLossy,
		appLabeledBinTreeLossy,
		labeledBinTreeConcatRightmost,
		pureLabeledBinTreeCombinatorial,
		appLabeledBinTreeCombinatorial
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad.Fix
import Data.Functor.Compose
import Data.List
import Data.Maybe

import Control.Lens

-- | See F-algebras and catamorphisms for the idiom.
newtype Fixed f = Fixed {_fixed :: f (Fixed f)}
makeLenses ''Fixed

instance (Eq (f (Fixed f))) => Eq (Fixed f) where
	(Fixed a) == (Fixed b) = a == b
instance (Ord (f (Fixed f))) => Ord (Fixed f) where
	(Fixed a) <= (Fixed b) = a <= b
instance (Show (f (Fixed f))) => Show (Fixed f) where
	show (Fixed a) = show a

getFixed :: Fixed f -> f (Fixed f)
getFixed = (^.fixed)

cata :: (Functor f) => (f a -> a) -> (Fixed f -> a)
cata fAlgebra = fAlgebra . fmap (cata fAlgebra) . getFixed

newtype RCompose f g a = RCompose {_rcompose :: g (f a) }
	deriving (Eq, Ord, Show, Semigroup, Monoid, Enum, Read, Num, Fractional, Real, RealFrac, Bounded)
		via (g (f a))
	deriving (Functor, Applicative, Foldable, {-Traversable, -}Contravariant)
		via (Compose g f)
makeLenses ''RCompose

getRCompose :: RCompose f g a -> g (f a)
getRCompose = (^.rcompose)

if' :: Bool -> a -> a -> a
if' True then_ _  = then_
if' False _ else_ = else_

deconsBool :: r -> r -> Bool -> r
deconsBool withTrue _  (True)  = withTrue
deconsBool _ withFalse (False) = withFalse

voidA :: (Arrow a) => a b c -> a b ()
voidA f = f >>> arr (const ())

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

mfix' :: (Monad m) => (a -> m a) -> m a
--mfix' f = let ma = ma >>= f in ma
mfix' f = fix $ \me -> me >>= f

joinMaybeResult :: Maybe (a -> Maybe b) -> (a -> Maybe b)
joinMaybeResult mf = \a -> do
	f <- mf
	f a

chunksOfI :: Integer -> [a] -> [[a]]
chunksOfI = chunksOf

chunksOf :: (Integral i) => i -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take' n xs : chunksOf n (drop' n xs)
	where (take', drop') = (genericTake, genericDrop)

closeFirstO :: (Arrow a) => a b ((), c) -> a b c
closeFirstO = (>>> arr (\((), c) -> c))

closeSecondO :: (Arrow a) => a b (c, ()) -> a b c
closeSecondO = (>>> arr (\(c, ()) -> c))

closeFirstI :: (Arrow a) => a ((), b) c -> a b c
closeFirstI = (arr ((,) ()) >>>)

closeSecondI :: (Arrow a) => a (b, ()) c -> a b c
closeSecondI = (arr (flip (,) ()) >>>)

openFirstO :: (Arrow a) => a b c -> a b ((), c)
openFirstO = (>>> arr ((,) ()))

openSecondO :: (Arrow a) => a b c -> a b (c, ())
openSecondO = (>>> arr (flip (,) ()))

openFirstI :: (Arrow a) => a b c -> a ((), b) c
openFirstI = (arr (\((), b) -> b) >>>)

openSecondI :: (Arrow a) => a b c -> a (b, ()) c
openSecondI = (arr (\(b, ()) -> b) >>>)

openFirstIO :: (Arrow a) => a b c -> a ((), b) ((), c)
openFirstIO = openFirstO . openFirstI

openSecondIO :: (Arrow a) => a b c -> a (b, ()) (c, ())
openSecondIO = openSecondO . openSecondI

closeFirstIO :: (Arrow a) => a ((), b) ((), c) -> a b c
closeFirstIO = closeFirstI . closeFirstO

closeSecondIO :: (Arrow a) => a (b, ()) (c, ()) -> a b c
closeSecondIO = closeSecondI . closeSecondO

withOpenFirstIO :: (Arrow a) => (a ((), b0) ((), c0) -> a ((), b1) ((), c1)) -> (a b0 c0 -> a b1 c1)
withOpenFirstIO f = closeFirstIO . f . openFirstIO

withOpenSecondIO :: (Arrow a) => (a (b0, ()) (c0, ()) -> a (b1, ()) (c1, ())) -> (a b0 c0 -> a b1 c1)
withOpenSecondIO f = closeSecondIO . f . openSecondIO

concatFirst :: [([a], b)] -> [(a, b)]
concatFirst = concat . map (\(ys, b) -> map (\y -> (y, b)) ys)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

split :: [(a, b)] -> ([a], [b])
split xs = (map fst xs, map snd xs)

trueAsIntegralI :: Integer
trueAsIntegralI = 1

falseAsIntegralI :: Integer
falseAsIntegralI = 0

trueAsIntegral :: (Integral i) => i
trueAsIntegral = fromIntegral $ trueAsIntegralI

falseAsIntegral :: (Integral i) => i
falseAsIntegral = fromIntegral $ falseAsIntegralI

deconsMaybe :: r -> (a -> r) -> Maybe a -> r
deconsMaybe withNothing _ (Nothing) = withNothing
deconsMaybe _ withJust    (Just a)  = withJust a

-- | A convenient way to deconstruct a maybe with a default value.
morElse :: Maybe a -> a -> a
morElse = flip fromMaybe

{-
-- | Empty Eq, Ord, Show instance.
--
-- Useful to hide a component from a record that otherwise implements these
-- instances, e.g. a utility function like ‘spaLumpGetVertexAdjacents’ not
-- essential to the record.  Alternatively, these utility functions could be
-- moved from the record, so that they take an additional input argument of the
-- record.
type AssumeEOS a = a
instance Eq (AssumeEOS a) where _ == _ = True
instance Ord (AssumeEOS a) where _ <= _ = True
instance Show (AssumeEOS a) where show _ = "(AssumeEOS)"
-}

-- | TODO: move our bintree code to a new module.
type BinTree n l = Fixed (BinTreeF n l)
data BinTreeF n l me =
	-- | Empty node.
	  EmptyBT
	-- | Leaf node.
	| LeafBT l
	-- | Fork node.
	| ForkBT me n me

type BinTreeLabeled a = BinTree a a

newtype LabeledBinTree a = LabeledBinTree {_labeledBinTree :: BinTreeLabeled a}
makeLenses ''LabeledBinTree

type Tree = LabeledBinTree

deconsLabeledBinTree :: r -> (a -> r) -> (LabeledBinTree a -> a -> LabeledBinTree a -> r) -> LabeledBinTree a -> r
deconsLabeledBinTree withEmptyBT _          _          (LabeledBinTree (Fixed (EmptyBT     ))) = withEmptyBT
deconsLabeledBinTree _           withLeafBT _          (LabeledBinTree (Fixed (LeafBT a    ))) = withLeafBT a
deconsLabeledBinTree _           _          withForkBT (LabeledBinTree (Fixed (ForkBT l a r))) = withForkBT (LabeledBinTree l) a (LabeledBinTree r)

mkLabeledEmpty :: LabeledBinTree a
mkLabeledEmpty = LabeledBinTree . Fixed $ EmptyBT

mkLabeledLeaf :: a -> LabeledBinTree a
mkLabeledLeaf a = LabeledBinTree . Fixed $ LeafBT a

mkLabeledFork :: LabeledBinTree a -> a -> LabeledBinTree a -> LabeledBinTree a
mkLabeledFork l a r = LabeledBinTree . Fixed $ ForkBT (_labeledBinTree l) a (_labeledBinTree r)

fmapLabeledBinTree :: (a -> b) -> (LabeledBinTree a -> LabeledBinTree b)
fmapLabeledBinTree f = deconsLabeledBinTree (mkLabeledEmpty) (\a -> mkLabeledLeaf (f a)) (\l a r -> mkLabeledFork (fmapLabeledBinTree f l) (f a) (fmapLabeledBinTree f r))

foldrLabeledBinTree :: (a -> b -> b) -> b -> LabeledBinTree a -> b
foldrLabeledBinTree f z = deconsLabeledBinTree
	z
	(\a     -> f a z)
	(\l a r -> foldrLabeledBinTree f (f a (foldrLabeledBinTree f z r)) l)

-- | Replace all forks of empty nodes with leaf nodes.
--
-- We can do this because 'LabeledBinTree' requires leaf and fork node
-- element types to be qual.
simplifyLeavesLabeledBinTree :: LabeledBinTree a -> LabeledBinTree a
simplifyLeavesLabeledBinTree = deconsLabeledBinTree
	mkLabeledEmpty
	(\a     -> mkLabeledLeaf a)
	(\l a r -> deconsLabeledBinTree
		(deconsLabeledBinTree
			(mkLabeledLeaf a)  -- Replacement: both original left and right trees are empty.
			(\_ra         -> mkLabeledFork (simplifyLeavesLabeledBinTree l) a (simplifyLeavesLabeledBinTree r))
			(\_rl _ra _rr -> mkLabeledFork (simplifyLeavesLabeledBinTree l) a (simplifyLeavesLabeledBinTree r))
			r
		)
		(\_la         -> mkLabeledFork (simplifyLeavesLabeledBinTree l) a (simplifyLeavesLabeledBinTree r))
		(\_ll _la _lr -> mkLabeledFork (simplifyLeavesLabeledBinTree l) a (simplifyLeavesLabeledBinTree r))
		l
	)

-- | Simplify a LabeledBinTree tree by removing all Leave constructs, replacing
-- leaves with forks of empties.  The values of leaves is not lost; they are
-- simply reframed as forks of empties (with the value still present).
simplifyEmptiesLabeledBinTree :: LabeledBinTree a -> LabeledBinTree a
simplifyEmptiesLabeledBinTree = deconsLabeledBinTree
	mkLabeledEmpty
	(\a     -> mkLabeledFork mkLabeledEmpty a mkLabeledEmpty)
	(\l a r -> mkLabeledFork l              a r             )

-- | Normalize a LabeledBinTree with 'simplifyEmptiesLabeledBinTree',
-- replacing all leaves with forks of empties.
normalizeLabeledBinTree :: LabeledBinTree a -> LabeledBinTree a
normalizeLabeledBinTree = simplifyEmptiesLabeledBinTree

-- | Given normalized trees, replace each of the root tree's inner leaf nodes
-- with a fork node whose left branch is the likewise joined left tree and
-- whose right branch is the likewise joined right tree.  This is perhaps like
-- a non-determinism of paths to leaves, perhaps somewhat like list monad
-- non-determinism.
joinLabeledBinTree :: LabeledBinTree (LabeledBinTree a) -> LabeledBinTree a
joinLabeledBinTree = _

singletonLabeledBin :: a -> LabeledBinTree a
singletonLabeledBin x = mkLabeledLeaf x

repeatLabeledBinTree :: a -> LabeledBinTree a
repeatLabeledBinTree x = mkLabeledFork (repeatLabeledBinTree x) x (repeatLabeledBinTree x)

pureLabeledBinTreeLossy :: a -> LabeledBinTree a
pureLabeledBinTreeLossy x = repeatLabeledBinTree x

-- | Zip-like lossy Applicative for LabeledBinTree: component-wise.
appLabeledBinTreeLossy :: (LabeledBinTree (a -> b)) -> LabeledBinTree a -> LabeledBinTree b
appLabeledBinTreeLossy mf ma = deconsLabeledBinTree
	(mkLabeledEmpty)  -- Discard any remaining elements of ‘ma’.
	(\f ->
		deconsLabeledBinTree
			(mkLabeledEmpty)  -- Discard the remaining elements of ‘mf’.
			(\a     -> mkLabeledLeaf (f a))
			(\_ a _ -> mkLabeledLeaf (f a))  -- Discard the remaining elements of ‘ma’.
			ma
	)
	(\fl f fr ->
		deconsLabeledBinTree
			(mkLabeledEmpty)  -- Discard the remaining elements of ‘mf’.
			(\a       -> mkLabeledLeaf (f a))  -- Discard the remaining elements of ‘mf’.
			(\al a ar -> mkLabeledFork
				(appLabeledBinTreeLossy fl al)
				(f a)
				(appLabeledBinTreeLossy fr ar)
			)
			ma
	)
	mf

-- | Add another tree to the right-most leaf of this tree.
--
-- Keep following the right-most node until empty is found, and then replace it
-- with the sub-tree.
labeledBinTreeConcatRightmost :: LabeledBinTree a -> LabeledBinTree a -> LabeledBinTree a
labeledBinTreeConcatRightmost base additional = deconsLabeledBinTree
	additional
	(\a     -> mkLabeledFork mkLabeledEmpty a additional)
	(\l a r -> mkLabeledFork l              a (labeledBinTreeConcatRightmost r additional))
	base

pureLabeledBinTreeCombinatorial :: a -> LabeledBinTree a
pureLabeledBinTreeCombinatorial x = mkLabeledLeaf x

-- | List-like combinatorial Applicative for LabeledBinTree:
--
-- TODO:
-- A nested bintree is unfolded (joined) as follows: given a (nested) tree at a
-- node, the ou
-- root to non-fork nodes
appLabeledBinTreeCombinatorial :: (LabeledBinTree (a -> b)) -> LabeledBinTree a -> LabeledBinTree b
appLabeledBinTreeCombinatorial = _

instance Functor LabeledBinTree where
	fmap :: (a -> b) -> (LabeledBinTree a -> LabeledBinTree b)
	fmap = fmapLabeledBinTree

instance Foldable LabeledBinTree where
	foldr :: (a -> b -> b) -> b -> LabeledBinTree a -> b
	foldr = foldrLabeledBinTree

-- | The default Applicative instance of LabeledBinTree is 'appLabeledBinTreeCombinatorial'.
instance Applicative LabeledBinTree where
	pure :: a -> LabeledBinTree a
	pure = pureLabeledBinTreeCombinatorial
	(<*>) :: (LabeledBinTree (a -> b)) -> LabeledBinTree a -> LabeledBinTree b
	(<*>) = appLabeledBinTreeCombinatorial
