{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, InstanceSigs #-}

module Data.LabeledBinTree
	(
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
		forkinizeLeaves,
		forkinizeLeavesDirect,
		joinLabeledBinTree,
		joinDirectLabeledBinTree,
		singletonLabeledBin,
		repeatLabeledBinTree,
		pureLabeledBinTreeLossy,
		appLabeledBinTreeLossy,
		labeledBinTreeConcatRightmost,
		pureLabeledBinTreeCombinatorial,
		appLabeledBinTreeCombinatorial,
		mapLeavesDirectLabeledBinTree,
		mapLeavesLabeledBinTree,
		mapNonleavesLabeledBinTree,
		bindLabeledBinTree,
		traverseLBT,

		-- * Sizes
		numElemsLBT,
		numNodesWeightedDirectLBT,
		numNodesDirectLBT,
		numEmptyDirectLBT,
		numLeavesDirectLBT,
		numForkDirectLBT,
		numAnyDirectLBT,

		numElemsLBTI,
		numNodesWeightedDirectLBTI,
		numNodesDirectLBTI,
		numEmptyDirectLBTI,
		numLeavesDirectLBTI,
		numForkDirectLBTI,
		numAnyDirectLBTI,

		-- * Constructor aliases
		emptyLBT,
		leafLBT,
		forkLBT,

		-- * Equivalence relations
		eqLBT,
		leqLBT,

		-- * Utils
		showFs,
		showFs'
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))

import Control.Lens

import Immutaball.Share.Utils

type BinTree n l = Fixed (BinTreeF n l)
data BinTreeF n l me =
	-- | Empty node.
	  EmptyBT
	-- | Leaf node.
	| LeafBT l
	-- | Fork node.
	| ForkBT me n me
	deriving (Eq, Ord, Show)

type BinTreeLabeled a = BinTree a a

newtype LabeledBinTree a = LabeledBinTree {_labeledBinTree :: BinTreeLabeled a}
	deriving (Eq, Ord, Show)
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

--- | Normalize a LabeledBinTree with 'simplifyLeavesLabeledBinTree',
--- replacing all forks of empty nodes with leaf nodes.
normalizeLabeledBinTree :: LabeledBinTree a -> LabeledBinTree a
normalizeLabeledBinTree = simplifyLeavesLabeledBinTree

-- | Replace all leaf nodes with a fork node with the given left and right
-- branches.
--
-- Note: normalizing here doesn't change the result if we were to instead check
-- for leaf equivalence manually ourselves.
forkinizeLeaves :: LabeledBinTree a -> LabeledBinTree a -> LabeledBinTree a -> LabeledBinTree a
forkinizeLeaves ll base lr = forkinizeLeavesDirect ll (normalizeLabeledBinTree base) lr

-- | Assuming the base tree is already normalized, replace all leaf nodes in
-- the base tree with a fork node with the given left and right branches.
forkinizeLeavesDirect :: LabeledBinTree a -> LabeledBinTree a -> LabeledBinTree a -> LabeledBinTree a
forkinizeLeavesDirect ll base lr = deconsLabeledBinTree
	mkLabeledEmpty
	(\a     -> mkLabeledFork ll a lr)
	(\l a r -> mkLabeledFork (forkinizeLeavesDirect ll l lr) a (forkinizeLeavesDirect ll r lr))
	base

-- | Replace each of the root tree's inner leaf nodes with a fork node whose
-- left branch is the likewise joined outer left tree and whose right branch is
-- the likewise joined outer right tree.  This is perhaps like a
-- non-determinism of paths to leaves, perhaps somewhat like list monad
-- non-determinism.
joinLabeledBinTree :: LabeledBinTree (LabeledBinTree a) -> LabeledBinTree a
joinLabeledBinTree = joinDirectLabeledBinTree . normalizeLabeledBinTree . (normalizeLabeledBinTree <$>)

-- | 'joinLabeledBinTree' that assumes the tree and all nested trees have
-- already been normalized.
joinDirectLabeledBinTree :: LabeledBinTree (LabeledBinTree a) -> LabeledBinTree a
joinDirectLabeledBinTree = deconsLabeledBinTree
	mkLabeledEmpty
	(\a -> a)
	(\l a r -> forkinizeLeavesDirect (joinDirectLabeledBinTree l) a (joinDirectLabeledBinTree r))

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

-- | List-like combinatorial Applicative for LabeledBinTree, with a focus on
-- leaves.
--
-- Note: since ‘m >>= f = join (f <$> m)’
-- and ‘m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))’,
-- then applying a binary tree of integer unaops (e.g. plus 1, times 2, squared
-- plus 3, etc.) to a binary tree of integers, would mean ‘m1 <*> m2’ is:
-- map, for each binop: map, for each integer: singleton of the binop applied
-- to the integer; then join; then join.  Before the first join, we have a
-- binary tree of a binary tree of a binary tree: a binary tree of, for each of
-- the unaops, a binary tree of the integers, except the integers are singleton
-- applications of the function to the integer.  Joining a binary tree of
-- singletons returns the same binary tree without the singleton wrappers (join
-- composed with pure), so after the first, inner join, we then have: a binary
-- tree of, for each of the unaops, a binary tree of the original integers each
-- applied to that unaop.  (e.g. copy the integer tree for each node in the
-- unaop tree, and apply that unop.)  We now have a binary tree of binary
-- trees, where each node in the outer tree represents which unary operation /
-- function we applied to the inner list to get the inner tree output for that
-- unaop.
--
-- Finally, given this binary tree of outputs for each unaop, we perform
-- another join: take the root binary tree, representing, say, the mapping of
-- the ‘plus1’ root unaop to the input integers, and then for each leaf, turn
-- it into a fork node where the left gets you a continued pattern for the
-- root's left unaops tree, and likewise for right.
--
-- So the result is you start with the root node function applied to the
-- argument tree, but once you reach any leaf, you can keep descending either
-- left or right (possibly a branch can be Empty), and repeat the original
-- argument/input tree except for different unaops.  e.g. go to a leaf for the
-- root unaop, then go left, go to a would-be leaf, then go left again, and
-- then you start the tree for the unaop at the unaop tree's root's left's left
-- node's function.
appLabeledBinTreeCombinatorial :: LabeledBinTree (a -> b) -> LabeledBinTree a -> LabeledBinTree b
appLabeledBinTreeCombinatorial mf ma = joinLabeledBinTree ((\f -> (\a -> f a) <$> ma) <$> mf)

-- | Map leaf nodes, only checking directly for leaves.  Normalizing the tree
-- can yield different results.  To also map nodes that would be leaves when
-- normalized, use 'mapLeavesLabeledBinTree'.
mapLeavesDirectLabeledBinTree :: (a -> a) -> LabeledBinTree a -> LabeledBinTree a
mapLeavesDirectLabeledBinTree f = deconsLabeledBinTree
	mkLabeledEmpty
	(\a -> mkLabeledLeaf (f a))
	(\l a r -> mkLabeledFork (mapLeavesDirectLabeledBinTree f l) a (mapLeavesDirectLabeledBinTree f r))

-- | 'mapLeavesDirectLabeledBinTree' but treats forks of empty nodes as leaves.
mapLeavesLabeledBinTree :: (a -> a) -> LabeledBinTree a -> LabeledBinTree a
mapLeavesLabeledBinTree f = deconsLabeledBinTree
	mkLabeledEmpty
	(\a -> mkLabeledLeaf (f a))
	--(\l a r -> mkLabeledFork (mapLeavesDirectLabeledBinTree l) a (mapLeavesDirectLabeledBinTree r))
	(\l a r -> deconsLabeledBinTree
		(deconsLabeledBinTree
			(                mkLabeledFork (mapLeavesDirectLabeledBinTree f l) (f a) (mapLeavesDirectLabeledBinTree f r))
			(\_ra         -> mkLabeledFork (mapLeavesDirectLabeledBinTree f l) a     (mapLeavesDirectLabeledBinTree f r))
			(\_rl _ra _rr -> mkLabeledFork (mapLeavesDirectLabeledBinTree f l) a     (mapLeavesDirectLabeledBinTree f r))
			r
		)
		(\_la         -> mkLabeledFork (mapLeavesDirectLabeledBinTree f l) a (mapLeavesDirectLabeledBinTree f r))
		(\_ll _la _lr -> mkLabeledFork (mapLeavesDirectLabeledBinTree f l) a (mapLeavesDirectLabeledBinTree f r))
		l
	)

-- | Map each element, skipping leaves.
mapNonleavesLabeledBinTree :: (a -> a) -> LabeledBinTree a -> LabeledBinTree a
mapNonleavesLabeledBinTree f = deconsLabeledBinTree
	mkLabeledEmpty
	mkLabeledLeaf
	(\l a r -> mkLabeledFork (mapNonleavesLabeledBinTree f l) (f a) (mapNonleavesLabeledBinTree f r))

-- | For each element in the (left) tree, replace with the tree produced by
-- applying the function to that element, where each leaf becomes a fork node
-- with its left and right children having likewise binding.
--
-- See 'joinLabeledBinTree' for more information.
bindLabeledBinTree :: LabeledBinTree a -> (a -> LabeledBinTree b) -> LabeledBinTree b
bindLabeledBinTree ma f = joinLabeledBinTree (f <$> ma)

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
	(<*>) :: LabeledBinTree (a -> b) -> LabeledBinTree a -> LabeledBinTree b
	(<*>) = appLabeledBinTreeCombinatorial

-- | The default Monad instance of LabeledBinTree uses 'joinLabeledBinTree'
--
-- If my hand-wavy analysis is correct, joining trees strictly would have
-- exponentially growing space costs, so it may rarely be useful to use this
-- if evaluating the entire tree, rather than only down a specific or select
-- set of paths through the tree.
instance Monad LabeledBinTree where
	return :: a -> LabeledBinTree a
	return = pure
	(>>=) :: LabeledBinTree a -> (a -> LabeledBinTree b) -> LabeledBinTree b
	(>>=) = bindLabeledBinTree

traverseLBT :: Applicative f => (a -> f b) -> LabeledBinTree a -> f (LabeledBinTree b)
traverseLBT f = deconsLabeledBinTree
	(          pure emptyLBT)
	(\a     -> pure leafLBT <*> f a)
	(\l a r -> pure forkLBT <*> traverseLBT f l <*> f a <*> traverseLBT f r)

instance Traversable LabeledBinTree where
	traverse :: Applicative f => (a -> f b) -> LabeledBinTree a -> f (LabeledBinTree b)
	traverse = traverseLBT

-- * Sizes

-- | The number of non-empty elements in an lbt.
numElemsLBT :: (Num i) => LabeledBinTree a -> i
numElemsLBT = foldr (\_x acc -> acc + 1) 0

-- | The number of nodes, empty, leaf, or forking.
--
-- Provide the size of empty, leaf, and forking nodes (e.g. 0, 1, 1) and find
-- the sum.
numNodesWeightedDirectLBT :: (Num i) => i -> i -> i -> LabeledBinTree a -> i
numNodesWeightedDirectLBT a b c tree0 = (\f -> f 0 [tree0]) . fix $ \count counter remaining -> case remaining of
	[] -> counter
	(tree:remaining') -> deconsLabeledBinTree
		(           count (counter + a) $     remaining')
		(\_a     -> count (counter + b) $     remaining')
		(\l _a r -> count (counter + c) $ l:r:remaining')
		tree

-- | The number of nodes, empty, leaf, or forking.
numNodesDirectLBT :: (Num i) => LabeledBinTree a -> i
numNodesDirectLBT = numNodesWeightedDirectLBT 1 1 1

-- | Count the number of empty nodes.
numEmptyDirectLBT :: (Num i) => LabeledBinTree a -> i
numEmptyDirectLBT = numNodesWeightedDirectLBT 1 0 0

-- | Count the number of leaf nodes.
numLeavesDirectLBT :: (Num i) => LabeledBinTree a -> i
numLeavesDirectLBT = numNodesWeightedDirectLBT 0 1 0

-- | Count the number of fork nodes (branching).
numForkDirectLBT :: (Num i) => LabeledBinTree a -> i
numForkDirectLBT = numNodesWeightedDirectLBT 0 0 1

-- | Count the number of nodes in the tree of any of the 3 kinds.
numAnyDirectLBT :: (Num i) => LabeledBinTree a -> i
numAnyDirectLBT = numNodesWeightedDirectLBT 1 1 1

-- | 'numElemsLBT' specialized to Integer.
numElemsLBTI :: LabeledBinTree a -> Integer
numElemsLBTI = numElemsLBT

-- | 'numNodesWeightedDirectLBT' specialized to Integer.
numNodesWeightedDirectLBTI :: Integer -> Integer -> Integer -> LabeledBinTree a -> Integer
numNodesWeightedDirectLBTI = numNodesWeightedDirectLBT

-- | 'numNodesDirectLBT' specialized to Integer.
numNodesDirectLBTI :: LabeledBinTree a -> Integer
numNodesDirectLBTI = numNodesDirectLBT

-- | 'numEmptyDirectLBT' specialized to Integer.
numEmptyDirectLBTI :: LabeledBinTree a -> Integer
numEmptyDirectLBTI = numEmptyDirectLBT

-- | 'numLeavesDirectLBT' specialized to Integer.
numLeavesDirectLBTI :: LabeledBinTree a -> Integer
numLeavesDirectLBTI = numLeavesDirectLBT

-- | 'numForkDirectLBT' specialized to Integer.
numForkDirectLBTI :: LabeledBinTree a -> Integer
numForkDirectLBTI = numForkDirectLBT

-- | 'numAnyDirectLBT' specialized to Integer.
numAnyDirectLBTI :: LabeledBinTree a -> Integer
numAnyDirectLBTI = numAnyDirectLBT

-- * Constructor aliases

-- | Shorter alias of 'mkLabeledEmpty'.
emptyLBT :: LabeledBinTree a
emptyLBT = mkLabeledEmpty

-- | Shorter alias of 'mkLabeledLeaf'.
leafLBT :: a -> LabeledBinTree a
leafLBT = mkLabeledLeaf

-- | Shorter alias of 'mkLabeledFork'.
forkLBT :: LabeledBinTree a -> a -> LabeledBinTree a -> LabeledBinTree a
forkLBT = mkLabeledFork

-- * Equivalence relations

-- | Equivalence after normalization.
eqLBT :: (Eq a) => LabeledBinTree a -> LabeledBinTree a -> Bool
eqLBT = (==) `on` normalizeLabeledBinTree

-- | Leq after normalization.
leqLBT :: (Ord a) => LabeledBinTree a -> LabeledBinTree a -> Bool
leqLBT = (<=) `on` normalizeLabeledBinTree

-- | fs-style show.
showFs :: (Show a) => LabeledBinTree a -> String
showFs = showFs' show

-- | fs-style show, with a custom Show instance of ‘a’; primitive but still
-- more readable than default Show.
--
-- This implementation is currently relatively inefficient, since there is a
-- lot of unneeded repeated concatenation.  Consider DLists if wanting to
-- improve.
showFs' :: (a -> String) -> LabeledBinTree a -> String
showFs' showA tree = (\f -> f "" tree) . fix $ \withPrefix prefix subtree -> deconsLabeledBinTree
	(prefix ++ "×\n")
	(\a -> prefix ++ (showA a) ++ "\n")
	(\l a r -> prefix ++ showA a ++ "\n" ++ withPrefix (prefix ++ "\t.") l ++ withPrefix (prefix ++ "\t·") r)
	subtree
