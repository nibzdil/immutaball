{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Orphans.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Data.LabeledBinTree.Orphans
	(
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))

import Test.QuickCheck

import Data.LabeledBinTree
import Immutaball.Share.Utils

instance (Arbitrary a) => Arbitrary (LabeledBinTree a) where
	-- | Styled as recommended in the QuickCheck manual.
	arbitrary = sized . fix $ \tree -> \size -> let subtree = tree (size `div` 2) in
		if' (size <= 0) (pure emptyLBT) .
		if' (size == 1) (leafLBT <$> arbitrary) .
		oneof $ [
			pure emptyLBT,
			leafLBT <$> arbitrary,
			forkLBT <$> subtree <*> arbitrary <*> subtree
		]
	shrink = deconsLabeledBinTree
		[]
		(\_a    -> [emptyLBT])
		(\l a r -> concat
			[
				[emptyLBT],
				[l, r],
				[forkLBT l' a' r' | (l', a', r') <- shrink (l, a, r)]
			]
		)
