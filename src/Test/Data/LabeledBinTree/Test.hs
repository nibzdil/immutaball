{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Data.LabeledBinTree.Test
	(
		main,
		testsMain,
		tests,

		simpleConstant
	) where

--import Control.Arrow
--import Data.Functor.Identity

--import Control.Lens
import Control.Monad
import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), assertBool)
import Test.Tasty.QuickCheck

import Data.LabeledBinTree
import Test.Data.LabeledBinTree.Orphans ()

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

simpleConstant :: Integer
simpleConstant = 3

tests :: TestTree
tests = testGroup "Data.LabeledBinTree" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3,

		-- I think it grows exponentionally (although I haven't rigorously
		-- worked out the tight big theta growth class for space if everything
		-- were strictly evaluated), so when testing for associativity, we'll
		-- want to limit tho size of trees.  e.g. I saw maxTreeSizeSum of 3*8
		-- (average size of 8) result in a tree size of 443,621.
		testGroup "testing monadic associativity of Tree" $
			let maxTreeSizeSum = 3*6 in  -- See above; probably exponential growth, and we're not traversing a single path but the entire tree.
			[
				testProperty "monadic associativity test of Tree 0" $
					\(int  :: Integer) ->
					\(fints :: LabeledBinTree Integer) ->
					\(gints :: LabeledBinTree Integer) ->
					\(hints :: LabeledBinTree Integer) ->
					(sum $ numAnyDirectLBTI <$> [fints, gints, hints]) <= maxTreeSizeSum ==>  -- This seems to stop what might be intermittent size explotions.
					let f = \y -> (\x -> x + y) <$> fints in
					let g = \y -> (\x -> x + y) <$> gints in
					let h = \y -> (\x -> x + y) <$> hints in
					( (f <=< g) <=< h ) int `eqLBT` ( f <=< (g <=< h) ) int,

				testProperty "monadic associativity test of Tree 0, but with strict equality" $
					\(int  :: Integer) ->
					\(fints :: LabeledBinTree Integer) ->
					\(gints :: LabeledBinTree Integer) ->
					\(hints :: LabeledBinTree Integer) ->
					(sum $ numAnyDirectLBTI <$> [fints, gints, hints]) <= maxTreeSizeSum ==>  -- This seems to stop what might be intermittent size explotions.
					let f = \y -> (\x -> x + y) <$> fints in
					let g = \y -> (\x -> x + y) <$> gints in
					let h = \y -> (\x -> x + y) <$> hints in
					( (f <=< g) <=< h ) int == ( f <=< (g <=< h) ) int
			]
	]
