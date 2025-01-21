{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, ScopedTypeVariables #-}

module Test.Immutaball.Share.Math.Test
	(
		main,
		testsMain,
		tests,

		simpleConstant,
		right3,
		forward3,
		up3,
		eq3Duplicate,
		zint,
		eqEachEach
	) where

--import Control.Arrow
--import Data.Functor.Identity

import Control.Lens
import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), assertBool)
import Test.Tasty.QuickCheck

import Immutaball.Share.Math
import Test.Immutaball.Share.Math.Orphans ()

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

simpleConstant :: Integer
simpleConstant = 3

right3 :: Vec3 Double
right3 = Vec3 1.0 0.0 0.0

forward3 :: Vec3 Double
forward3 = Vec3 0.0 1.0 0.0

up3 :: Vec3 Double
up3 = Vec3 0.0 0.0 1.0

eq3Duplicate :: Vec3 Double -> Vec3 Double -> Bool
eq3Duplicate a b = abs ((b - a)^.r3) <= smalld

-- | Helps resolve default type to Integer warnings when using ‘0’.
zint :: Integer
zint = 0

-- | Given a list of values possibly known to already be unique, return a bool
-- that is True if the equality comparison on each possible pairing of values
-- is as expected.
--
-- If e.g. the values are randomly generated, they might not be unique, so only
-- check each value with each other in this case.
eqEachEach :: Bool -> (a -> a -> Bool) -> [a] -> Bool
eqEachEach guaranteedUnique eq vals =
	and $
		[ r
		| ivals <- return $ zip [zint..] vals
		, (ai, av) <- ivals
		, (bi, bv) <- ivals
		, guaranteedUnique || ai == bi
		, r <- return $ (ai == bi) == (av `eq` bv)
		]

tests :: TestTree
tests = testGroup "Immutaball.Share.Math" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3,

		testGroup "meta tests" $
			[
				testCase "right3 == right3" $
					right3 `eq3Duplicate` right3 @?= True,
				testCase "right3 /= forward3" $
					right3 `eq3Duplicate` forward3 @?= False,
				testCase "each axis unit vector equality checks correctly with each" $
					and [r | axes <- return $ zip [zint..] [right3, forward3, up3], (ai, av) <- axes, (bi, bv) <- axes, r <- return $ (ai == bi) == (av `eq3Duplicate` bv)] @?= True,
				testCase "each axis unit vector equality checks correctly with each" $
					eqEachEach True eq3Duplicate [right3, forward3, up3] @?= True
			],

		testGroup "eq3 tests" $
			[
				testCase "right3 == right3" $
					right3 `eq3` right3 @?= True,
				testCase "right3 /= forward3" $
					right3 `eq3` forward3 @?= False,
				testCase "each axis unit vector equality checks correctly with each" $
					and [r | axes <- return $ zip [zint..] [right3, forward3, up3], (ai, av) <- axes, (bi, bv) <- axes, r <- return $ (ai == bi) == (av `eq3` bv)] @?= True,
				testCase "each axis unit vector equality checks correctly with each" $
					eqEachEach True eq3 [right3, forward3, up3] @?= True
			],

		testGroup "eqEachEach for vecs and mats" $
			[
				testProperty "eqEachEach random vec2" $
					\(vals :: [Vec2 Double]) -> eqEachEach False eq2 vals
			],

		testGroup "3D pointing orientation utils (aiming)" $
			[
			]
	]
