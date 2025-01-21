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
		zdouble,
		circle,
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

-- TODO: remove debugging when done.
import Debug.Trace as D------------------------------------------------------ TODO
import Text.Printf

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

zdouble :: Double
zdouble = 0.0

-- | Similar but to specialize to 'float' so that equivalence checks'
-- thresholds aren't too tight.
circle :: Float
circle = tau

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
				testGroup "randomly generated" $
					[
						testProperty "eqEachEach random vec2" $
							\(vals :: [Vec2 Double]) -> eqEachEach False eq2 vals,
						testProperty "eqEachEach random vec3" $
							\(vals :: [Vec3 Double]) -> eqEachEach False eq3 vals,
						testProperty "eqEachEach random vec4" $
							\(vals :: [Vec4 Double]) -> eqEachEach False eq4 vals,
						testProperty "eqEachEach random mat3" $
							\(vals :: [Mat3 Double]) -> eqEachEach False eqm3 vals,
						testProperty "eqEachEach random mat4" $
							\(vals :: [Mat4 Double]) -> eqEachEach False eqm4 vals
					],

				testGroup "fixed" $
					[
						testCase "eqEachEach fixed vec2" $
							eqEachEach True eq2 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed vec3" $
							eqEachEach True eq3 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed vec4" $
							eqEachEach True eq4 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed mat3" $
							eqEachEach True eqm3 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True,
						testCase "eqEachEach fixed mat4" $
							eqEachEach True eqm4 [pure zdouble, pure 2.0, pure 3.5, pure $ -1.0] @?= True
					]
			],

		testGroup "3D pointing orientation utils (aiming) - using floats so that equality checks' precisions aren't too tight" $
			[
				testCase "aim right 1/8th circle" $
					--D.trace (printf "DEBUG0: left is %s and right is %s." (show $ aimHoriz3DSimple (circle/8) (Vec3 0.0 1.0 0.0)) (show $ (Vec3 (sqrt 0.5) (sqrt 0.5) 0.0))) $
					aimHoriz3DSimple (circle/8) (Vec3 0.0 1.0 0.0) `eq3` (Vec3 (sqrt 0.5) (sqrt 0.5) 0.0) @?= True,
				testCase "aim right 30 degrees" $
					aimHoriz3DSimple (circle/12) (Vec3 0.0 1.0 0.0) `eq3` (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 0.0) @?= True,

				testCase "look 45 deg up from 30 deg right" $
					-- length of Vec2 x y should be equal to z here, so that it's a right angle.
					-- prenormalized:
					-- 	  (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) (sqrt $ x^2 + y^2))
					-- 	= (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) (sqrt $ 1/4 + 3/4))
					-- 	= (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 1.0)
					-- prenormalized^.r: sqrt (1/4 + 3/4 + 1^2) = sqrt (1 + 1) = sqrt 2
					-- normalized:
					-- 	  (Vec3 (1.0      / 2.0) (sqrt 3.0 / 2.0) 1.0             ) / sqrt 2
					-- 	= (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0))
					D.trace (printf "DEBUG0: left is %s and right is %s." (show $ aimVert3DSimple Nothing (circle/8) (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 0.0)) (show $ Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0  :: Float))) $
					aimVert3DSimple Nothing (circle/8) (Vec3 (1.0 / 2.0) (sqrt 3.0 / 2.0) 0.0) `eq3` (Vec3 (sqrt 2.0 / 4.0) (sqrt 1.5 / 2.0) (sqrt 2.0 / 2.0)) @?= True
			]
	]
