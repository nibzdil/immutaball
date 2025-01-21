{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Test.Immutaball.Share.Math.Test
	(
		main,
		testsMain,
		tests,

		simpleConstant,
		right3,
		forward3,
		up3,
		eq3
	) where

--import Control.Arrow
--import Data.Functor.Identity

import Control.Lens
import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), assertBool)
--import Test.Tasty.QuickCheck

import Immutaball.Share.Math

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

eq3 :: Vec3 Double -> Vec3 Double -> Bool
eq3 a b = abs ((b - a)^.r3) <= smalld

tests :: TestTree
tests = testGroup "Immutaball.Share.Math" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3,

		testGroup "meta tests" $
			[
				testCase "right3 == right3" $
					right3 `eq3` right3 @?= True,
				testCase "right3 /= forward3" $
					right3 `eq3` forward3 @?= False
			],

		testGroup "3D pointing orientation utils (aiming)" $
			[
			]
	]
