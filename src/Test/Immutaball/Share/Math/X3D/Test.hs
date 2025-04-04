{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

module Test.Immutaball.Share.Math.X3D.Test
	(
		main,
		testsMain,
		tests,

		simpleConstant,
		sampleLine0,
		sampleLine1
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
import Immutaball.Share.Utils
import Test.Immutaball.Share.Math.Core.Orphans ()

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

simpleConstant :: Integer
simpleConstant = 3

sampleLine0 :: Line3 Double
sampleLine0 = line3Points (Vec3 0 0 0) (Vec3 1 1 0)

sampleLine1 :: Line3 Double
sampleLine1 = line3Points (Vec3 1 0 1) (Vec3 1 3 1)

tests :: TestTree
tests = testGroup "Immutaball.Share.Math.X3D" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3,

		testGroup "line3 line3 tests" $
			[
				testCase "sample lines are distance 1" $
					line3Line3Distance sampleLine0 sampleLine1 `equivalentSmall` 1 @?= True,
				testCase "sample lines are distance 1 with second z-negated" $
					line3Line3Distance sampleLine0 (sampleLine1 & ol3.z3 %~ negate) `equivalentSmall` 1 @?= True
			]
	]
