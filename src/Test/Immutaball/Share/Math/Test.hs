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
	) where

import Control.Arrow
import Data.Functor.Identity

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

tests :: TestTree
tests = testGroup "Immutaball.Share.Math" $
	[
		testCase "simpleConstant == 3" $
			simpleConstant @?= 3
	]
