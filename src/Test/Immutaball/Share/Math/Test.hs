{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}

module Test.Immutaball.Share.Math.Test
	(
		main,
		testsMain,
		tests
	) where

--import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
--import Test.Tasty.HUnit hiding ((@?=), assertBool)
--import Test.Tasty.QuickCheck

import qualified Test.Immutaball.Share.Math.Core.Test

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

tests :: TestTree
tests = testGroup "Immutaball.Share.Math.Core" $
	[
		Test.Immutaball.Share.Math.Core.Test.tests
	]
