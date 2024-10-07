{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Test.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Test.Immutaball.Share.State.Test
	(
		main,
		testsMain,
		tests
	) where

import Control.Arrow

import Control.Concurrent.STM.TMVar
import Test.HUnit
--import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit hiding ((@?=), assertBool)
--import Test.Tasty.QuickCheck

--import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.State
import Immutaball.Share.Wire
import Test.Immutaball.Share.State.Fixtures

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain tests

tests :: TestTree
tests = testGroup "Immutaball.Share.State" $
	[
		testCase "trivial is 3" $
			withImmutaball trivialImmutaball [] >>= (@?= 3)
	]

trivialImmutaball :: TMVar Integer -> IBContext -> Immutaball
trivialImmutaball mout _cxt0 = proc _requests -> do
	_ <- monadic -< liftIBIO $ Atomically (putTMVar mout 3) id
	returnA -< [DoneResponse]
