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
		tests,

		trivialImmutaball,
		tenTimesImmutaball,
		holdingImmutaball,
		tenTimesCounterImmutaball,
		glImmutaball
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
import Immutaball.Share.State.Context
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
			withImmutaball trivialImmutaball [] >>= (@?= 3),
		testCase "tenTimes is 3" $
			withImmutaball tenTimesImmutaball [] >>= (@?= 4),
		testCase "holding is 3" $
			withImmutaball holdingImmutaball [] >>= (@?= 5),
		testCase "tenTimesCounter is 13" $
			withImmutaball tenTimesCounterImmutaball [] >>= (@?= 13),
		testCase "can hold a video context" $
			withImmutaball' False glImmutaball [] >>= (@?= ())
	]

trivialImmutaball :: TMVar Integer -> IBContext -> Immutaball
trivialImmutaball mout _cxt0 = proc _requests -> do
	_ <- monadic -< liftIBIO $ Atomically (putTMVar mout 3) id
	returnA -< [DoneResponse]

tenTimesImmutaball :: TMVar Integer -> IBContext -> Immutaball
tenTimesImmutaball mout _cxt0 = proc _requests -> do
	_ <- monadic -< liftIBIO $ Atomically (writeTMVar mout 4) id
	delayNI 9 [ContinueResponse] -< [DoneResponse]

holdingImmutaball :: TMVar Integer -> IBContext -> Immutaball
holdingImmutaball mout _cxt0 = proc _requests -> do
	x <- hold 5 -< Nothing
	_ <- monadic -< liftIBIO $ Atomically (writeTMVar mout x) id
	delayNI 9 [ContinueResponse] -< [DoneResponse]

tenTimesCounterImmutaball :: TMVar Integer -> IBContext -> Immutaball
tenTimesCounterImmutaball mout _cxt0 = proc _requests -> do
	_ <- monadic <<< delay (liftIBIO $ Atomically (putTMVar mout 3) id) <<< constWire (liftIBIO $ pure ()) -< ()
	x <- monadic -< liftIBIO $ Atomically (takeTMVar mout) id
	_ <- monadic -< liftIBIO $ Atomically (putTMVar mout (x+1)) id
	delayNI 9 [ContinueResponse] -< [DoneResponse]

glImmutaball :: TMVar () -> IBContext -> Immutaball
glImmutaball mout cxt0 = proc _requests -> do
	_ <- monadic -< liftIBIO $ Atomically (writeTMVar mout ()) id
	rec
		cxtnp1 <- delay (initialStateCxt cxt0) -< cxtn
		cxtn <- stateContextStorage (initialStateCxt cxt0) <<< Just <$> requireVideo -< cxtnp1
	delayNI 9 [ContinueResponse] -< [DoneResponse]
