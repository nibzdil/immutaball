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
		withFrameManager,

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
import Immutaball.Share.Utils
import Test.Immutaball.Share.State.Fixtures

main :: IO ()
main = testsMain

testsMain :: IO ()
testsMain = defaultMain (tests False)

tests :: Bool -> TestTree
tests headless = testGroup "Immutaball.Share.State" $
	[
		testCase "init SDL (tinitSDL)" $ do
			_ <- immutaballFixture' sharedSDLManager headless
			return ()
	] ++
	[
		afterInit . testCase "trivial is 3 (tSDLheadless)" . syncGLSDL $
			withImmutaball trivialImmutaball [] >>= (@?= 3),
		afterInit . testCase "tenTimes is 4 (tSDLheadless)" . syncGLSDL $
			withImmutaball tenTimesImmutaball [] >>= (@?= 4),
		afterInit . testCase "holding is 5 (tSDLheadless)" . syncGLSDL $
			withImmutaball holdingImmutaball [] >>= (@?= 5),
		afterInit . testCase "tenTimesCounter is 13 (tSDLheadless)" . syncGLSDL $
			withImmutaball tenTimesCounterImmutaball [] >>= (@?= 13)
	] ++ (if' headless [] $
	[
		after AllFinish "$NF ~ /tSDLheadless/" . afterInit . testCase "can hold a video context (SDLgl)" . exclusively' $
			withImmutaball' False glImmutaball [] >>= (@?= ())
	]) ++
	[
		withFrameManager headless "id frame manager: " id,
		withFrameManager headless "immutaballMultiToSingle: " (fromImmutaballSingle' . immutaballMultiToSingle'),
		withFrameManager headless "immutaballSigleToMulti: "  (fromImmutaballMulti'  . immutaballSingleToMulti'),

		testGroup "test mfix works" $
			[
				withFrameManager headless "loopWire . first: " (loopWire . first)
			]
	] ++
	[
		after AllFinish "$NF ~ /tSDL/" . testCase "clean up SDL (tfinishSDL)" $
			immutaballFixture headless >>= freeImmutaballFixture
	]
	where
		sharedSDLManager :: Bool
		sharedSDLManager = True
		syncGLSDL = if' sharedSDLManager id (if' headless id exclusively')
		afterInit = after AllFinish "$NF ~ /tinitSDL/"
		exclusively' = if' sharedSDLManager id exclusively

withFrameManager :: (Applicative t) =>
	Bool ->
	String ->
	(
		Wire ImmutaballM (t Request)  (t Response) ->
		Wire ImmutaballM RequestFrame ResponseFrame
	) ->
	TestTree
withFrameManager headless prefix frameManager =
	testGroup (prefix ++ " immutaball wire tests with frame manager") $
		[
			afterInit . testCase (prefix ++ "trivial is 3 (tSDLheadless)") . syncGLSDL $
				withImmutaball ((frameManager .) . trivialImmutaball) [] >>= (@?= 3),
			afterInit . testCase (prefix ++ "tenTimes is 4 (tSDLheadless)") . syncGLSDL $
				withImmutaball ((frameManager .) . tenTimesImmutaball) [] >>= (@?= 4),
			afterInit . testCase (prefix ++ "holding is 5 (tSDLheadless)") . syncGLSDL $
				withImmutaball ((frameManager .) . holdingImmutaball) [] >>= (@?= 5),
			afterInit . testCase (prefix ++ "tenTimesCounter is 13 (tSDLheadless)") . syncGLSDL $
				withImmutaball ((frameManager .) . tenTimesCounterImmutaball) [] >>= (@?= 13)
		] ++ (if' headless [] $
		[
			after AllFinish "$NF ~ /tSDLheadless/" . afterInit . testCase "can hold a video context (SDLgl)" . exclusively' $
				withImmutaball' False ((frameManager .) . glImmutaball) [] >>= (@?= ())
		])
	where
		sharedSDLManager :: Bool
		sharedSDLManager = True
		syncGLSDL = if' sharedSDLManager id (if' headless id exclusively')
		afterInit = after AllFinish "$NF ~ /tinitSDL/"
		exclusively' = if' sharedSDLManager id exclusively

--trivialImmutaball :: TMVar Integer -> IBContext -> Immutaball
trivialImmutaball :: (Applicative t) => TMVar Integer -> IBContext -> Wire ImmutaballM (t Request) (t Response)
trivialImmutaball mout _cxt0 = proc _request -> do
	_ <- monadic -< liftIBIO $ Atomically (putTMVar mout 3) id
	returnA -< pure DoneResponse

tenTimesImmutaball :: (Applicative t) => TMVar Integer -> IBContext -> Wire ImmutaballM (t Request) (t Response)
tenTimesImmutaball mout _cxt0 = proc _request -> do
	_ <- monadic -< liftIBIO $ Atomically (writeTMVar mout 4) id
	delayNI 9 (pure ContinueResponse) -< pure DoneResponse

holdingImmutaball :: (Applicative t) => TMVar Integer -> IBContext -> Wire ImmutaballM (t Request) (t Response)
holdingImmutaball mout _cxt0 = proc _requests -> do
	x <- hold 5 -< Nothing
	_ <- monadic -< liftIBIO $ Atomically (writeTMVar mout x) id
	delayNI 9 (pure ContinueResponse) -< pure DoneResponse

tenTimesCounterImmutaball :: (Applicative t) => TMVar Integer -> IBContext -> Wire ImmutaballM (t Request) (t Response)
tenTimesCounterImmutaball mout _cxt0 = proc _requests -> do
	_ <- monadic <<< delay (liftIBIO $ Atomically (putTMVar mout 3) id) <<< constWire (liftIBIO $ pure ()) -< ()
	x <- monadic -< liftIBIO $ Atomically (takeTMVar mout) id
	_ <- monadic -< liftIBIO $ Atomically (putTMVar mout (x+1)) id
	delayNI 9 (pure ContinueResponse) -< pure DoneResponse

glImmutaball :: (Applicative t) => TMVar () -> IBContext -> Wire ImmutaballM (t Request) (t Response)
glImmutaball mout baseCxt0 = proc _requests -> do
	_ <- monadic -< liftIBIO $ Atomically (writeTMVar mout ()) id
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireVideo -< cxtLast
		cxt <- returnA -< cxtn
	delayNI 9 (pure ContinueResponse) -< pure DoneResponse
	where cxt0 = initialStateCxt baseCxt0
