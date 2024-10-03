{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Controller
	(
		controlImmutaball,
		takeAllSDLEvents,
		stepFrame,
		stepEvent,
		stepClock,
		unimplementedHelper,

		-- * SDL utils
		isKbdEventDown,
		kbdEventChar
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))

import Control.Concurrent.STM.TMVar
import Control.Lens
import Control.Wire.Controller
import SDL.Event
import SDL.Input.Keyboard

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.Utils

controlImmutaball :: IBContext -> Immutaball -> ImmutaballIO
controlImmutaball cxt0 immutaball0 =
	result
	where
		result :: ImmutaballIO
		--result = runBasicImmutaballIO (mkPutStrLn "Internal error: unimplemented.") <>> runBasicImmutaballIO mkExitFailureBasicIO
		result = initialFrame
		initialFrame :: ImmutaballIO
		initialFrame =
			mkBIO . GetUs $ \us0 ->
			nextFrame us0 immutaball0
		nextFrame :: Integer -> Immutaball -> ImmutaballIO
		nextFrame usNm1 immutaballN =
			mkBIO . GetUs $ \usN ->
			let dus = max 0 $ usNm1 - usN in
			let ds = (fromInteger dus / 1000000.0)  :: Float in
			let usNm1pMinClockPeriod = usNm1 + (max 0 . round $ 1000000.0 * maybe 0 id (cxt0^.ibStaticConfig.minClockPeriod)) in
			if' (usN < usNm1pMinClockPeriod) (mkBIO (DelayUs (usNm1pMinClockPeriod - usN)) <>>) id .
			takeAllSDLEvents cxt0 $ \events ->
			let events' = maybe id (take . fromIntegral) (cxt0^.ibStaticConfig.maxFrameEvents) events in
			stepFrame cxt0 ds usN events' immutaballN (nextFrame usN)

takeAllSDLEvents :: IBContext -> ([Event] -> ImmutaballIO) -> ImmutaballIO
takeAllSDLEvents cxt withEvents =
	mkAtomically newEmptyTMVar $ \eventStorage ->
	flip fix [] $ \me events ->
	(issueCommand (cxt^.ibSDLManagerHandle) (PollEvent eventStorage) <>) .
	mkAtomically (takeTMVar eventStorage) $ \mevent ->
	case mevent of
		Nothing -> withEvents $ reverse events
		Just event -> me (event:events)

-- Step each event then clock.
-- TODO: handle maxEventPeriod.  (stepEvent just sometimes adds a stepClock
-- before going back to the callback.)
stepFrame :: IBContext -> Float -> Integer -> [Event] -> Immutaball -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
stepFrame cxt ds us events immutaball withImmutaball =
	foldr
		(\event withImmutaballNp1 -> \immutaballN -> stepEvent cxt event immutaballN withImmutaballNp1)
		(\immutaballN -> stepClock cxt immutaballN withImmutaball)
		events
		immutaball

stepEvent :: IBContext -> Event -> Immutaball -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
stepEvent cxt event immutaballN withImmutaballNp1 =
	case event of
		(Event _ (KeyboardEvent kbdEvent)) ->
			let (char, down) = (fromIntegral $ kbdEventChar kbdEvent, isKbdEventDown kbdEvent) in
			let mresponse = stepWire immutaballN [Keybd char down] in
			maybe (const mempty) (&) mresponse $ \(response, immutaballNp1) ->
			--response <> withImmutaballNp1 immutaballNp1
			(withImmutaballNp1 immutaballNp1 <>) .
			mconcat . flip map response $ \responseI ->
			case responseI of
				PureFork immutaballNp1_2 -> withImmutaballNp1 immutaballNp1_2
				ImmutaballIOFork ibio -> Fixed $ withImmutaballNp1 <$> ibio
		_ ->
			-- Ignore all unhandled events.
			withImmutaballNp1 immutaballN

stepClock :: IBContext -> Immutaball -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
stepClock cxt immutaballN withImmutaballNp1 =
	unimplementedHelper

unimplementedHelper :: ImmutaballIO
unimplementedHelper =
	runBasicImmutaballIO (mkPutStrLn "Internal error: unimplemented.") <>> runBasicImmutaballIO mkExitFailureBasicIO

-- * SDL utils

isKbdEventDown :: KeyboardEventData -> Bool
isKbdEventDown (KeyboardEventData _ Pressed  _ _) = True
isKbdEventDown (KeyboardEventData _ Released _ _) = False

kbdEventChar :: KeyboardEventData -> Integer
kbdEventChar (KeyboardEventData _ _ _ (Keysym _ (Keycode keyCode) _)) = (fromIntegral keyCode)
