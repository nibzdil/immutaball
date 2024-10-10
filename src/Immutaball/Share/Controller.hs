{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Controller
	(
		controlImmutaball,
		takeAllSDLEvents,
		stepFrameNoMaxClockPeriod,
		stepFrame,
		stepEvent,
		stepEventNoMaxClockPeriod,
		stepClock,
		processStepResult,
		doesResponseFork,
		unimplementedHelper,

		-- * SDL utils
		isKbdEventDown,
		kbdEventChar,
		isMousePressed,
		getMouseButton
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))
import Data.List

import Control.Concurrent.STM.TMVar
import Control.Lens
import SDL.Event
import SDL.Input.Keyboard
import qualified SDL.Raw.Enum as Raw
import SDL.Vect

import Control.Monad.Trans.MaybeM
import Immutaball.Share.AutoPar
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.Utils
import Immutaball.Share.Wire

controlImmutaball :: IBContext -> Immutaball -> ImmutaballIO
controlImmutaball cxt0 immutaball0 =
	result
	where
		result :: ImmutaballIO
		result = initialFrame
		initialFrame :: ImmutaballIO
		initialFrame =
			mkBIO . GetUs $ \us0 ->
			nextFrame us0 [] immutaball0
		nextFrame :: Integer -> [Event] -> Immutaball -> ImmutaballIO
		nextFrame usNm1 queuedEvents immutaballN =
			mkBIO . GetUs $ \usN ->
			let dus = max 0 $ usNm1 - usN in
			let ds = (fromInteger dus / 1000000.0)  :: Float in
			let usNm1pMinClockPeriod = usNm1 + (max 0 . round $ 1000000.0 * maybe 0 id (cxt0^.ibStaticConfig.minClockPeriod)) in
			if' (usN < usNm1pMinClockPeriod) (mkBIO . DelayUs (usNm1pMinClockPeriod - usN)) id .
			takeAllSDLEvents cxt0 $ \events ->
			let events' = queuedEvents ++ events in
			let events'' = maybe id (take . fromIntegral) (cxt0^.ibStaticConfig.maxFrameEvents) events' in
			stepFrame' cxt0 ds usN events'' immutaballN (nextFrame usN)
		stepFrame'
			| Nothing <- (cxt0^.ibStaticConfig.maxClockPeriod) = stepFrameNoMaxClockPeriod'
			| otherwise                                        = stepFrame
		stepFrameNoMaxClockPeriod' cxt ds us events immutaball withImmutaball =
			stepFrameNoMaxClockPeriod cxt ds us events immutaball (withImmutaball [])

takeAllSDLEvents :: IBContext -> ([Event] -> ImmutaballIO) -> ImmutaballIO
takeAllSDLEvents cxt withEvents =
	mkAtomically newEmptyTMVar $ \eventStorage ->
	flip fix [] $ \me events ->
	issueSDLCommand (cxt^.ibSDLManagerHandle) (PollEvent eventStorage) .
	mkAtomically (takeTMVar eventStorage) $ \mevent ->
	case mevent of
		Nothing -> withEvents $ reverse events
		Just event -> me (event:events)

-- | Step each event then clock.
stepFrameNoMaxClockPeriod :: IBContext -> Float -> Integer -> [Event] -> Immutaball -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
stepFrameNoMaxClockPeriod cxt ds us events immutaball withImmutaball =
	foldr
		(\event withImmutaballNp1 -> \immutaballN -> stepEventNoMaxClockPeriod cxt event immutaballN withImmutaballNp1)
		(\immutaballN -> stepClock cxt ds us immutaballN withImmutaball)
		events
		immutaball

-- | Step each event then clock.
--
-- This variant handles 'maxClockPeriod' (see documentation in 'StaticConfig').
-- It ensures that if 'maxClockPeriod' amount of time has passed since any
-- clock step, a clock step is inserted before processing the next event,
-- if at least one event has been processed.
stepFrame :: IBContext -> Float -> Integer -> [Event] -> Immutaball -> ([Event] -> Immutaball -> ImmutaballIO) -> ImmutaballIO
stepFrame cxt ds us events immutaball withImmutaball =
	let z = (\queued _mclockAtUs _noClock immutaballN -> stepClock cxt ds us immutaballN (withImmutaball queued)) in
	let defer = \queued -> z queued Nothing False in
	let eventsWithRemaining = zip events (drop 1 $ tails events) in
	foldr
		(\(event, eventsRemaining) withImmutaballNp1 -> \mclockAtUs noClock immutaballN -> stepEvent cxt event eventsRemaining mclockAtUs noClock immutaballN defer withImmutaballNp1)
		(z [])
		eventsWithRemaining
		((\p -> max 0 . round $ 1000000.0 * p) <$> (cxt^.ibStaticConfig.maxClockPeriod))
		True
		immutaball

-- | Step an event; variant that handles 'maxClockPeriod'.
--
-- maxClockPeriod: the maybe argument just makes it so that if the current time in
-- milliseconds is >= the threshold, we insert a clock step before processing
-- more events.  Then we also use the ‘noClock’ Bool to make sure we process at
-- least one Event at a time.
stepEvent ::
	IBContext -> Event -> [Event] ->
	Maybe Integer -> Bool -> Immutaball ->
	([Event] -> Immutaball -> ImmutaballIO) ->
	(Maybe Integer -> Bool -> Immutaball -> ImmutaballIO) ->
	ImmutaballIO
stepEvent cxt event eventsRemaining mclockAtUs noClock immutaballN defer withImmutaballNp1 =
	case (mclockAtUs, noClock) of
		(Just clockAtUs, False) ->
			mkBIO . GetUs $ \us_ ->
			if' (not $ us_ >= clockAtUs)
				(
					stepEventNoMaxClockPeriod cxt event immutaballN (withImmutaballNp1 mclockAtUs False)
				)
				(
					defer eventsRemaining immutaballN
				)
		_ -> stepEventNoMaxClockPeriod cxt event immutaballN (withImmutaballNp1 mclockAtUs noClock)

-- | Step an event.
stepEventNoMaxClockPeriod :: IBContext -> Event -> Immutaball -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
stepEventNoMaxClockPeriod cxt event immutaballN withImmutaballNp1 =
	case event of
		(Event _ (QuitEvent)) ->
			mempty
		(Event _ (MouseMotionEvent (MouseMotionEventData _ _ _ (P (V2 x y)) (V2 dx dy)))) ->
			let (x', y', dx', dy') = (fromIntegral x, fromIntegral y, fromIntegral dx, fromIntegral dy) in
			let mresponse = stepWire immutaballN (pure $ Point x' y' dx' dy') in
			processStepResult cxt mresponse withImmutaballNp1
		(Event _ (MouseButtonEvent (MouseButtonEventData _ pressed _ mouseButton _ _))) ->
			let (button, down) = (fromIntegral $ getMouseButton mouseButton, isMousePressed pressed) in
			let mresponse = stepWire immutaballN (pure $ Click button down) in
			processStepResult cxt mresponse withImmutaballNp1
		(Event _ (KeyboardEvent kbdEvent)) ->
			let (char, down) = (fromIntegral $ kbdEventChar kbdEvent, isKbdEventDown kbdEvent) in
			let mresponse = stepWire immutaballN (pure $ Keybd char down) in
			processStepResult cxt mresponse withImmutaballNp1
		_ ->
			-- Ignore all unhandled events.
			withImmutaballNp1 immutaballN

stepClock :: IBContext -> Float -> Integer -> Immutaball -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
stepClock cxt du us immutaballN withImmutaballNp1 =
	let mresponse = stepWire immutaballN (pure $ Clock du) in
	processStepResult cxt mresponse $ \immutaballNp1 ->
	let mresponse_ = stepWire immutaballNp1 (pure . Paint $ (fromIntegral us) / 1000000.0) in
	if' (cxt^.ibHeadless)
		(withImmutaballNp1 immutaballNp1)
		(processStepResult cxt mresponse_ withImmutaballNp1)

processStepResult :: (Foldable t) => IBContext -> ImmutaballM (t Response, Immutaball) -> (Immutaball -> ImmutaballIO) -> ImmutaballIO
processStepResult cxt mresponse withImmutaballNp1 =
	runAutoParT mresponse & \mioresponse ->
	either (\ioresponse -> Fixed . flip fmap ioresponse) (&) (runMaybeMT mioresponse) $ \(response, immutaballNp1) ->
	let failFork = mkBIO . PutStrLn "Error: processStepResult: wire forking is disabled, but the wire requested a fork; aborting" $ mkBIO ExitFailureBasicIOF in
	if' (not (cxt^.ibStaticConfig.allowWireForks) && doesResponseFork response) failFork $
	flip foldMap response $ \responseI ->
	case responseI of
		ContinueResponse -> withImmutaballNp1 immutaballNp1
		DoneResponse -> mempty
		PureFork immutaballFork -> withImmutaballNp1 immutaballFork <> withImmutaballNp1 immutaballNp1
		ImmutaballIOFork mimmutaballFork -> Fixed (fmap withImmutaballNp1 mimmutaballFork) <> withImmutaballNp1 immutaballNp1

doesResponseFork :: (Foldable t) => t Response -> Bool
doesResponseFork response
	| any isFork responses = True
	| lcontinues > 1       = True
	| otherwise            = False
	where
		responses :: [Response]
		responses = foldr (:) [] response
		isFork :: Response -> Bool
		isFork (ContinueResponse  ) = False
		isFork (DoneResponse      ) = False
		isFork (PureFork         _) = True
		isFork (ImmutaballIOFork _) = True
		isContinue :: Response -> Bool
		isContinue (ContinueResponse  ) = True
		isContinue (DoneResponse      ) = False
		isContinue (PureFork         _) = False
		isContinue (ImmutaballIOFork _) = False
		lcontinues :: Integer
		lcontinues = genericLength . filter isContinue $ responses

unimplementedHelper :: ImmutaballIO
unimplementedHelper =
	runBasicImmutaballIO (mkPutStrLn "Internal error: unimplemented." $ mkExitFailureBasicIO)

-- * SDL utils

isKbdEventDown :: KeyboardEventData -> Bool
isKbdEventDown (KeyboardEventData _ Pressed  _ _) = True
isKbdEventDown (KeyboardEventData _ Released _ _) = False

kbdEventChar :: KeyboardEventData -> Integer
kbdEventChar (KeyboardEventData _ _ _ (Keysym _ (Keycode keyCode) _)) = (fromIntegral keyCode)

isMousePressed :: InputMotion -> Bool
isMousePressed Pressed  = True
isMousePressed Released = False

getMouseButton :: MouseButton -> Integer
getMouseButton (ButtonLeft)         = Raw.SDL_BUTTON_LEFT
getMouseButton (ButtonMiddle)       = Raw.SDL_BUTTON_MIDDLE
getMouseButton (ButtonRight)        = Raw.SDL_BUTTON_RIGHT
getMouseButton (ButtonX1)           = Raw.SDL_BUTTON_X1
getMouseButton (ButtonX2)           = Raw.SDL_BUTTON_X2
getMouseButton (ButtonExtra button) = fromIntegral button
