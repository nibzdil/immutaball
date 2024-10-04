{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Immutaball.Share.State
	(
		-- * Immutaball wires
		Immutaball,
		ImmutaballM,
		RequestFrameMulti,
		RequestFrameSingle,
		RequestFrame,
		Request(..),
		ResponseFrameMulti,
		ResponseFrameSingle,
		ResponseFrame,
		Response(..),
		{-
		closeFork,
		closeFork',
		immutaballIOLinear,
		-}
		stepImmutaball,

		-- * Frame management
		immutaballMultiToSingle,
		immutaballSingleToMulti,
		fromImmutaballMulti,
		fromImmutaballSingle,
		immutaballMultiQueueFrames
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Data.Functor.Identity

import Control.Monad.Trans.MaybeM

import Immutaball.Share.AutoPar
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.Wire

-- * Immutaball wires

-- | An immutaball wire.
--
-- Wire is perhaps like Fixed StateT.
--
-- > data Wire m a b = Wire { _stepWire :: a -> m (b, Wire m a b) }
--type Immutaball = Wire Maybe RequestFrame ResponseFrame
type Immutaball = Wire ImmutaballM RequestFrame ResponseFrame

-- | Immutaball wire monad.
type ImmutaballM = AutoParT (MaybeMT ImmutaballIOF)

type RequestFrameMulti = [Request]
type RequestFrameSingle = Identity Request
type RequestFrame = RequestFrameMulti
data Request =
	  Clock Float            -- ^ timer dt
	| Paint Float            -- ^ paint t
	| Point Int Int Int Int  -- ^ mouse movement x y dx dy
	| Stick Int Float        -- ^ stick axis value
	| Angle Float Float      -- ^ angle x z
	| Click Int Bool         -- ^ click button down
	| Keybd Int Bool         -- ^ keyboard char down
	| Buttn Int Bool         -- ^ button button down
	| Touch Int Int Float Float Float Float Float  -- ^ finger-touch device finger x y dx dy pressure
	deriving (Eq, Ord, Show)

type ResponseFrameMulti = [Response]
type ResponseFrameSingle = Identity Response
type ResponseFrame = ResponseFrameMulti
data Response =
	-- | Tell the controller that we're alive and to keep stepping.
	-- If there is no continue response, this thread is done if there is no
	-- forking.  If there are multiple continue responses in a single step,
	-- this tells the controller to fork (not applicable with single reponse
	-- wires).  (On a single fork, as the controller source indicates, only use
	-- the fork construction like 'PureFork', which means this thread will
	-- still continue alongside the fork; if you include both a PureFork a
	-- ContinueResponse, the end result will be 3 steppers running, not 2
	-- steppers.)
	  ContinueResponse
	-- | Tell the controller to stop stepping.  We are done.  (In a multi
	-- frame, this is equivalent to a no-op; removing it from the response list
	-- gives equivalent results, for our controller.)
	| DoneResponse
	-- | Request the controller fork the wire; requires the context enable forking.
	| PureFork         Immutaball
	-- | Request the controller fork the wire; requires the context enable forking.
	| ImmutaballIOFork (ImmutaballIOF Immutaball)
	-- | AnythingElseToTellTheController NewIBContextIfNeeded SomeOtherData

-- The controller now manages forking and closing more fully.
{-
-- | End a wire.
--
-- This can be combined with 'ImmutaballIOFork' to keep a single wire running.
{- --closeFork :: Immutaball -}
closeFork :: Wire ImmutaballMClosable () ()
--closeFork = wire $ \_ -> hoistMaybe Nothing
closeFork = withM returnA (const . hoistMaybe $ Nothing)

closeFork' :: Wire ImmutaballMClosable () a
--closeFork' = wire $ \_ -> hoistMaybe Nothing
closeFork' = withM returnA (const . hoistMaybe $ Nothing)

immutaballIOLinear :: ImmutaballIOF Immutaball -> Wire ImmutaballMClosable () ResponseFrame
immutaballIOLinear ibIO = wire (\() -> hoistMaybe $ Just ([ImmutaballIOFork ibIO], closeFork'))
-}

-- | Convenience utility to simplify the 'AutoPar' layer.
--
-- It's 'stepWire' without 'AutoPar'.
stepImmutaball :: Immutaball -> RequestFrame -> MaybeMT ImmutaballIOF (ResponseFrame, Immutaball)
stepImmutaball immutaball request = runAutoParT $ stepWire immutaball request

-- * Frame management

-- | Send one request at a time; if there is no response, then treat it as a
-- DoneResponse (just as the controller would close a wire if a multi-response
-- wire returns [] with no continue).
immutaballMultiToSingle :: Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Wire ImmutaballM RequestFrameSingle ResponseFrameSingle
immutaballMultiToSingle w = proc (Identity request) -> do
	responses <- w -< [request]
	mresponse <- queue -< responses
	returnA -< Identity $ maybe DoneResponse id mresponse

immutaballSingleToMulti :: Wire ImmutaballM RequestFrameSingle ResponseFrameSingle -> Wire ImmutaballM RequestFrameMulti ResponseFrameMulti
immutaballSingleToMulti = error "TODO: unimplemented."

fromImmutaballMulti :: Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Immutaball
fromImmutaballMulti = id

fromImmutaballSingle :: Wire ImmutaballM RequestFrameSingle ResponseFrameSingle -> Immutaball
fromImmutaballSingle = immutaballSingleToMulti

-- | Transform a wire that can handle unlimited requests and response,
-- into one that queues up to the context's limit to only process so many
-- requests and so many responses at once.
immutaballMultiQueueFrames :: Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Immutaball
immutaballMultiQueueFrames = error "TODO: unimplemented."
