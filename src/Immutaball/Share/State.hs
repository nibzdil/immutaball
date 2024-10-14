{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, TemplateHaskell #-}

module Immutaball.Share.State
	(
		-- * Immutaball wires
		Immutaball,
		ImmutaballM,
		runImmutaballM,
		RequestFrameMulti,
		RequestFrameSingle,
		RequestFrame,
		Request(..), AsRequest(..),
		ResponseFrameMulti,
		ResponseFrameSingle,
		ResponseFrame,
		Response(..), AsResponse(..),
		{-
		closeFork,
		closeFork',
		immutaballIOLinear,
		-}
		stepImmutaball,
		stepImmutaballWire,
		liftIBIO,

		IBContext,
		ContextConfig,
		StaticConfig,

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

import Control.Lens

import Control.Monad.Trans.MaybeM
import Immutaball.Share.AutoPar
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
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

runImmutaballM :: ImmutaballM me -> ImmutaballIOF me
runImmutaballM = runMaybeInMT . runAutoParT

type RequestFrameMulti = [Request]
type RequestFrameSingle = Identity Request
type RequestFrame = RequestFrameMulti
data Request =
	  Clock Double           -- ^ timer dt
	| Paint Double           -- ^ paint t
	| Point Int Int Int Int  -- ^ mouse movement x y dx dy
	| Stick Int Double       -- ^ stick axis value
	| Angle Double Double    -- ^ angle x z
	| Click Int Bool         -- ^ click button down
	| Keybd Int Bool         -- ^ keyboard char down
	| Buttn Int Bool         -- ^ button button down
	| Touch Int Int Double Double Double Double Double  -- ^ finger-touch device finger x y dx dy pressure
	deriving (Eq, Ord, Show)
--makeClassyPrisms ''Request

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
--makeClassyPrisms ''Response

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
stepImmutaball :: Immutaball -> RequestFrame -> ImmutaballIOF (ResponseFrame, Immutaball)
stepImmutaball = stepImmutaballWire

-- | Convenience utility to simplify the 'AutoPar' layer.
--
-- It's 'stepWire' without 'AutoPar'.
stepImmutaballWire :: Wire ImmutaballM a b -> a -> ImmutaballIOF (b, Wire ImmutaballM a b)
stepImmutaballWire immutaball request = runImmutaballM $ stepWire immutaball request

liftIBIO :: ImmutaballIOF a -> ImmutaballM a
liftIBIO = AutoParT . MaybeMT . Left

type IBContext     = IBContext' Immutaball
type ContextConfig = ContextConfig' IBContext Immutaball
type StaticConfig  = StaticConfig' (IBContext -> Maybe Immutaball)

-- * Frame management

-- | Send one request at a time; if there is no response, then treat it as a
-- DoneResponse (just as the controller would close a wire if a multi-response
-- wire returns [] with no continue).
immutaballMultiToSingle :: Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Wire ImmutaballM RequestFrameSingle ResponseFrameSingle
immutaballMultiToSingle w = proc (Identity request) -> do
	responses <- w -< [request]
	mresponse <- queue -< responses
	returnA -< Identity $ maybe DoneResponse id mresponse

-- | If the single wire received no request, we give an empty response frame
-- (like a DoneResponse).
immutaballSingleToMulti :: Wire ImmutaballM RequestFrameSingle ResponseFrameSingle -> Wire ImmutaballM RequestFrameMulti ResponseFrameMulti
immutaballSingleToMulti w = proc requests -> do
	request <- queue -< requests
	case request of
		Nothing -> do
			returnA -< []
		Just jrequest -> do
			(Identity response) <- w -< Identity jrequest
			returnA -< [response]

fromImmutaballMulti :: Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Immutaball
fromImmutaballMulti = id

fromImmutaballSingle :: Wire ImmutaballM RequestFrameSingle ResponseFrameSingle -> Immutaball
fromImmutaballSingle = immutaballSingleToMulti

-- | Transform a wire that can handle unlimited requests and response,
-- into one that queues up to the context's limit to only process so many
-- requests and so many responses at once.
immutaballMultiQueueFrames :: IBContext -> Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Wire ImmutaballM RequestFrameMulti ResponseFrameMulti
immutaballMultiQueueFrames cxt w = proc requests -> do
	requestChunk <- maybe returnA queueN (cxt^.ibStaticConfig.maxStepFrameSize) -< requests
	responses <- w -< requestChunk
	responseChunk <- maybe returnA queueN (cxt^.ibStaticConfig.maxResponseFrameSize) -< responses
	returnA -< responseChunk

-- Lenses at end of file to avoid TH errors.
makeClassyPrisms ''Request
makeClassyPrisms ''Response
