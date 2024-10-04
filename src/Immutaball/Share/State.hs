{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Immutaball.Share.State
	(
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
		fromImmutaballMulti,
		fromImmutaballSingle
	) where

import Prelude ()
import Immutaball.Prelude

--import Control.Arrow
import Data.Functor.Identity

import Control.Monad.Trans.MaybeM
--import Control.Wire

import Immutaball.Share.AutoPar
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.Wire

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
	  ContinueResponse
	-- | Tell the controller to stop stepping.  We are done.
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

-- TODO:
fromImmutaballMulti :: Wire ImmutaballM RequestFrameMulti ResponseFrameMulti -> Immutaball
fromImmutaballMulti = error "TODO: unimplemented."

fromImmutaballSingle :: Wire ImmutaballM RequestFrameSingle ResponseFrameSingle -> Immutaball
fromImmutaballSingle = error "TODO: unimplemented."
