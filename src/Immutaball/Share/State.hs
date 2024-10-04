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
		RequestFrame,
		Request(..),
		ResponseFrame,
		Response(..),
		closeFork,
		closeFork',
		immutaballIOLinear,
		stepImmutaball
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow

import Control.Monad.Trans.Maybe
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

-- | Maybe identity.  Maybe can end a wire.
type ImmutaballM = MaybeT AutoPar

type RequestFrame = [Request]
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

type ResponseFrame = [Response]
data Response =
	  PureFork         Immutaball
	| ImmutaballIOFork (ImmutaballIOF Immutaball)
	-- | AnythingElseToTellTheController NewIBContextIfNeeded SomeOtherData

-- | End a wire.
--
-- This can be combined with 'ImmutaballIOFork' to keep a single wire running.
{- --closeFork :: Immutaball -}
closeFork :: Wire ImmutaballM () ()
--closeFork = wire $ \_ -> hoistMaybe Nothing
closeFork = withM returnA (const . hoistMaybe $ Nothing)

closeFork' :: Wire ImmutaballM () a
--closeFork' = wire $ \_ -> hoistMaybe Nothing
closeFork' = withM returnA (const . hoistMaybe $ Nothing)

immutaballIOLinear :: ImmutaballIOF Immutaball -> Wire ImmutaballM () ResponseFrame
immutaballIOLinear ibIO = wire (\() -> hoistMaybe $ Just ([ImmutaballIOFork ibIO], closeFork'))

-- | Convenience utility to simplify the 'AutoPar' layer.
--
-- It's 'stepWire' without 'AutoPar'.
stepImmutaball :: Immutaball -> RequestFrame -> Maybe (ResponseFrame, Immutaball)
stepImmutaball immutaball request = runAutoPar . runMaybeT $ stepWire immutaball request
