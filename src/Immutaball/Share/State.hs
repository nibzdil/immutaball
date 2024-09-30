{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.State
	(
		Immutaball,
		RequestFrame,
		Request(..),
		ResponseFrame,
		Response(..),
		closeFork
	) where

import Control.Arrow

import Control.Wire
import Data.Functor.Identity

import Immutaball.Share.ImmutaballIO

-- | An immutaball wire.
--
-- Wire is perhaps like Fixed StateT.
--
-- > data Wire m a b = Wire { _stepWire :: a -> m (b, Wire m a b) }
type Immutaball = Wire Maybe RequestFrame ResponseFrame

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

-- | End a wire.
--
-- This can be combined with 'ImmutaballIOFork' to keep a single wire running.
closeFork :: Immutaball
closeFork = withM returnA (const Nothing)
