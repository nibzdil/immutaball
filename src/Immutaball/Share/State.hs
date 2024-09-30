{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.State
	(
		Immutaball,
		ImmutaballF(..), immutaballF,
		getImmutaballF,
		RequestFrame,
		Request(..),
		ResponseFrame,
		Response(..)
	) where

import Control.Lens
import Control.Monad.Trans.Except
import Control.Wire
import Data.Functor.Identity

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.Utils

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

	| IOResponse Int 
	deriving (Eq, Ord, Show)

type ResponseFrame = [Response]
data Response =
	ImmutaballIO

-- | An immutaball wire.
--
-- Wire is perhaps like Fixed StateT.
--
-- > data Wire m a b = Wire { _stepWire :: a -> m (b, Wire m a b) }
--
-- We start with:
-- > type Immutaball = Wire Identity RequestFrame ResponseFrame
--
-- But to allow wires when stepped to return an ImmutaballIO instead, we add an
-- ExceptionT to get ‘Either’, and this requires Fixed for self-reference.
type Immutaball = Fixed ImmutaballF
newtype ImmutaballF a = ImmutaballF { _immutaballF :: Wire (Except (ImmutaballIOF (ResponseFrame, a))) RequestFrame ResponseFrame }
	--deriving (Arrow, ArrowChoice, ArrowLoop, Choice, Category, Functor, Applicative)
makeLenses ''ImmutaballF

getImmutaballF :: ImmutaballF a -> Wire (Except (ImmutaballIOF (ResponseFrame, a))) RequestFrame ResponseFrame
getImmutaballF = (^.immutaballF)
