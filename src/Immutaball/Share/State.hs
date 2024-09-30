{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.State
	(
		RequestFrame,
		Request(..),
		ResponseFrame,
		Response(..)
	) where

import Immutaball.Share.ImmutaballIO

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
	ImmutaballIO ImmutaballIO
