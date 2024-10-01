{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Immutaball.Ball.State.Title
	(
		mkTitleState
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow

import Control.Wire

import Immutaball.Share.Context
import Immutaball.Share.State

-- TODO
mkTitleState :: IBContext -> Immutaball
mkTitleState _cxt0 = proc _requestFrame -> do
	returnA -< []
