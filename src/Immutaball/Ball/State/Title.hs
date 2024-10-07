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

import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- TODO:
mkTitleState :: IBContext -> Immutaball
mkTitleState baseCxt0 = fromImmutaballSingle $ proc _request -> do
	rec
		cxtnp1 <- delay (initialStateCxt baseCxt0) -< cxtn
		cxtn <- stateContextStorage (initialStateCxt baseCxt0) <<< Just <$> requireVideo -< cxtnp1
	-- TODO:
	returnA -< pure ContinueResponse
