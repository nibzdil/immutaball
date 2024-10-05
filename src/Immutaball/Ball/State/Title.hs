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
import Data.Functor.Identity

import Immutaball.Share.Context
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- TODO: FIXME: I see no window :(.
-- TODO:
mkTitleState :: IBContext -> Immutaball
mkTitleState baseCxt0 = fromImmutaballSingle $ proc _request -> do
	rec
		-- TODO: maybe revise?
		cxtn <- stateContextStorage (initialStateCxt baseCxt0) -< Just cxtnp1
		-- TODO: fix commented line
		--cxtnp1 <- delay Nothing -< requireVideo -< cxtn
		cxtnp1 <- delayWire (initialStateCxt baseCxt0) requireVideo -< cxtn
	-- TODO:
	-- case request of  -- TODO
	returnA -< Identity ContinueResponse
	--returnA -< Identity DoneResponse
