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

import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- TODO:
mkTitleState :: Either IBContext IBStateContext -> Immutaball
mkTitleState baseCxt0 = fromImmutaballSingle $ proc (Identity _request) -> do
	--let cxt0 = either initialStateCxt id baseCxt0
	rec
		-- BROKEN: -<< discards wires!  This turns into a fork bomb of new windows.
		-- 	> cl && cabal build --package-db=~/.local/state/cabal/store/ghc-9.13.20240927/package.db && cl && { (sleep 2.5s && pkill -KILL immutaball) & timeout --kill-after=0.5s 2s cabal run --package-db=~/.local/state/cabal/store/ghc-9.13.20240927/package.db immutaball -- ; }
		-- If you use -<<, don't expect the left side to ‘keep stepping’.
		--cxtnp1 <- delay cxt0 -<< cxtn
		--cxtn <- stateContextStorage cxt0 <<< Just <$> requireVideo -<< cxtnp1
		cxtnp1 <- delay (either initialStateCxt id baseCxt0) -< cxtn
		cxtn <- stateContextStorage (either initialStateCxt id baseCxt0) <<< Just <$> requireVideo -< cxtnp1
	-- TODO
	returnA -< pure ContinueResponse
