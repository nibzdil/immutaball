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
	rec
		cxtn <- stateContextStorage cxt0 <<< Just <$> requireVideo <<< delay cxt0 -< cxtn
	-- TODO
	returnA -< pure ContinueResponse
	where cxt0 = either initialStateCxt id baseCxt0
