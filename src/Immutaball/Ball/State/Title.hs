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
import Immutaball.Share.Utils

import Debug.Trace  ---------------------------- TODO--
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.SDLIO
import SDL.Video
import SDL.Video.OpenGL
import qualified Data.Text as T
import Data.Function (fix)

-- TODO: FIXME: I see no window :(.
-- TODO:
mkTitleState :: IBContext -> Immutaball
--mkTitleState baseCxt0 = fromImmutaballSingle $ proc _request -> do
--mkTitleState baseCxt0 = trace "DEBUG1: start" . fromImmutaballSingle $ proc _request -> do
mkTitleState baseCxt0 = trace "DEBUG1: start" $ proc _requests -> do
	-- TODO: FIXME: GHC can't see initialCxt; fails with ‘not in scope’.
	--let initialCxt = initialStateCxt baseCxt0
	{-
	rec
		--cxtn <- stateContextStorage initialCxt -< Just cxtnp1
		--cxtn <- stateContextStorage (initialStateCxt baseCxt0) -< Just cxtnp1
		cxtn <- stateContextStorage (initialStateCxt baseCxt0) -< Nothing
		-- TODO: fix commented line
		--cxtnp1 <- delay Nothing -< requireVideo -< cxtn
		--cxtnp1 <- delayWire (initialStateCxt baseCxt0) requireVideo -< cxtn
		cxtnp1 <- delayWire (initialStateCxt baseCxt0) returnA -< cxtn
	-}
	--cxtn <- stateContextStorage (initialStateCxt baseCxt0) -< Nothing
	--cxtn <- returnA -< Nothing  -- Does not hang.
	--cxtn <- hold (()) -< Nothing  -- Hangs.  Maybe it's an mfix bug.
	cxtn <- debugHold (()) -< Nothing  -- Hangs.  Maybe it's an mfix bug.
	-- TODO:
	-- case request of  -- TODO
	--returnA -< Identity ContinueResponse
	--returnA -< trace "DEBUG0: ContinueResponse" $ Identity ContinueResponse
	returnA -< trace "DEBUG0: ContinueResponse" $ [ContinueResponse]
	--returnA -< Identity DoneResponse

-- This hangs:
{-
debugHold a0 = proc ma -> do
	rec
		output <- returnA -< maybe lastJust id ma
		lastJust <- delay a0 -< output
	returnA -< output
-}
-- This doesn't:
debugHold :: (Applicative m) => a -> Wire m (Maybe a) a
debugHold a0 = flip fix a0 $ \me lastJust -> wire $ \ma -> let a = maybe lastJust id ma in pure (a, me a)

-- Is something wrong with mfix?
