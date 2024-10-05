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

-- TODO: FIXME: I see no window :(.
-- TODO:
mkTitleState :: IBContext -> Immutaball
--mkTitleState baseCxt0 = fromImmutaballSingle $ proc _request -> do
{-
mkTitleState baseCxt0 = trace "DEBUG1: start" . fromImmutaballSingle $ proc _request -> do
	-- TODO: FIXME: GHC can't see initialCxt; fails with ‘not in scope’.
	--let initialCxt = initialStateCxt baseCxt0
	rec
		--cxtn <- stateContextStorage initialCxt -< Just cxtnp1
		cxtn <- stateContextStorage (initialStateCxt baseCxt0) -< Just cxtnp1
		-- TODO: fix commented line
		--cxtnp1 <- delay Nothing -< requireVideo -< cxtn
		cxtnp1 <- delayWire (initialStateCxt baseCxt0) requireVideo -< cxtn
	-- TODO:
	-- case request of  -- TODO
	--returnA -< Identity ContinueResponse
	returnA -< trace "DEBUG0: ContinueResponse" $ Identity ContinueResponse
	--returnA -< Identity DoneResponse
-}
{-
mkTitleState baseCxt0 = trace "DEBUG1: start" $ proc _request -> do
	--returnA -< [DoneResponse]
	--_ <- returnA -< ()
	-- TODO FIXME: how come only the first IO is executed (without the
	-- controller getting to the point of checking for allowWireForks), and
	-- then it quits?
	_ <- monadic -< liftIBIO . BasicImmutaballIOF . PutStrLn $ "DEBUG12: IO works!"
	_ <- monadic -< liftIBIO . BasicImmutaballIOF . PutStrLn $ "DEBUG13: IO works!"
	_ <- monadic -< liftIBIO . BasicImmutaballIOF . DelayUs $ (5 * 1000 * 1000)

	--returnA -< [DoneResponse]
	returnA -< [ContinueResponse]
-}

{-
-- TODO: try a non-arrow notation version to see if it works.  Maybe (probably
-- not) it's an Arrows syntax bug.  Maybe when composing it manually, you find
-- something that doesn't make sense or think of something to check.
mkTitleState baseCxt0 = trace "DEBUG1: start" $ -- . wire $ \_request ->
	pure (liftIBIO . BasicImmutaballIOF . PutStrLn $ "DEBUG12: IO works!") >>> monadic >>>
	pure (liftIBIO . BasicImmutaballIOF . PutStrLn $ "DEBUG13: IO works!") >>> monadic >>>
	pure (liftIBIO . BasicImmutaballIOF . DelayUs $ (5 * 1000 * 1000)) >>> monadic >>>
	pure [ContinueResponse]
-}
-- Yay, now the following works!
{-
-- Okay, same results.  So it's not Arrow notation.
-- Here, I'll just keep it here for now to reduce possible variables:
-- {-
mkTitleState baseCxt0 = trace "DEBUG1: start" $ -- . wire $ \_request ->
	pure (liftIBIO . BasicImmutaballIOF $ PutStrLn "DEBUG12: IO works!" ()) >>> monadic >>>
	pure (liftIBIO . BasicImmutaballIOF $ PutStrLn "DEBUG13: IO works!" ()) >>> monadic >>>
	pure (liftIBIO . BasicImmutaballIOF $ DelayUs (5 * 1000 * 1000) ()) >>> monadic >>>
	pure [ContinueResponse]
-- -}
-- TODO: try a version that doesn't use ‘monadic’.
-- :O Oh, I think that PutStrLn lacks any callback!!!
-- Aha!!!  That's probably it.  Maybe just add a ‘me’ component to PutStrLn (like ‘(() -> me)’).  So it's not String -> Immu, but String -> Immu -> Immu.
--mkTitleState baseCxt0 = trace "DEBUG1: start" . wire $ \_request -> (liftIBIO . BasicImmutaballIOF . PutStrLn $ "DEBUG12: IO works!")
-}

-- This works too =).
mkTitleState baseCxt0 = trace "DEBUG1: start" $ proc _request -> do
	_ <- monadic -< liftIBIO . BasicImmutaballIOF $ PutStrLn "DEBUG12: IO works!" ()
	_ <- monadic -< liftIBIO . BasicImmutaballIOF $ PutStrLn "DEBUG13: IO works!" ()
	window <- monadic -< liftIBIO . BasicImmutaballIOF . SDLIO $ SDLWithWindow (T.pack "DEBUG") defaultWindow id
	_ <- monadic -< liftIBIO . BasicImmutaballIOF $ DelayUs (5 * 1000 * 1000) ()

	returnA -< [ContinueResponse]
