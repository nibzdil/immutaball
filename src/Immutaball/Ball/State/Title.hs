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

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- TODO:
mkTitleState :: IBContext -> Immutaball
mkTitleState baseCxt0 = fromImmutaballSingle $ proc (Identity request) -> do
	--_ <- monadic -< liftIBIO (BasicImmutaballIOF $ PutStrLn ("DEBUG1: mkTitleState start") ())
	--rec
		--cxtnp1 <- delay (initialStateCxt baseCxt0) >>> requireVideo -< cxtn
		--cxtn <- stateContextStorage (initialStateCxt baseCxt0) <<< Just <$> returnA -< cxtnp1
		--cxt <- delay (initialStateCxt baseCxt0) >>> requireVideo -< cxt
		--cxt <- delayNI 2 (initialStateCxt baseCxt0) >>> requireVideo -< cxt
		--cxt <- delayNI 2 (initialStateCxt baseCxt0) >>> requireVideo -< cxt
		--cxt <- replaceNow (constWire ()) <<< constWire () <<< requireVideo -< (initialStateCxt baseCxt0)
		--_cxt <- replaceNow (constWire (constWire ())) <<< constWire () <<< requireVideo -< (initialStateCxt baseCxt0)
		--_cxt <- requireVideo >>> constWire () >>> id &&& constWire (constWire ()) >>> replace -< (initialStateCxt baseCxt0)

		--_cxt <- replace (requireVideo >>> constWire () >>> id &&& constWire (constWire ())) -< (initialStateCxt baseCxt0)
		--_repeatSafe <- monadic -< liftIBIO . BasicImmutaballIOF $ DelayUs (1 * 1000 * 1000) ()
	-- TODO: FIXME: when in rec, we get 2 windows!
	--rec
		--_cxt <- replace (requireVideo >>> constWire () >>> id &&& constWire (constWire ())) -< (initialStateCxt baseCxt0)
	-- TODO: FIXME: loopWire also makes 2 windows.
	_ <- monadic -< liftIBIO . BasicImmutaballIOF $ PutStrLn "DEBUG0: start" ()
	_cxt <- loopWire . first $ replace (requireVideo >>> constWire () >>> id &&& constWire (constWire ())) -< (initialStateCxt baseCxt0)
	_repeatSafe <- monadic -< liftIBIO . BasicImmutaballIOF $ DelayUs (1 * 1000 * 1000) ()
	_ <- monadic -< liftIBIO . BasicImmutaballIOF $ PutStrLn "DEBUG1: finish (yay, loopWire works here!)" ()

	--replace :: (Monad m) => Wire m a (Wire m a b) -> Wire m a b
	-- TODO:
	_ <- monadic -< case request of (Keybd i b) -> liftIBIO (BasicImmutaballIOF $ PutStrLn ("DEBUG0: keyboard event: " ++ show (i, b)) ()); _ -> liftIBIO (pure ())
	--_ <- monadic -< liftIBIO (BasicImmutaballIOF $ PutStrLn ("DEBUG2: mkTitleState end") ())
	-- TODO:
	returnA -< pure ContinueResponse
