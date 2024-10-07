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
	rec
		--cxtnp1 <- delay (initialStateCxt baseCxt0) >>> requireVideo -< cxtn
		--cxtn <- stateContextStorage (initialStateCxt baseCxt0) <<< Just <$> returnA -< cxtnp1
		cxt <- delay (initialStateCxt baseCxt0) >>> requireVideo -< cxt
	-- TODO:
	_ <- monadic -< case request of (Keybd i b) -> liftIBIO (BasicImmutaballIOF $ PutStrLn ("DEBUG0: keyboard event: " ++ show (i, b)) ()); _ -> liftIBIO (pure ())
	--_ <- monadic -< liftIBIO (BasicImmutaballIOF $ PutStrLn ("DEBUG2: mkTitleState end") ())
	-- TODO:
	returnA -< pure ContinueResponse
