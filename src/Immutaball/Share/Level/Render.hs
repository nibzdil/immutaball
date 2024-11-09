{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Render.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

module Immutaball.Share.Level.Render
	(
		renderLevel
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Lens

import Immutaball.Share.Level.Analysis
import Immutaball.Share.Level.Base
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- | TODO: implement.
renderLevel :: Wire ImmutaballM ((MView, SolWithAnalysis), IBStateContext) IBStateContext
renderLevel = proc ((_camera, swa), cxtn) -> do
	let levelPath = swa^.swaMeta.smPath
	(mlastLevelPath, cxtnp1) <- setCurrentlyLoadedSOL -< (levelPath, cxtn)
	let newLevel = Just levelPath == mlastLevelPath
	cxtnp2 <- returnA ||| renderSetupNewLevel -< if' (not newLevel) (Left cxtnp1) (Right (swa, cxtnp1))
	let
		sol :: Sol
		sol = swa^.swaSol

		sa :: SolAnalysis
		sa = swa^.swaSa
	let _unused = (sol, sa)  -- TODO
	returnA -< cxtnp2

renderSetupNewLevel :: Wire ImmutaballM (SolWithAnalysis, IBStateContext) IBStateContext
renderSetupNewLevel = proc (_swa, cxtn) -> do
	returnA -< cxtn
