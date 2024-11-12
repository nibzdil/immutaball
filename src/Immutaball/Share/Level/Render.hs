{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Render.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows #-}

-- | TODO: split shared render into shared and ball render.
--
-- Currently this uses ball game state.
module Immutaball.Share.Level.Render
	(
		renderLevel
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Lens

import Immutaball.Ball.Game
import Immutaball.Share.Level.Analysis
import Immutaball.Share.Level.Base
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Video
import Immutaball.Share.Wire

-- | TODO: implement.
renderLevel :: Wire ImmutaballM ((MView, SolWithAnalysis, GameState), IBStateContext) IBStateContext
renderLevel = proc ((_camera, swa, _gs), cxtn) -> do
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
renderSetupNewLevel = proc (swa, cxtn) -> do
	let sra = swa^.swaSa.saRenderAnalysis

	-- Upload SSBOs.
	cxtnp1  <- setSSBO -< ((shaderSSBOVertexDataLocation,                 sra^.sraVertexDataGPU),                cxtn)
	cxtnp2  <- setSSBO -< ((shaderSSBOGeomDataLocation,                   sra^.sraGeomDataGPU),                  cxtnp1)
	cxtnp3  <- setSSBO -< ((shaderSSBOLumpDataLocation,                   sra^.sraLumpDataGPU),                  cxtnp2)
	cxtnp4  <- setSSBO -< ((shaderSSBOPathDoublesDataLocation,            sra^.sraPathDoublesDataGPU),           cxtnp3)
	cxtnp5  <- setSSBO -< ((shaderSSBOPathInt32sDataLocation,             sra^.sraPathInt32sDataGPU),            cxtnp4)
	cxtnp6  <- setSSBO -< ((shaderSSBOBodyDataLocation,                   sra^.sraBodyDataGPU),                  cxtnp5)
	cxtnp7  <- setSSBO -< ((shaderSSBOGcDataLocation,                     sra^.sraGcArrayGPU),                   cxtnp6)
	cxtnp8  <- setSSBO -< ((shaderSSBOAllGeomPassMvDataLocation,          sra^.sraAllGeomPassMvGPU),             cxtnp7)
	cxtnp9  <- setSSBO -< ((shaderSSBOAllGeomPassTexturesDataLocation,    sra^.sraAllGeomPassTexturesGPU),       cxtnp8)
	cxtnp10 <- setSSBO -< ((shaderSSBOAllGeomPassGisDataLocation,         sra^.sraAllGeomPassGisGPU),            cxtnp9)
	cxtnp11 <- setSSBO -< ((shaderSSBOGeomPassMvRangesDataLocation,       sra^.sraGeomPassMvRangesGPU),          cxtnp10)
	cxtnp12 <- setSSBO -< ((shaderSSBOGeomPassTexturesRangesDataLocation, sra^.sraGeomPassTexturesRangesGPU),    cxtnp11)
	cxtnp13 <- setSSBO -< ((shaderSSBOGeomPassGisRangesDataLocation,      sra^.sraGeomPassGisRangesGPU),         cxtnp12)
	cxtnp14 <- setSSBO -< ((shaderSSBOGeomPassBisDataLocation,            sra^.sraGeomPassBisGPU),               cxtnp13)

	-- Upload elems vao and buf.
	--TODO

	let cxt = cxtnp14  -- TODO

	returnA -< cxt
