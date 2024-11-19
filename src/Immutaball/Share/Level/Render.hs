{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Render.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, ScopedTypeVariables #-}

-- | TODO: split shared render into shared and ball render.
--
-- Currently this uses ball game state.
module Immutaball.Share.Level.Render
	(
		renderLevel,
		renderSetupNewLevel,
		renderScene,
		renderGeomPass
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad
import Data.Int
import Data.Ix
import Data.Maybe

import Control.Lens
import Data.Array.IArray
import qualified Data.Map.Lazy as M
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types

import Immutaball.Ball.Game
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.Level.Analysis
import Immutaball.Share.Level.Base
import Immutaball.Share.Math
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Video
import Immutaball.Share.Wire

-- | TODO: implement.
renderLevel :: Wire ImmutaballM ((MView, SolWithAnalysis, GameState), IBStateContext) IBStateContext
renderLevel = proc ((camera, swa, gs), cxtn) -> do
	-- Set up.
	let levelPath = swa^.swaMeta.smPath
	(mlastLevelPath, cxtnp1) <- setCurrentlyLoadedSOL -< (levelPath, cxtn)
	let newLevel = not $ Just levelPath == mlastLevelPath
	cxtnp2 <- returnA ||| renderSetupNewLevel -< if' (not newLevel) (Left cxtnp1) (Right (swa, cxtnp1))

	-- Render the scene.
	cxtnp3 <- renderScene -< ((camera, swa, gs), cxtnp2)

	-- Return the state context.

	let cxt = cxtnp3
	returnA -< cxt

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
	cxtnp15 <- setElemVAOAndBuf -< (sra^.sraGcArrayGPU, True, cxtnp14)

	-- Pre-initialize the transformation matrix with the identity.
	cxtnp16 <- setTransformation -< (identity4, cxtnp15)

	-- Return the state context.

	let cxt = cxtnp16

	returnA -< cxt

-- | After setup, render the scene.
renderScene :: Wire ImmutaballM ((MView, SolWithAnalysis, GameState), IBStateContext) IBStateContext
renderScene = proc ((camera, swa, gs), cxtn) -> do
	-- Set up the transformation matrix.
	let mat = transformationMatrix camera
	cxtnp1 <- setTransformation -< (mat, cxtn)

	-- Render the scene.
	let
		sol :: Sol
		sol = swa^.swaSol

		sa :: SolAnalysis
		sa = swa^.swaSa

		sra :: SolRenderAnalysis
		sra = sa^.saRenderAnalysis
	let _unused = (sol)

	let
		ourFlatten :: (Int32, (SolWithAnalysis, GameState, Bool, GeomPass)) -> (Int32, SolWithAnalysis, GameState, Bool, GeomPass)
		ourFlatten (a, (b, c, d, e)) = (a, b, c, d, e)
	let geomPasses = map ourFlatten . zip [0..] $ map (\gp -> (swa, gs, False, gp)) (sra^.sraOpaqueGeoms) ++ map (\gp -> (swa, gs, True, gp)) (sra^.sraTransparentGeoms)
	cxtnp2 <- foldlA renderGeomPass -< (cxtnp1, geomPasses)

	-- Return the state context.
	let cxt = cxtnp2

	returnA -< cxt

	where
		transformationMatrix :: MView -> Mat4 Double
		transformationMatrix view_ = viewMat view_

-- | Render a partition of the level geometry, so that we can handle processing up to 16 textures at a time.
renderGeomPass :: Wire ImmutaballM (IBStateContext, (Int32, SolWithAnalysis, GameState, Bool, GeomPass)) IBStateContext
renderGeomPass = proc (cxtn, (geomPassIdx, swa, _gs, isAlpha, gp)) -> do
	-- Setup.
	let sdlGL1'_ = sdlGL1 (cxtn^.ibContext.ibSDLManagerHandle)
	let sdlGL1' = liftIBIO . sdlGL1'_

	-- Render the geom pass.

	-- Render all 16 mtrls in the geom pass.
	(mtrlsMetaReversed :: [((WidthHeightI, GLuint), MtrlMeta)], cxtnp1) <-
		foldrA cachingRenderMtrlAccumReversed -< (([], cxtn), map (\mi -> (swa^.swaSol, mi)) (elems (gp^.gpMv)))
	let mtrlsMeta = reverse mtrlsMetaReversed
	let (mtrlsGlTextures :: [GLuint]) = map (fst >>> snd) mtrlsMeta
	let (assignTextures :: GLIOF ()) = do
		forM_ (zip [0..] mtrlsGlTextures) $ \(idx, glTexture) -> do
			case flip M.lookup numToGL_TEXTUREi idx of
				Nothing -> return ()
				Just gl_TEXTUREi -> do
					-- Apparently actually we set GL_TEXTUREi for the texture()
					-- GLSL call, not the texture name.  So set the i used in
					-- GL_TEXTUREi as the value, not the texture identifier.
					--GLUniform1i (fromIntegral idx) (fromIntegral texture) ()
					GLUniform1i (fromIntegral idx) (fromIntegral idx) ()

					--GLActiveTexture GL_TEXTUREi ()
					GLActiveTexture gl_TEXTUREi ()
					GLBindTexture GL_TEXTURE_2D glTexture ()

	-- Render all geometry in this geom pass.
	(melemVAOAndBuf, cxtnp2) <- getElemVAOAndBuf -< cxtnp1
	let elemVAOAndBuf = fromMaybe (error "Internal error: renderGeomPass expected elem vao and buf to be present, but it's missing!") melemVAOAndBuf
	let (elemVAO, _elemBuf) = elemVAOAndBuf
	let (renderGeomPassScene :: GLIOF ()) = do
		-- Tell the shaders to enable the scene data.
		GLUniform1i shaderEnableSceneDataLocation trueAsIntegral ()

		-- Tell the shaders what geom pass to use.
		GLUniform1i (shaderSceneGeomPassIdxLocation) (fromIntegral geomPassIdx) ()

		-- Tell the shaders to render this pass's geometries.  It will handle the rest; it already has the geom pass data we uploaded upon setup.
		GLBindVertexArray elemVAO ()

		-- Use the vao to tell the shader to draw the geometry.
		let numGpGis = rangeSize . bounds $ gp^.gpGis
		GLDrawArrays GL_TRIANGLES 0 (fromIntegral (3 * numGpGis)) ()

		-- Unbind the VAO.
		GLBindVertexArray 0 ()

	let (alphaSetup :: GLIOF ()) = do
		if isAlpha
			then do
				GLEnable GL_BLEND ()
				GLBlendEquationSeparate GL_FUNC_ADD GL_FUNC_ADD ()
				GLBlendFuncSeparate GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA GL_ONE GL_ZERO ()

				GLEnable GL_DEPTH_TEST ()
				GLDepthMask GL_FALSE ()
			else do
				GLDisable GL_BLEND ()

				GLEnable GL_DEPTH_TEST ()
				GLDepthMask GL_TRUE ()
	let (alphaFinish :: GLIOF ()) = do
		if isAlpha
			then do
				GLDisable GL_BLEND ()

				GLDepthMask GL_TRUE ()
			else do
				GLDisable GL_BLEND ()

				GLDepthMask GL_TRUE ()

	-- Now aggregate the rendering to render the geom pass.
	let (renderGeomPassGL :: GLIOF ()) = do
		alphaSetup
		assignTextures
		renderGeomPassScene
		alphaFinish
	() <- monadic -< sdlGL1' renderGeomPassGL

	-- Return the state context.
	let cxt = cxtnp2
	returnA -< cxt

	where
		-- | Add 2 variants: lookup sol mtrl for path, and also accumulate results in a list.
		cachingRenderMtrlAccumReversed :: Wire ImmutaballM ((LevelIB, Int32), ([((WidthHeightI, GLuint), MtrlMeta)], IBStateContext)) ([((WidthHeightI, GLuint), MtrlMeta)], IBStateContext)
		cachingRenderMtrlAccumReversed = proc ((sol, mi), (accum_, cxtn)) -> do
			let mtrl = (sol^.solMv) ! mi
			let mtrlPath = mtrl^.mtrlF
			(glTextureMeta, cxtnp1) <- cachingRenderMtrl -< (mtrlPath, cxtn)
			returnA -< (glTextureMeta:accum_, cxtnp1)
