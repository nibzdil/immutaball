{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Render.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, ScopedTypeVariables #-}

-- | Level rendering.
module Immutaball.Ball.Level.Render
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
import Immutaball.Ball.State.Game  -- rendererTransformationMatrix
import Immutaball.Share.Config
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

renderLevel :: Wire ImmutaballM ((MView, SolWithAnalysis, GameState), IBStateContext) IBStateContext
renderLevel = proc ((camera_, swa, gs), cxtn) -> do
	-- Set up.
	let levelPath = swa^.swaMeta.smPath
	(mlastLevelPath, cxtnp1) <- setCurrentlyLoadedSOL -< (levelPath, cxtn)
	let newLevel = not $ Just levelPath == mlastLevelPath
	cxtnp2 <- returnA ||| renderSetupNewLevel -< if' (not newLevel) (Left cxtnp1) (Right (swa, cxtnp1))

	-- Render the scene.
	cxtnp3 <- renderScene -< ((camera_, swa, gs), cxtnp2)

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
	cxtnp15 <- setSSBO -< ((shaderSSBOTexcoordsDoubleDataLocation,        sra^.sraTexcoordsDoubleDataGPU),       cxtnp14)

	-- Upload elems vao and buf.
	cxtnp16 <- setElemVaoVboEbo -< (sra^.sraGcArrayGPU, True, cxtnp15)

	-- Pre-initialize the transformation matrix with the identity.
	cxtnp17 <- setTransformation -< (identity4, cxtnp16)

	-- Approximate disabling OpenGL clipping by depth (by our transformed y coordinates, which transform into OpenGL z depth coordinates).
	-- Also other miscellaneous OpenGL setup.
	let sdlGL1'_ = sdlGL1 (cxtnp17^.ibContext.ibSDLManagerHandle)
	let sdlGL1' = liftIBIO . sdlGL1'_
	() <- monadic -< sdlGL1' $ do
		-- TODO FIXME: this doesn't seem to actually stop clipping for depth
		-- values outside a certain range.
		--
		-- To investigate, disable rescaleDepth, try retour de force level 2,
		-- enable free camera, and rotate the camera until you can see the
		-- level.  Move toward that location, and then you can see that the
		-- distance from the camera needs to be only within a certain range.
		--
		-- So instead we'll just use rescaleDepth.
		GLDepthRange (cxtnp17^.ibContext.ibStaticConfig.x'glNearVal) (cxtnp17^.ibContext.ibStaticConfig.x'glFarVal) ()
	-- Other miscellaneous OpenGL setup.
	() <- monadic -< sdlGL1' $ do
		--GLPlaceholder GL_VAL ()
		pure ()

	-- Return the state context.

	let cxt = cxtnp17

	returnA -< cxt

-- | After setup, render the scene.
renderScene :: Wire ImmutaballM ((MView, SolWithAnalysis, GameState), IBStateContext) IBStateContext
renderScene = proc ((camera_, swa, gs), cxtn) -> do
	-- Set up the transformation matrix.
	--let mat = rendererTransformationMatrix camera_
	mat <- arr $ uncurry3 rendererTransformationMatrix -< (cxtn, gs, camera_)
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
		ourFlatten :: (Int32, (MView, SolWithAnalysis, GameState, Bool, GeomPass)) -> (Int32, MView, SolWithAnalysis, GameState, Bool, GeomPass)
		ourFlatten (a, (b, c, d, e, f)) = (a, b, c, d, e, f)
	let geomPasses = map ourFlatten . zip [0..] $ map (\gp -> (camera_, swa, gs, False, gp)) (sra^.sraOpaqueGeoms) ++ map (\gp -> (camera_, swa, gs, True, gp)) (sra^.sraTransparentGeoms)
	cxtnp2 <- foldlA renderGeomPass -< (cxtnp1, geomPasses)

	-- Return the state context.
	let cxt = cxtnp2

	returnA -< cxt

-- | Render a partition of the level geometry, so that we can handle processing up to 16 textures at a time.
renderGeomPass :: Wire ImmutaballM (IBStateContext, (Int32, MView, SolWithAnalysis, GameState, Bool, GeomPass)) IBStateContext
renderGeomPass = proc (cxtn, (geomPassIdx, camera_, swa, gs, isAlpha, gp)) -> do
	-- Setup.
	let sdlGL1'_ = sdlGL1 (cxtn^.ibContext.ibSDLManagerHandle)
	let sdlGL1' = liftIBIO . sdlGL1'_

	-- Render the geom pass.

	-- First, if the body is on a path, update the transformation matrix to
	-- reflect the translation on the path.  TODO: currently we only support
	-- translations on paths here.
	let bi = gp^.gpBi
	let body = (swa^.swaSol.solBv) ! bi
	let (moriginPath :: Maybe Int32) = do
		let originPath = body^.bodyP0
		guard . inRange (bounds $ swa^.swaSol.solPv) $ originPath
		return $ originPath
	let (pathTransformationAtTimeMap :: M.Map Int32 (Double -> Mat4 Double)) = swa^.swaSa.saOtherAnalysis.soaPathTransformationAtTime.fakeEOS
	let (mpathTransformation :: Maybe (Mat4 Double)) = do
		originNode <- moriginPath
		transformationAtTime <- flip M.lookup pathTransformationAtTimeMap $ originNode
		-- TODO: update pathsTimeElapsed, then invert the comments in the next 2 lines.
		pathTimeElapsed <- return $ gs^.gsTimeElapsed
		--pathTimeElapsed <- flip M.lookup (gs^.gsPathState.psPathsTimeElapsed) originNode
		let transformation = transformationAtTime $ pathTimeElapsed
		return transformation
	mat <- arr $ uncurry3 rendererTransformationMatrix -< (cxtn, gs, camera_)
	let (mnetBodyTransformation :: Maybe (Mat4 Double)) = do
		pathTransformation <- mpathTransformation
		return $ mat <> pathTransformation
	cxtnp1 <- returnA ||| setTransformation -< deconsMaybe (Left cxtn) (\pathTransformation -> Right (pathTransformation, cxtn)) $ mnetBodyTransformation

	-- Render all 16 mtrls in the geom pass.
	(mtrlsMeta :: [((WidthHeightI, GLuint), MtrlMeta)], cxtnp2) <-
		foldrA cachingRenderMtrlAccum -< (([], cxtnp1), map (\mi -> (swa^.swaSol, mi)) (elems (gp^.gpMv)))
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
	(melemVaoVboEbo, cxtnp3) <- getElemVaoVboEbo -< cxtnp2
	let elemVaoVboEbo = fromMaybe (error "Internal error: renderGeomPass expected elem vao and buf to be present, but it's missing!") melemVaoVboEbo
	let (elemVao, _elemVbo, _elemEbo) = elemVaoVboEbo
	let (renderGeomPassScene :: GLIOF ()) = do
		-- Tell the shaders to enable the scene data.
		GLUniform1i shaderEnableSceneDataLocation trueAsIntegral ()
		-- Tell the shaders we are not drawing the ball right now.
		GLUniform1i shaderEnableBallDataLocation falseAsIntegral ()

		-- Tell the shaders what geom pass to use.
		GLUniform1i (shaderSceneGeomPassIdxLocation) (fromIntegral geomPassIdx) ()

		-- Tell the shaders to render this pass's geometries.  It will handle the rest; it already has the geom pass data we uploaded upon setup.
		GLBindVertexArray elemVao ()

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
	let cxt = cxtnp3
	returnA -< cxt

	where
		-- | Add 2 variants: lookup sol mtrl for path, and also accumulate results in a list.
		cachingRenderMtrlAccum :: Wire ImmutaballM ((LevelIB, Int32), ([((WidthHeightI, GLuint), MtrlMeta)], IBStateContext)) ([((WidthHeightI, GLuint), MtrlMeta)], IBStateContext)
		cachingRenderMtrlAccum = proc ((sol, mi), (accum_, cxtn)) -> do
			let mtrl = (sol^.solMv) ! mi
			let mtrlPath = mtrl^.mtrlF
			(glTextureMeta, cxtnp1) <- cachingRenderMtrl -< (mtrlPath, cxtn)
			returnA -< (glTextureMeta:accum_, cxtnp1)
