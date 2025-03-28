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

-- TODO DEBUG:
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO

import Debug.Trace as D--------------------------------- TODO DEBUG

-- | TODO: implement.
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
	--() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG0: renderSetupNewLevel: sra is " ++ show sra) ()
	let solGeoms = map (^.geomMi) $ (elems (swa^.swaSol.solGv))
	() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG0: renderSetupNewLevel: sol path is " ++ show (swa^.swaMeta.smPath)) ()
	() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG1: renderSetupNewLevel: all sol geom mis is " ++ show solGeoms) ()
	() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG2: renderSetupNewLevel: all sol mv is " ++ show (swa^.swaSol.solMv)) ()
	() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG3") ()

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
	--let mat = transformationMatrix camera_
	mat <- arr $ uncurry3 transformationMatrix -< (cxtn, gs, camera_)
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
		uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
		uncurry3 f (a, b, c) = f a b c

		transformationMatrix :: IBStateContext -> GameState -> MView -> Mat4 Double
		transformationMatrix cxt gs view_ = worldToGL <> rescaleDepth depthScale 0 <> viewMat' viewCollapse view_ <> tilt
			where
				x'cfg = cxt^.ibContext.ibStaticConfig
				depthScale = x'cfg^.x'cfgDepthScale
				viewCollapse = x'cfg^.x'cfgViewCollapse

				-- Translate to the ball (negated actually), rotate by gsCameraAngle, tilt3z, and untranslate.
				sq_ x = x*x
				tilt =
					translate3 (gs^.gsBallPos) <>     -- Finally, undo the first translation.
					tilt3z upVec <>                   -- Tilt the world.
					rotatexy (-gs^.gsCameraAngle) <>  -- Rotate camera.
					translate3 (-gs^.gsBallPos)       -- First, go to ball (negated actually).
				upVec = Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec^.xy3.r2))
				--upVec = D.trace ("DEBUG0: " ++ show (tilt3z $ fix $ \upVec_ -> Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec_^.xy3.r2)))) $ Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec^.xy3.r2))
				--fix f = let x = f x in x
				--upVec = D.trace ("DEBUG0: upVec: " ++ show (fix $ \upVec_ -> Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec_^.xy3.r2)))) $ Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec^.xy3.r2))
				-- FIXME: inversions.  tilt3z when normalizing z seems to be inverting x and y?
				-- TODO this part is fixed; now clean up, maybe once you fix the remaining orientation issues.
				--fix f = let x = f x in x
				--upVec = D.trace (let upVec__ = (fix $ \upVec_ -> Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec_^.xy3.r2))) in "DEBUG0:\n\tupVec: " ++ show upVec__ ++ "\n\ttilt3z upVec: " ++ show (tilt3z upVec__)) $ Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec^.xy3.r2))
				--upVec = D.trace (let upVec__ = (fix $ \upVec_ -> Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec_^.xy3.r2))) in "DEBUG0:\n\tupVec: " ++ show upVec__ ++ "\n\ttilt3z upVec: " ++ show (tilt3z upVec__) ++ "\n\tv3normalize upVec: " ++ show (v3normalize upVec__)) $ Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec^.xy3.r2))
				--upVec = D.trace ("DEBUG0: cameraAngle: " ++ show (gs^.gsCameraAngle)) $ Vec3 (sin $ gs^.gsGravityState.gravsTiltRightRadians) (sin $ gs^.gsGravityState.gravsTiltForwardRadians) (sqrt $ 1 - sq_ (upVec^.xy3.r2))

-- | Render a partition of the level geometry, so that we can handle processing up to 16 textures at a time.
renderGeomPass :: Wire ImmutaballM (IBStateContext, (Int32, SolWithAnalysis, GameState, Bool, GeomPass)) IBStateContext
renderGeomPass = proc (cxtn, (geomPassIdx, swa, _gs, isAlpha, gp)) -> do
	-- Setup.
	let sdlGL1'_ = sdlGL1 (cxtn^.ibContext.ibSDLManagerHandle)
	let sdlGL1' = liftIBIO . sdlGL1'_

	-- Render the geom pass.

	-- Render all 16 mtrls in the geom pass.
	(mtrlsMeta :: [((WidthHeightI, GLuint), MtrlMeta)], cxtnp1) <-
		foldrA cachingRenderMtrlAccum -< (([], cxtn), map (\mi -> (swa^.swaSol, mi)) (elems (gp^.gpMv)))
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
	(melemVaoVboEbo, cxtnp2) <- getElemVaoVboEbo -< cxtnp1
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
	let cxt = cxtnp2
	returnA -< cxt

	where
		-- | Add 2 variants: lookup sol mtrl for path, and also accumulate results in a list.
		cachingRenderMtrlAccum :: Wire ImmutaballM ((LevelIB, Int32), ([((WidthHeightI, GLuint), MtrlMeta)], IBStateContext)) ([((WidthHeightI, GLuint), MtrlMeta)], IBStateContext)
		cachingRenderMtrlAccum = proc ((sol, mi), (accum_, cxtn)) -> do
			let mtrl = (sol^.solMv) ! mi
			let mtrlPath = mtrl^.mtrlF
			(glTextureMeta, cxtnp1) <- cachingRenderMtrl -< (mtrlPath, cxtn)
			returnA -< (glTextureMeta:accum_, cxtnp1)
