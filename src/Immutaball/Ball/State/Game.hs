{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Play.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, ScopedTypeVariables #-}

-- | Game state and immutaball state interface.
module Immutaball.Ball.State.Game
	(
		GameRequest(..), giRequest, giGameState, giIBStateContext, grRequest,
		GameResponse(..), goGameEvents, goGameState, goIBStateContext, grGameEvents,
		GameEvent(..), AsGameEvent(..),
		stepGame,
		renderBall,
		renderBallSetup,
		stepGameInput,
		stepGameInputMovement,
		stepGameInputEvents,
		stepGameClock,
		stepGameClockDebugFreeCamera
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Int
import Data.Maybe

import Control.Arrow
import Control.Lens
import Data.Array.IArray
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types
import qualified SDL.Raw.Enum as Raw

import Immutaball.Ball.Game
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.Math
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Video
import Immutaball.Share.Wire

-- TODO: implement.

data GameRequest = GameRequest {
	_giRequest        :: Request,
	_giGameState      :: GameState,
	_giIBStateContext :: IBStateContext
}
makeLenses ''GameRequest
grRequest :: Lens' GameRequest Request
grRequest = giRequest

data GameResponse = GameResponse {
	_goGameEvents     :: [GameEvent],
	_goGameState      :: GameState,
	_goIBStateContext :: IBStateContext
}
--makeLenses ''GameResponse

data GameEvent =
	  NewGameMode GameMode
	-- | PlaceholderEvent
makeLenses ''GameResponse
makeClassyPrisms ''GameEvent

grGameEvents :: Lens' GameResponse [GameEvent]
grGameEvents = goGameEvents

-- | Step the game.
-- TODO: finish implementing.
stepGame :: Wire ImmutaballM GameRequest GameResponse
stepGame = proc gr -> do
	let request = gr^.giRequest
	let cxtn = gr^.giIBStateContext

	-- Get starting game state.
	let (gsn :: GameState) = (gr^.giGameState)

	-- Handle input (in request).  (With many possibly input requests, stepGameInput itself handles delegation of this.)
	(gsnp1, cxtnp1) <- stepGameInput -< (gsn, request, cxtn)

	-- Step the game on clock request.  (Only one request is clock; handle it here it.)
	mdt <- returnA -< const Nothing ||| Just $ matching _Clock request
	(gsnp2, cxtnp2) <- returnA ||| stepGameClock -< maybe (Left (gsnp1, cxtnp1)) (\dt -> Right (gsn, dt, cxtnp1)) $ mdt

	-- Identify new game state.
	let (gs :: GameState) = gsnp2
	let (cxt :: IBStateContext) = cxtnp2

	-- Return results.
	returnA -< GameResponse {
		_goGameEvents = [],  -- TODO: detect new game mode.
		_goGameState = gs,
		_goIBStateContext = cxt
	}

renderBall :: Wire ImmutaballM (GameState, IBStateContext) IBStateContext
renderBall = proc (gs, cxtn) -> do
	hasInit <- delay False -< returnA True
	cxtnp1 <- arr snd ||| renderBallSetup -< if' hasInit Left Right (gs, cxtn)

	let sdlGL1' = liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)

	-- Render the ball.
	(mballElemVaoVboEbo, cxtnp2) <- getBallElemVaoVboEbo -< cxtnp1
	let ballElemVaoVboEbo = fromMaybe (error "Internal error: renderBall expected elem vao and buf to be present, but it's missing!") mballElemVaoVboEbo
	let (ballElemVao, _ballElemVbo, _ballElemEbo) = ballElemVaoVboEbo

	-- TODO: set the transformation matrix explicitly.  For now we take
	-- advantage of the fact that the transformation matrix hasn't been updated
	-- since the scene was drawn (before the ball was), so just conveniently
	-- implicitly re-use the transformation matrix in the meantime.

	let (renderBallDirect :: GLIOF ()) = do
		-- Tell the shaders to disable the scene data.
		GLUniform1i shaderEnableSceneDataLocation falseAsIntegral ()
		-- Tell the shaders we are drawing the ball right now.
		GLUniform1i shaderEnableBallDataLocation trueAsIntegral ()

		-- Tell the shaders the ball radius.
		let (ballRadiusf :: GLfloat) = realToFrac $ gs^.gsBallRadius
		GLUniform1f shaderBallRadiusLocation ballRadiusf ()
		-- Tell the shaders the ball num triangles.
		let (ballNumTriangles :: Integer) = cxtnp2^.ibContext.ibStaticConfig.x'cfgBallTriangles
		GLUniform1i shaderBallNumTrianglesLocation (fromIntegral ballNumTriangles) ()
		-- Tell the shaders the ball position.
		let (ballPosX :: GLfloat, ballPosY :: GLfloat, ballPosZ :: GLfloat) = (realToFrac $ gs^.gsBallPos.x3, realToFrac $ gs^.gsBallPos.y3, realToFrac $ gs^.gsBallPos.z3)
		GLUniform3f shaderBallPosLocation ballPosX ballPosY ballPosZ ()
		-- Tell the shaders the ball rotation.
		let (ballRotX :: GLfloat, ballRotY :: GLfloat, ballRotZ :: GLfloat) = (realToFrac $ gs^.gsBallRot.x3, realToFrac $ gs^.gsBallRot.y3, realToFrac $ gs^.gsBallRot.z3)
		GLUniform3f shaderBallRotLocation ballRotX ballRotY ballRotZ ()

		-- Tell the shaders to render the ball's triangles' vertices.  The
		-- shader for now will just directly draw a basic ball.
		-- (TODO: support more than just basic ball.)
		GLBindVertexArray ballElemVao ()

		-- Use the vao to tell the shader to draw the geometry.
		--let (ballNumTriangles :: Integer) = cxtnp2^.ibContext.ibStaticConfig.x'cfgBallTriangles
		GLDrawArrays GL_TRIANGLES 0 (fromIntegral (3 * ballNumTriangles)) ()

		-- Unbind the VAO.
		GLBindVertexArray 0 ()
	-- We're not using textures for the basic ball; the shader can calculate the color.
	let (assignTextures :: GLIOF ()) = return ()
	-- Enable transparency for the ball.
	let (isAlpha :: Bool) = True
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

	let (doRenderBall :: GLIOF ()) = do
		alphaSetup
		assignTextures
		renderBallDirect
		alphaFinish
	() <- monadic -< sdlGL1' doRenderBall

	let cxt = cxtnp2
	returnA -< cxt

renderBallSetup :: Wire ImmutaballM (GameState, IBStateContext) IBStateContext
renderBallSetup = proc (_gs0, cxtn) -> do
	-- A new game state hasn't had its ball render setup performed yet.
	-- Perform setup.

	-- First, set up ball elem vao and vbo.  See if a previous setup has
	-- already done it.  Race conditions are okay here, since setting the ball
	-- elem vao would discard an old elem vao in a race properly.
	-- (Alternatively, we could also always set it up here since it would only
	-- happen on a new game state, but checking probably saves a bit of
	-- performance for uploading the VAO.)
	(mballElemVaoVboEbo, cxtnp1) <- getBallElemVaoVboEbo -< cxtn

	-- Set up the data, ballElemVaoVboEboDataGPU.
	let (ballNumTriangles :: Integer) = cxtnp1^.ibContext.ibStaticConfig.x'cfgBallTriangles
	let (ballElemVaoVboEboData :: Array Int32 Int32) = genArray (0, 3 * fromIntegral ballNumTriangles - 1) $ \idx -> idx
	let (ballElemVaoVboEboDataGPU :: GLData) = gpuEncodeArray ballElemVaoVboEboData

	-- Upload ball elems vao and buf.  (Just returnA the cxtnp1 if there's already a ball VAO.)
	cxtnp2 <- arr (^._3) ||| setBallElemVaoVboEbo -< if' (isJust mballElemVaoVboEbo) Left Right (ballElemVaoVboEboDataGPU, True, cxtnp1)

	let cxt = cxtnp2
	returnA -< cxt

-- | Handle input.
stepGameInput :: Wire ImmutaballM (GameState, Request, IBStateContext) (GameState, IBStateContext)
stepGameInput = proc (gsn, request, cxtn) -> do
	-- Handle forward, right, etc., movement.
	(gsnp1, cxtnp1) <- stepGameInputMovement -< (gsn, request, cxtn)

	-- Handle clicking to start playing, etc.
	(gsnp2, cxtnp2) <- stepGameInputEvents -< (gsnp1, request, cxtnp1)

	-- Identify output.
	let cxt = cxtnp2
	let gs = gsnp2

	-- Return.
	returnA -< (gs, cxt)

stepGameInputMovement :: Wire ImmutaballM (GameState, Request, IBStateContext) (GameState, IBStateContext)
stepGameInputMovement = proc (gsn, request, cxtn) -> do
	-- Movement.
	let update = case request of
		(Keybd char down) ->
			if' (char == cxtn^.ibNeverballrc.keyForward)                  (gsInputState.ginsForwardOn  .~ down) .
			if' (char == cxtn^.ibNeverballrc.keyBackward)                 (gsInputState.ginsBackwardOn .~ down) .
			if' (char == cxtn^.ibNeverballrc.keyLeft)                     (gsInputState.ginsLeftOn     .~ down) .
			if' (char == cxtn^.ibNeverballrc.keyRight)                    (gsInputState.ginsRightOn    .~ down) .
			if' (char == cxtn^.ibContext.ibStaticConfig.x'cfgVertUpKey)   (gsInputState.ginsVertUpOn   .~ down) .
			if' (char == cxtn^.ibContext.ibStaticConfig.x'cfgVertDownKey) (gsInputState.ginsVertDownOn .~ down) $
			id
		_ -> id

	let gsnp1 = gsn & update

	-- Toggle debug camera view.
	let freeCamToggleUpdate = if' (not $ cxtn^.ibContext.ibStaticConfig.x'cfgDebugFreeCamera) id $
		case request of
			(Keybd char True) ->
				if' (char == cxtn^.ibContext.ibStaticConfig.x'cfgFreeCameraToggleKey) (gsDebugState.gdsCameraDebugOn %~ not) $
				id
			_ -> id

	let gsnp2 = gsnp1 & freeCamToggleUpdate

	-- Free camera aiming.
	-- mouse sense gives int in pixels for a full circle rotation, except we also scale rotation amount by curMouseSpeed (the 2 speeds are multiplied (although neverballrc is reciprocal)).
	let (curMouseSpeed :: Double) = 4.0
	let (curMouseSense :: Int) = cxtn^.ibNeverballrc.mouseSense
	let updateFreeAim = if' (not $ gsnp2^.gsDebugState.gdsCameraDebugOn) id $ case request of
		(Point _x _y dx' dy') ->
			-- SDL for me reported inverted dy (i.e. SDL's 0,0 at the top-left corner); so invert dy.
			let dx =  dx' in
			let dy = -dy' in

			let (dxd :: Double) = fromIntegral dx in
			let (dyd :: Double) = fromIntegral dy in

			let (drawCirclesX :: Double) = dxd / fromIntegral curMouseSense in
			let (drawCirclesY :: Double) = dyd / fromIntegral curMouseSense in

			let (drawRadiansX :: Double) = tau * drawCirclesX in
			let (drawRadiansY :: Double) = tau * drawCirclesY in

			let (dradiansX :: Double) = curMouseSpeed * drawRadiansX in
			let (dradiansY :: Double) = curMouseSpeed * drawRadiansY in

			(gsDebugState.gdsCameraAimRightRadians %~ (+ dradiansX)) .
			(gsDebugState.gdsCameraAimUpRadians    %~ (+ dradiansY)) .
			id
		_ -> id

	-- Cap: let net view be capped later in 'mkGameStateAnalysis', since there might already be a vertical aim to target.
	-- But also cap here at twice the radius to allow full movement.
	let updateCapFreeAimUp = gsDebugState.gdsCameraAimUpRadians %~ (min (2 * tau/2) . max (-2 * tau/2))

	let gsnp3 = gsnp2 & updateCapFreeAimUp . updateFreeAim

	-- Tilt the world if playing.
	-- The config for curMouseSense is interpreted to mean the number of pixels
	-- needed to traverse the entire tilt range (2 * x'cfgMaxTilt).
	let (mouseTiltModifier :: Double) = 1.0  -- Don't change mouse sensitivity for tilt.
	let (mouseSensitivity :: Int) = cxtn^.ibNeverballrc.mouseSense
	let (maxTilt :: Double) = cxtn^.ibContext.ibStaticConfig.x'cfgMaxTilt
	let updateTiltBase = if' (not . isPlaying $ gsnp1^.gsGameMode) id $ case request of
		(Point _x _y dx' dy') ->
			-- SDL for me reported inverted dy (i.e. SDL's 0,0 at the top-left corner); so invert dy.
			let dx =  dx' in
			let dy = -dy' in

			let (dxd :: Double) = fromIntegral dx in
			let (dyd :: Double) = fromIntegral dy in

			let (dTiltRangesX :: Double) = dxd / fromIntegral mouseSensitivity in
			let (dTiltRangesY :: Double) = dyd / fromIntegral mouseSensitivity in

			let (dRawRadiansX :: Double) = tau * dTiltRangesX in
			let (dRawRadiansY :: Double) = tau * dTiltRangesY in

			let (dRadiansX :: Double) = mouseTiltModifier * dRawRadiansX in
			let (dRadiansY :: Double) = mouseTiltModifier * dRawRadiansY in

			(gsGravityState.gravsTiltRightRadians   %~ (+ dRadiansX)) .
			(gsGravityState.gravsTiltForwardRadians %~ (+ dRadiansY)) .
			id
		_ -> id
	let updateTiltCap =
		(gsGravityState.gravsTiltRightRadians   %~ (min maxTilt . max (-maxTilt))) .
		(gsGravityState.gravsTiltForwardRadians %~ (min maxTilt . max (-maxTilt))) .
		id
	let updateTilt = updateTiltCap <<< updateTiltBase

	let gsnp4 = gsnp3 & updateTilt

	-- Identify output.
	let gs = gsnp4
	let cxt = cxtn

	-- Return.
	returnA -< (gs, cxt)

-- | Handle clicking to start playing, etc.
-- TODO: generate event for new game mode.
stepGameInputEvents :: Wire ImmutaballM (GameState, Request, IBStateContext) (GameState, IBStateContext)
stepGameInputEvents = proc (gsn, request, cxtn) -> do
	-- Start playing?
	let update = if' (gsn^.gsGameMode /= Intermission) id $ case request of
		(Click _button@Raw.SDL_BUTTON_LEFT _down@True) -> (gsGameMode .~ Playing)
		_ -> id

	let gsnp1 = gsn & update

	-- Identify output.
	let gs = gsnp1
	let cxt = cxtn

	-- Return.
	returnA -< (gs, cxt)

-- | Step a frame of the game state on clock.
stepGameClock :: Wire ImmutaballM (GameState, Double, IBStateContext) (GameState, IBStateContext)
stepGameClock = proc (gsn, dt, cxtn) -> do
	-- Step debug camera.
	(gsnp1, cxtnp1) <- returnA ||| stepGameClockDebugFreeCamera -<
		if' (not $ gsn^.gsDebugState.gdsCameraDebugOn)
			(Left  (gsn, cxtn))
			(Right (gsn, dt, cxtn))

	-- Advance time elapsed if in play state.
	let cxtnp2 = cxtnp1
	let gsnp2 = gsnp1 &
		if' (not . isPlaying $ gsnp1^.gsGameMode)
			(id)
			(gsTimeElapsed %~ (+ dt))

	-- Identify output.
	let gs = gsnp2
	let cxt = cxtnp2

	-- Return.
	returnA -< (gs, cxt)

-- | Step the debug free camera.
--
-- Check whether this mode is enabled before using this wire.
stepGameClockDebugFreeCamera :: Wire ImmutaballM (GameState, Double, IBStateContext) (GameState, IBStateContext)
stepGameClockDebugFreeCamera = proc (gsn, dt, cxtn) -> do
	let gsa    = mkGameStateAnalysis cxtn gsn
	let netMov = gsa^.gsaNetRightForwardJump
	let (netRight, netForward, netJump) = netMov

	-- FRP architecture note: we have access to both 1) local state (FRP) and
	-- 2) the  relatively global game state.  If we wanted, we could use FRP to
	-- make a wire (time-varying value) that outputs the free camera position
	-- offset value, and just unconditionally set the game state, disregarding
	-- its input.  We could also instead not use FRP but just calculate the
	-- next state.
	--
	-- To demonstrate capabilities, I instead take a third, hybrid approach:
	-- both.  Use FRP to calculate a diff locally, and then apply that diff to
	-- the state (relatively) globally.

	let (unrotatedNetMovementVec :: Vec3 Double) = Vec3 (fromIntegral netRight) (fromIntegral netForward) (fromIntegral netJump)
	let (netMovementVec :: Vec3 Double) = tilt3ySimple (v3normalize $ (gsa^.gsaView.mviewTarget) `minusv3` (gsa^.gsaView.mviewPos)) `mv3` unrotatedNetMovementVec
	-- relativeNetMovementVec is not needed: netMovementVec _already_ includes the free camera rotation, not just the original orientation in the level file.
	--let (relativeNetMovementVec :: Vec3 Double) = aimVert3DSimple (Just $ 0.99*tau) (gsn^.gsDebugState.gdsCameraAimUpRadians) . aimHoriz3DSimple (gsn^.gsDebugState.gdsCameraAimRightRadians) $ netMovementVec
	let netMovementVec' = if' (not freeCameraRelative) unrotatedNetMovementVec $ netMovementVec
	posOffset <- integrate zv3 -< (dt * freeCameraSpeed) `sv3` netMovementVec'

	-- FRP (local).
	posOffsetDiff <- differentiate -< posOffset
	-- State (global).
	let posOffsetUpdate = gsDebugState.gdsCameraPosOffset %~ (+ posOffsetDiff)
	let gsnp1 = gsn & posOffsetUpdate

	-- Identity output.
	let gs = gsnp1
	let cxt = cxtn

	-- Return.
	returnA -< (gs, cxt)

	where
		-- Units per second.
		freeCameraSpeed :: Double
		freeCameraSpeed = 2.0

		-- Relative to aim?  This is normal behavior, but temporarily setting
		-- this to False is an option.
		freeCameraRelative :: Bool
		freeCameraRelative = True
