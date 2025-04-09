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
		stepGameClockDebugFreeCamera,
		stepGameBallPhysics,
		physicsBallAdvance,
		physicsBallAdvanceStationary,
		physicsBallAdvanceGhostly,
		physicsBallAdvanceBruteForce,
		physicsBallAdvanceBruteForceCompute

		-- * Local utils.
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad
import Data.Int
import Data.Foldable
import Data.List
import qualified Data.Map.Lazy as M
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
import Immutaball.Share.Level
import Immutaball.Share.Level.Analysis
import Immutaball.Share.Math
import Immutaball.Share.SDLManager
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Video
import Immutaball.Share.Wire

import Debug.Trace as D  ---------------------------------------------------------TODO
import Text.Printf

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
		(Click _button@Raw.SDL_BUTTON_LEFT  down) -> (gsInputState.ginsMouseLeftOn  .~ down)
		(Click _button@Raw.SDL_BUTTON_RIGHT down) -> (gsInputState.ginsMouseRightOn .~ down)
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
	let gsa = mkGameStateAnalysis cxtn gsn

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

	-- Respond to left and right mouse down by moving the camera.
	rec
		lastNetMouseRight <- delay 0 -< netMouseRight
		netMouseRight <- returnA -< gsa^.gsaNetMouseRight

		lastIsPlayingState <- delay False -< isPlayingState
		isPlayingState <- returnA -< isPlaying $ gsnp2^.gsGameMode

		foundChange <- hold False -< if' ((lastIsPlayingState == isPlayingState) && (lastNetMouseRight /= netMouseRight)) (Just True) Nothing

	let cameraAngleSpeed = 1.0  -- radians per second  -- TODO: use neverballrc rotate_slow
	let updateCameraAngle = if' (not foundChange || not isPlayingState) id $
		(gsCameraAngle %~ (+ (-fromIntegral netMouseRight) * cameraAngleSpeed * dt)) .
		id
	let gsnp3 = gsnp2 & updateCameraAngle
	let cxtnp3 = cxtnp2

	-- Physically expend dt to advance the ball's position by its velocity,
	-- handling physics collisions.
	(gsnp4, cxtnp4) <- returnAWithoutMiddle ||| stepGameBallPhysics -< if' (not isPlayingState) Left Right $ (gsnp3, dt, cxtnp3)

	-- Identify output.
	let gs = gsnp4
	let cxt = cxtnp4

	-- Return.
	returnA -< (gs, cxt)
	where
		returnAWithoutMiddle = arr $ \(a, _b, c) -> (a, c)

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

-- | Expend dt to step the ball through the physical world.
--
-- Apply gravity and handle collisions.
stepGameBallPhysics :: Wire ImmutaballM (GameState, Double, IBStateContext) (GameState, IBStateContext)
stepGameBallPhysics = proc (gsn, dt, cxtn) -> do
	let gsa = mkGameStateAnalysis cxtn gsn

	let x'cfg = cxtn^.ibContext.ibStaticConfig

	-- Find the gravity vector.
	let gravityVectorUnrotated = (gsa^.gsaUpVec) & z3 %~ negate  -- Mirror on xy plane.
	let gravityVector = rotatexySimple (gsn^.gsCameraAngle) `mv3` gravityVectorUnrotated
	-- Don't apply the gravity vector yet, since we might apply too much dt,
	-- e.g. before the ball rests back on the floor; this could make the ball
	-- bouncy, especially depending on the frame rate.
	{-
	let gravityAcceleration = x'cfg^.x'cfgGravity
	let updateGravity =
		(gsBallVel %~ (+ (dt * gravityAcceleration) `sv3` gravityVector)) .
		id
	let gsnp1 = gsn & updateGravity
	-}
	let gsnp1 = gsn
	let cxtnp1 = cxtn

	-- Advance the ball through the physical world, handling collisions.
	--
	-- Expend dt to apply its velocity to the position, handling collisions by
	-- applying reflections.
	let (ballPos', ballVel') = physicsBallAdvance x'cfg (gsnp1^.gsSol) (gsnp1^.gsSolAnalysis.saPhysicsAnalysis) (gsnp1^.gsBallRadius) gravityVector dt (gsnp1^.gsBallPos) (gsnp1^.gsBallVel)
	let updateBall =
		(gsBallPos .~ ballPos') .
		(gsBallVel .~ ballVel') .
		id
	let (gsnp2, cxtnp2) = (gsnp1 & updateBall, cxtnp1)

	-- Identify output.
	let gs = gsnp2
	let cxt = cxtnp2

	-- Return.
	returnA -< (gs, cxt)

-- | Expend dt to step the ball through the physical world, handling collisions.
physicsBallAdvance :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> Double -> Vec3 Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvance x'cfg level spa ballRadius gravityVector dt ballPos ballVel = choice_ x'cfg level spa ballRadius gravityVector dt ballPos ballVel
	where
		choice_ :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> Double -> Vec3 Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
		--choice_ = physicsBallAdvanceStationary
		--choice_ = physicsBallAdvanceGhostly
		choice_ = physicsBallAdvanceBruteForce

-- | For debugging or performance checking, keep the ball stationary.
physicsBallAdvanceStationary :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> Double -> Vec3 Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvanceStationary _x'cfg _level _spa _ballRadius _gravityVector _dt p0 v0 = (p1, v0)
	where
		p1 = p0

-- | For debugging or performance checking, ignore all collision checking.
physicsBallAdvanceGhostly :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> Double -> Vec3 Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvanceGhostly x'cfg _level _spa _ballRadius gravityVector dt p0 v0 = (p1, v1)
	where
		p1 = p0 + (dt `sv3` v1)
		v1 = v0 + ((dt * gravityAcceleration) `sv3` gravityVector)
		gravityAcceleration = x'cfg^.x'cfgGravity

-- | This version completely ignores the BSP.  It checks collisions with every
-- lump every frame.
physicsBallAdvanceBruteForce :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> Double -> Vec3 Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvanceBruteForce = physicsBallAdvanceBruteForceCompute 0 0.0 0.0

-- | Finish computing 'physicsBallAdvanceBruteFroce'.
--
-- Make a line segment from p0 to p1, where p1 is p0 + dt*v, i.e. the tentative
-- end-point of the ball's position after the frame, if no collisions occur.
-- Check all lumps for a closest collision, and if one is found, expend enough
-- dt to advance the ball to the point of intersection, and then reflect v0
-- about the face's plane, and repeat, checking again for all lumps using the
-- remaining dt to expend.  Since the path represent the center of the ball,
-- use the sphere's radius to check for distance.  For edges and vertices, use
-- as the plane a constructed plane where the plane is orthogonal to the line
-- from the (new) ball center to the point of intersection.  Once no collisions
-- have been detected, freely expend the remaining dt.  We now have the new
-- ball position, at the last p1.
--
-- Parameters:
--
-- numCollisions:
-- 	for testing squishes, this is incremented each collision, and once it
-- 	exceeds the maximum, the squish condition is detected and the physics
-- 	engine will skip any remaining collisions, letting the ball go through lumps.
-- thresholdTimeRemaining:
-- 	Once this much dt has been expended, reset the 2 squish detection
-- 	parameters (this and numCollisions).
-- thresholdRDistanceRemaining:
-- 	Once the ball has traveled this much distance in terms of ball radiuses,
-- 	reset the squish state in this condition too.  We don't want the ball the
-- 	ghost through walls if the frame is slow and processing a lot of dt at
-- 	once, simply because it made many collisions in a frame even though it
-- 	traveled a long distance (e.g. spinning around in a cone if the system
-- 	briefly pauses), so we have thresholdTimeRemaining; and we also have
-- 	distance because otherwise the ball can always go fast enough so that it
-- 	bounces enough times between lumps and ghosts through.
physicsBallAdvanceBruteForceCompute :: Integer -> Double -> Double -> StaticConfig -> LevelIB -> SolPhysicsAnalysis -> Double -> Vec3 Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
--physicsBallAdvanceBruteForceCompute numCollisions thresholdTimeRemaining thresholdRDistanceRemaining x'cfg level spa ballRadius gravityVector dt p0 v0 =
physicsBallAdvanceBruteForceCompute numCollisions thresholdTimeRemaining thresholdRDistanceRemaining x'cfg level spa ballRadius gravityVector dt p0 v0
	-- Redundantly ensure dt is not zero or negative.
	| dt <= 0 + smallNum = let (p1, v1) = (p0, v0) in (p1, v1)
	-- Check the optional maxPhysicsStepTime config.
	| Just maxPhysicsStepTime <- x'cfg^.x'cfgMaxPhysicsStepTime, dt > maxPhysicsStepTime + smallNum =
		let (p1, v1) =
			physicsBallAdvanceBruteForceCompute
				numCollisions
				thresholdTimeRemaining
				thresholdRDistanceRemaining
				x'cfg
				level
				spa
				ballRadius
				gravityVector
				maxPhysicsStepTime
				p0
				v0 in
		physicsBallAdvanceBruteForceCompute
			numCollisions
			thresholdTimeRemaining
			thresholdRDistanceRemaining
			x'cfg
			level
			spa
			ballRadius
			gravityVector
			(dt - maxPhysicsStepTime)
			p1
			v1
	-- Now step the physics.
	| otherwise =
		case closestLumpIntersecting of  -- Find the next collision.
			Nothing ->  -- No more collisions this frame.
				-- Gravity: apply gravity to the rest of the path.
				let (p1, v1) = (p0 + (dt `sv3` v0), v0g dt) in (p1, v1)  -- Expend the rest of dt after the last collision.
			Just (_lastLi', edt, p0', v0') ->  -- Found the next collision.  Expend ‘edt’ to advance the pall to p0'.
				-- Gravity: only apply the gravity vector through to the next collision, later on when we produce what is v0' here.
				let travelRDistance = (p0' - p0)^.r3 / ballRadius; trd = travelRDistance in
				physicsBallAdvanceBruteForceCompute
					( if' resetSquishState 0                                                        (numCollisions + 1)                 )
					( if' resetSquishState (x'cfg^.x'cfgMaxFrameCollisionsDtThreshold        - edt) (thresholdTimeRemaining      - edt) )
					( if' resetSquishState (x'cfg^.x'cfgMaxFrameCollisionsRDistanceThreshold - edt) (thresholdRDistanceRemaining - trd) )
					x'cfg
					level
					spa
					ballRadius
					gravityVector
					(dt - edt)
					p0'
					v0'

	where
		bounceReturn = x'cfg^.x'cfgBounceReturn
		gravityAcceleration = x'cfg^.x'cfgGravity

		resetSquishState
			| thresholdTimeRemaining      <= 0 = True
			| thresholdRDistanceRemaining <= 0 = True
			| otherwise                        = False

		-- | Find the closest lump intersecting the ball's path, for collisions.
		closestLumpIntersectingRaw :: Maybe (Int32, Double, Vec3 Double, Vec3 Double)
		--closestLumpIntersectingRaw = safeHead . sortOn (^._2) . catMaybes . toList $ lumpsIntersecting
		closestLumpIntersectingRaw = safeHead . sortOn (^._2) . catMaybes $ lumpsIntersecting

		-- | Check cfgMaxFrameCollisions, and whether dt is already exhausted.
		closestLumpIntersecting :: Maybe (Int32, Double, Vec3 Double, Vec3 Double)
		closestLumpIntersecting
			| numCollisions >= (x'cfg^.x'cfgMaxFrameCollisions) = Nothing
			| dt <= 0 || (dt - 0) `equivalentSmall` 0           = Nothing
			| otherwise                                         = closestLumpIntersectingRaw

		-- | Map each lump into the closest collision, and get 1) the dt
		-- needed to expend to advance the ball to this point of collision, 2)
		-- the new ball pos and 3) ball vel.
		--
		-- Only the closest collision will be used for advancing the ball,
		-- before checking for collision again afterwards.
		--lumpsIntersecting :: Array Int32 (Maybe (Double, Vec3 Double, Vec3 Double))
		--lumpsIntersecting = level^.solLv <&> \lump ->
		lumpsIntersecting :: [Maybe (Int32, Double, Vec3 Double, Vec3 Double)]
		lumpsIntersecting = flip fmap (zip [0..] (toList $ level^.solLv)) . uncurry $ \li lump ->
			let
				checkVertices :: Bool
				checkVertices | (_:_:_:_) <- facesIntersectingNoBounds = True | otherwise = False
				verticesIntersecting :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				verticesIntersecting = if' (not checkVertices) [] . catMaybes $ do
					vi <- indirection <$> [lump^.lumpV0 .. lump^.lumpV0 + lump^.lumpVc - 1]
					let vertex = (level^.solVv) ! vi
					-- TODO
					return Nothing

				checkEdges :: Bool
				--checkEdges | (_:_:_) <- facesIntersectingNoBounds = True | otherwise = False
				checkEdges = True
				edgesIntersecting :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				edgesIntersecting = if' (not checkEdges) [] $ do
					D.trace "DEBUG0: edge check!" $ return ()
					-- For each edge,
					ei <- indirection <$> [lump^.lumpE0 .. lump^.lumpE0 + lump^.lumpEc - 1]
					let edge = (level^.solEv) ! ei
					let (vi, vj) = (((level^.solVv) ! (edge^.edgeVi))^.vertP, ((level^.solVv) ! (edge^.edgeVj))^.vertP)
					let el = line3Points vi vj

					-- Find the distance between the path (lp) and the edge.
					let ed = abs $ line3Line3Distance lp el

					-- Skip if _the infinite lines_ are too far away.  However,
					-- the line segments may still be too far away even if the
					-- infinite lines are not, so we'll filter lp coords in [0, 1].
					guard $ ed <= ballRadius

					-- Find the closest point.
					Just (lpx, elx) <- return $ line3Line3ClosestCoords lp el
					let elv = line3Lerp el elx

					-- Find the point on lp where the ball is at when it first
					-- collides with the edge.
					let lpCoordOffset = abs $ line3DistanceCoordFromPoint lp elv ballRadius

					-- Find candidates for x: they must be within [0, 1]; if no
					-- candidates pass, the line segment (the path) is too far
					-- away from the edge, so we'll skip this edge.
					let xCandidates = [lpx - lpCoordOffset, lpx + lpCoordOffset]
					(x:_) <- return . sort . filter (\x -> 0 <= x + smallNum && x - smallNum <= 1) $ xCandidates

					let ballIntersection = line3Lerp lp x `v3orWith` (lp^.p0l3)

					let edgePointBallIntersection = line3Lerp el . line3PointCoord el $ ballIntersection

					-- Check to make sure the intersection is actually on the
					-- edge _line segment_, not to leave it checking its
					-- infinite line only!
					let intersectionElx = line3PointCoord el edgePointBallIntersection
					guard $ 0 <= intersectionElx + smallNum && intersectionElx - smallNum <= 1

					-- For the reflecting plane for the ball's velocity,
					-- construct a virtual plane essentially with the vector
					-- from the edge to the ball's position at intersection as
					-- normal.
					let virtualPlane = normalPlane3 (v3normalize $ ballIntersection - edgePointBallIntersection) 0

					-- Make sure the ball is going towards the edge, not away
					-- from it, similar to the normal check for planes for
					-- avoiding multiple collisions in a single step for me
					-- thasme lump.
					guard $ (lp^.a0l3) `d3` (virtualPlane^.abcp3) <= 0

					-- Return the results of this collision, which is used if
					-- it is found to be the first potential collision on the
					-- path lp.
					let edt = x * dt
					let p0' = ballIntersection
					let v0' = plane3ReflectPointAmount (virtualPlane & dp3 .~ 0) (v0g edt) (bounceReturn)  -- v0g: Apply gravity for this path.
					return $ (li, edt, p0', v0')

				-- | Find all faces that intersect p0->p1.
				--
				-- Find where the p0->p1 intersects a side's plane.  However,
				-- handle the bounds of the face: if the face's plane's point
				-- of intersection with p0->p1 is not behind (<=) all other
				-- planes, then it's beyond the edges bordering the side.
				--
				-- Note this requires that all sides have a normal pointing
				-- _away_ from the convex lump inside the level file.
				facesIntersecting :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				facesIntersecting = facesIntersecting' False
				facesIntersectingNoBounds :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				facesIntersectingNoBounds = facesIntersecting' True
				-- | We use ignoreEdgeBounds when checking for edge and vertex
				-- intersections.
				facesIntersecting' :: Bool -> [(Int32, Double, Vec3 Double, Vec3 Double)]
				facesIntersecting' ignoreEdgeBounds = do
					let errMsg = "Internal error: physicsBallAdvanceBruteForceCompute: sides data missing for lump with li " ++ (show li) ++ "."
					let sidePlanes = flip M.lookup (spa^.spaLumpPlanes) li `morElse` error errMsg

					-- For each side (check useDirectSol for which set of sides to use),
					(sidx, sidePlane) <-
						if useDirectSol
							then do
								-- For each side,
								si <- indirection <$> [lump^.lumpS0 .. lump^.lumpS0 + lump^.lumpSc - 1]
								let side = (level^.solSv) ! si
								-- Get its plane.
								let sidePlane = normalPlane3 (side^.sideN) (side^.sideD)
								return (si, sidePlane)
							else do
								(sidx, sidePlane) <- zip [0..] sidePlanes
								return (sidx, sidePlane)
					-- Find where on lp it intersects; abort this try if it doesn't.
					Just x <- return $ line3CoordAtDistancePlane3 sidePlane lp ballRadius
					-- Only consider intersections on the line segment.
					--guard $ (0 <= x + smallNum && x - smallNum <= 1)
					-- The right side prevents the ball from ghost-glitching
					-- through a wall for very short 'lp' ('onPlaneCheck').
					let intersectsLp  = 0 <= x + smallNum && x - smallNum <= 1
					let onPlaneCheck  = plane3PointDistance sidePlane (lp^.p0l3) `near` ballRadius && plane3PointDistance sidePlane (lp^.p1l3) `near` ballRadius
					guard $ intersectsLp || onPlaneCheck

					-- Get the point in 3D space: this is where the ball would
					-- be if advanced to this intersection.
					let ballIntersection = line3Lerp lp x `v3orWith` (lp^.p0l3)
					-- Get the point on the plane where this collision would
					-- take place: the closest point on the plane to
					-- 'ballIntersection'.
					let planeIntersection = pointToPlane ballIntersection sidePlane
					-- Only consider intersections whose plane intersection
					-- points are behind all other sides.
					guard . (if' ignoreEdgeBounds (const True) id) . and $ do
						if useDirectSol
							then do
								let si = sidx

								-- For every other side …
								sj <- [lump^.lumpS0 .. lump^.lumpS0 + lump^.lumpSc - 1]
								guard $ sj /= si
								let sidej = (level^.solSv) ! sj
								let sidejPlane = normalPlane3 (sidej^.sideN) (sidej^.sideD)

								-- Make sure the point is behind this plane.
								return $ plane3PointDistance sidejPlane planeIntersection <= 0
							else do
								-- For every other side …
								(sjidx, sidejPlane) <- zip [0..] sidePlanes
								guard $ sjidx /= sidx

								-- Make sure the point is behind this plane.
								return $ plane3PointDistance sidejPlane planeIntersection <= 0

					-- Finally, make sure we only register a collision if the
					-- ball is going towards this plane, not away from it: e.g.
					-- you can only collide against a top face from above, not
					-- from underneath.  This prevents the same lump causing
					-- multiple collision events for what should be a single
					-- collision.  We can do this by making sure the dot
					-- product of the direction (axis) of ‘lp’ and the plane
					-- normal is not positive.
					guard $ (lp^.a0l3) `d3` (sidePlane^.abcp3) <= 0

					-- We've found an intersection.  Now calculate the values
					-- we would need if we ended up picking this after finding it
					-- is indeed the closest.

					let edt = x * dt
					let p0' = ballIntersection
					let v0' = plane3ReflectPointAmount (sidePlane & dp3 .~ 0) (v0g edt) (bounceReturn)  -- v0g: Apply gravity for this path.
					return $ (li, edt, p0', v0')

					where
						useDirectSol = False

				-- | Lower dimensionalities appear before higher
				-- dimensionalities for equal distances.  'sortOn' is a stable
				-- sort, so we can use it.
				allIntersecting = concat $
					[
						verticesIntersecting,
						edgesIntersecting,
						facesIntersecting
					]

				sortedIntersecting = sortOn (^._2) $ allIntersecting

				lumpComponentIntersecting :: Maybe (Int32, Double, Vec3 Double, Vec3 Double)
				lumpComponentIntersecting = safeHead sortedIntersecting

				-- Make sure the lump isn't the one we last used for collisions.
				r = lumpComponentIntersecting
			in
				r

			where
				-- | Draw a line segment from the ball's position (center of
				-- sphere) to the tentative end-point if all dt were to be
				-- expended now using its current velocity.
				--
				-- TODO: double check edge cases of small lp don't cause
				-- issues.  The guard has a p0 and p1 nearness check, but there
				-- might still be issues.
				lp = let p1 = p0 + (dt `sv3` v0) in line3Points p0 p1

		-- | It seems sols have indirection for lump sides, vertices, and
		-- edges, but not edge indices to vertices.
		indirection :: Int32 -> Int32
		indirection idx = (level^.solIv) ! idx

		v0g edt = v0 + ((edt * gravityAcceleration) `sv3` gravityVector)  -- What velocity is if v0 has time added to gravity.

-- * Local utils.
