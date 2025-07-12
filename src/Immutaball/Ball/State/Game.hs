{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Play.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, ScopedTypeVariables, NondecreasingIndentation #-}

-- | Game state and immutaball state interface.
module Immutaball.Ball.State.Game
	(
		GameRequest(..), giRequest, giGameState, giIBStateContext, grRequest,
		GameResponse(..), goGameEvents, goGameState, goIBStateContext, grGameEvents,
		GameEvent(..), AsGameEvent(..),

		NextCollision(..), ncClosestLump, ncCheckLumps, ncDistanceTo, ncTimeTo,
		NextCollisionState(..), ncsNc, ncsBspsLeft,
		LumpBSPPartitionParent(..), lbspppParent, lbspppIsRightBranch,
		LumpLpPlaneIntersection(..), llpiPlaneIdx, llpiPlane, llpiDistance,
			llpiTimeTo, llpiLi, llpiBi, llpiLp, llpiBodyTranslation,
			llpiIntersection, llpiIntersectionLx, llpiIntersectionBall,
		NextCollisionLump(..), nclLumpi, nclLpPlaneIntersections,

		stepGame,
		rendererTransformationMatrix,
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
		physicsBallAdvanceBruteForceCompute,

		physicsBallAdvanceBSP,

		-- * Utils.
		getPathTranslation,
		getBodyTranslation

		-- * Local utils.
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad
import Data.Int
import Data.Foldable
import Data.Function hiding (id, (.))
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe

import Control.Arrow
import Control.Lens
import Control.Monad.Trans.State.Lazy
import Data.Array.IArray
import qualified Data.Set as S
import Graphics.GL.Compatibility45
--import Graphics.GL.Core45
import Graphics.GL.Types
import qualified SDL.Raw.Enum as Raw

import Data.LabeledBinTree
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

-- TODO export.
data NextCollision = NextCollision {
	_ncClosestLump :: NextCollisionLump,
	_ncCheckLumps  :: [NextCollisionLump],  -- ^ Not necessarily sorted.
	_ncDistanceTo  :: Double,
	_ncTimeTo      :: Double
}
--makeLenses ''NextCollision

-- TODO export.
data NextCollisionState = NextCollisionState {
	_ncsNc       :: Maybe NextCollision,
	_ncsBspsLeft :: [(Int32, Tree LumpBSPPartition, Maybe LumpBSPPartitionParent)]
}
--makeLenses ''NextCollisionState

-- TODO export.
data LumpBSPPartitionParent = LumpBSPPartitionParent {
	-- | The parent of this partition.
	_lbspppParent        :: LumpBSPPartition,
	-- | Are we a right branch of the parent, rather than left?
	_lbspppIsRightBranch :: Bool
}
--makeLenses ''LumpBSPPartitionParent

data NextCollisionLump = NextCollisionLump {
	_nclLumpi                :: Int32,
	_nclLpPlaneIntersections :: [LumpLpPlaneIntersection]  -- Assumed to be non-empty and sorted.
}
--makeLenses ''NextCollisionLump

-- | When considering an advancement of the ball, for a given lump, and for a
-- given plane on that lump, a value of this type represents information about
-- the intersection of the lp line for the ball and this plane.
-- TODO export.
data LumpLpPlaneIntersection = LumpLpPlaneIntersection {
	-- | The lump plane index for the plane in question.
	_llpiPlaneIdx :: Integer,
	-- | The plane itself (relative body coords).
	_llpiPlane    :: Plane3 Double,
	-- | The distance along lp to the plane.
	_llpiDistance :: Double,
	-- | The dt that would be expended to reach this point of intersection.
	_llpiTimeTo   :: Double,

	-- | The lump index.
	_llpiLi :: Int32,
	-- | The body index.
	_llpiBi :: Int32,
	-- | For convenience, store the current body translation (where it is on
	-- the path - you can add the base vertices to get the net world coords.)
	_llpiBodyTranslation :: Vec3 Double,

	-- | The lp in question (relative body coords; use 'ipb' to go back to world coords).
	_llpiLp :: Line3 Double,
	-- | The point of intersection on the body, relative to body (use ‘ipb’ to go back).
	_llpiIntersection :: Vec3 Double,

	-- | Where on ‘lp’ the intersection happens (0.0 if lp is really small).
	_llpiIntersectionLx :: Double,
	-- | Where the ball would be positioned at the point of intersection; body coords.
	_llpiIntersectionBall :: Vec3 Double
}
--makeLenses ''LumpLpPlaneIntersection

makeLenses ''NextCollision
makeLenses ''NextCollisionState
makeLenses ''LumpBSPPartitionParent
makeLenses ''LumpLpPlaneIntersection
makeLenses ''NextCollisionLump

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

-- | Get a matrix that converts from world coordinates to OpenGL coordinates
-- given the view.
rendererTransformationMatrix :: IBStateContext -> GameState -> MView -> Mat4 Double
rendererTransformationMatrix cxt gs view_ = worldToGL <> rescaleDepth depthScale 0 <> viewMat' viewCollapse view_ <> tilt
	where
		x'cfg = cxt^.ibContext.ibStaticConfig
		depthScale = x'cfg^.x'cfgDepthScale
		viewCollapse = x'cfg^.x'cfgViewCollapse

		-- Translate to the ball (negated actually), rotate by gsCameraAngle, tilt3z, and untranslate.
		tilt =
			translate3 (gs^.gsBallPos) <>     -- Finally, undo the first translation.
			tilt3z (gsa^.gsaUpVec) <>         -- Tilt the world.
			rotatexy (-gs^.gsCameraAngle) <>  -- Rotate camera.
			translate3 (-gs^.gsBallPos)       -- First, go to ball (negated actually).
		gsa = mkGameStateAnalysis cxt gs

renderBall :: Wire ImmutaballM ((MView, GameState), IBStateContext) IBStateContext
renderBall = proc ((camera_, gs), cxtn) -> do
	hasInit <- delay False -< returnA True
	cxtnp1 <- arr snd ||| renderBallSetup -< if' hasInit Left Right (gs, cxtn)

	let sdlGL1' = liftIBIO . sdlGL1 (cxtnp1^.ibContext.ibSDLManagerHandle)

	-- Render the ball.
	(mballElemVaoVboEbo, cxtnp2) <- getBallElemVaoVboEbo -< cxtnp1
	let ballElemVaoVboEbo = fromMaybe (error "Internal error: renderBall expected elem vao and buf to be present, but it's missing!") mballElemVaoVboEbo
	let (ballElemVao, _ballElemVbo, _ballElemEbo) = ballElemVaoVboEbo

	-- Set the transformation matrix for the ball.
	mat <- arr $ uncurry3 rendererTransformationMatrix -< (cxtnp2, gs, camera_)
	cxtnp3 <- setTransformation -< (mat, cxtnp2)

	let (renderBallDirect :: GLIOF ()) = do
		-- Tell the shaders to disable the scene data.
		GLUniform1i shaderEnableSceneDataLocation falseAsIntegral ()
		-- Tell the shaders we are drawing the ball right now.
		GLUniform1i shaderEnableBallDataLocation trueAsIntegral ()

		-- Tell the shaders the ball radius.
		let (ballRadiusf :: GLfloat) = realToFrac $ gs^.gsBallRadius
		GLUniform1f shaderBallRadiusLocation ballRadiusf ()
		-- Tell the shaders the ball num triangles.
		let (ballNumTriangles :: Integer) = cxtnp3^.ibContext.ibStaticConfig.x'cfgBallTriangles
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
		--let (ballNumTriangles :: Integer) = cxtnp3^.ibContext.ibStaticConfig.x'cfgBallTriangles
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

	let cxt = cxtnp3
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
	let (ballPos', ballVel') = physicsBallAdvance x'cfg (gsnp1^.gsSol) (gsnp1^.gsSolAnalysis.saPhysicsAnalysis) (gsnp1^.gsSolAnalysis.saOtherAnalysis) (gsnp1^.gsBallRadius) gravityVector gsnp1 dt (gsnp1^.gsBallPos) (gsnp1^.gsBallVel)
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
physicsBallAdvance :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvance x'cfg level spa soa ballRadius gravityVector gs dt ballPos ballVel = choice_ x'cfg level spa soa ballRadius gravityVector gs dt ballPos ballVel
	where
		choice_ :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
		choice_ = physicsBallAdvanceStationary
		--choice_ = physicsBallAdvanceGhostly
		--choice_ = physicsBallAdvanceBruteForce
		--choice_ = physicsBallAdvanceBSP

-- | For debugging or performance checking, keep the ball stationary.
physicsBallAdvanceStationary :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvanceStationary _x'cfg _level _spa _soa _ballRadius _gravityVector _gs _dt p0 v0 = (p1, v0)
	where
		p1 = p0

-- | For debugging or performance checking, ignore all collision checking.
physicsBallAdvanceGhostly :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvanceGhostly x'cfg _level _spa _soa _ballRadius gravityVector _gs dt p0 v0 = (p1, v1)
	where
		p1 = p0 + (dt `sv3` v1)
		v1 = v0 + ((dt * gravityAcceleration) `sv3` gravityVector)
		gravityAcceleration = x'cfg^.x'cfgGravity

-- | This version completely ignores the BSP.  It checks collisions with every
-- lump every frame.
physicsBallAdvanceBruteForce :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
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
--
-- TODO: I once observed a rare glitching through a wall on Medium 16.
-- Probably make the planes and collisions handling a little more rigorous to
-- fix it, but the brute force algorithm isn't designed to be the primary
-- physics algorithm, so it probably isn't very important.
physicsBallAdvanceBruteForceCompute :: Integer -> Double -> Double -> StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
--physicsBallAdvanceBruteForceCompute numCollisions thresholdTimeRemaining thresholdRDistanceRemaining x'cfg level spa _soa ballRadius gravityVector gs dt p0 v0 =
physicsBallAdvanceBruteForceCompute numCollisions thresholdTimeRemaining thresholdRDistanceRemaining x'cfg level spa soa ballRadius gravityVector gs dt p0 v0
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
				soa
				ballRadius
				gravityVector
				gs
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
			soa
			ballRadius
			gravityVector
			gs
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
					soa
					ballRadius
					gravityVector
					gs
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
				-- I suspect checking the length of facesIntersectingNoBounds
				-- can introduce a space leak (e.g. try out retour de force 2),
				-- but it turns out probably checking the length of
				-- edgesIntersecting is probably better anyway.
				--checkVertices | (_:_:_:_) <- facesIntersectingNoBounds = True | otherwise = False
				-- TODO FIXME: verify this also isn't broken, but for now just
				-- always check verts.
				--checkVertices | (_:_:_) <- edgesIntersecting = True | otherwise = False
				checkVertices = True
				verticesIntersecting :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				verticesIntersecting = if' (not checkVertices) [] $ do
					-- For each verte,
					vi <- indirection <$> [lump^.lumpV0 .. lump^.lumpV0 + lump^.lumpVc - 1]
					let vertex = (level^.solVv) ! vi
					let v = vertex^.vertP

					-- Find the distance between the path (lp) and the vertex.
					let vd = abs $ line3PointDistance lp v

					-- Skip if the _infinite_ line extension from lp and
					-- the vertex are too far away.  This allows an early check
					-- and also to ensure the call to line3DistanceCoordFromPoint is valid.
					guard $ vd <= ballRadius

					-- Find the closest point coord on lp to the vertex.
					let lpClosestX = line3PointCoord lp v

					-- Find how far away the candidates for x (where the ball
					-- is exactly ballRadius away from the vertex) are from
					-- closestX.
					let lpCoordOffset = line3DistanceCoordFromPoint lp v ballRadius

					-- Find the coord on lp where the distance is the ball's
					-- radius.  Skip if we have nothing on lp.
					let xCandidates = [lpClosestX - lpCoordOffset, lpClosestX + lpCoordOffset]
					(x:_) <- return . sort . filter (\x -> 0 <= x + smallNum && x - smallNum <= 1) $ xCandidates

					-- Skip if x is not on lp.  (Already done by the filter above.)
					--guard $ 0 <= x + smallNum && x - smallNum <= 1

					-- Find the ball intersection point.
					let ballIntersection = line3Lerp lp x `v3orWith` (lp^.p0l3)

					-- For the reflecting plane for the ball's velocity,
					-- construct a virtual plane essentially with the vector
					-- from the vertex to the ball's position at intersection
					-- as normal.
					let virtualPlane = normalPlane3 (v3normalize $ ballIntersection - v) 0

					-- Make sure the ball is going towards the vertex, not away
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

				checkEdges :: Bool
				-- TODO FIXME: length >= 2 seems to cause edge collision detection
				-- to not work.  For now just disable this check.
				--checkEdges | (_:_:_) <- facesIntersectingNoBounds = True | otherwise = False
				checkEdges = True
				edgesIntersecting :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				edgesIntersecting = if' (not checkEdges) [] $ do
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
				_facesIntersectingNoBounds :: [(Int32, Double, Vec3 Double, Vec3 Double)]
				_facesIntersectingNoBounds = facesIntersecting' True
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

-- | Advance the ball's position and velocity, using the level BSP to handle
-- collisions and gravity.
--
-- Try expending the remaining dt, and detecting if any collisions are made
-- with any lumps or BSP partitions (e.g. check if the side (-1,0,1) of a plane
-- the position point is on would change).  If there's a collision, then p0->p1
-- intersects, and this algorithm sorts all the detected intersections to only
-- use the closest one, and then also checks among the remaining collisions the
-- ones that are near by the closest ones, to make sure these possible
-- collisions aren't skipped if advancing to the closest collision also changes
-- e.g. which side of a plane the ball would be on if it is advanced to the
-- closest collision.  We expend dt to advance the ball to the closest
-- collision and also check immediate (or near) new collisions the same
-- distance away from the closest collision.  Then we repeat the process anew
-- with the remaining dt to expend on advancing the ball, until there are no
-- more collisions, in which case we expend the remaining dt.
--
-- Also add a squish check mechanism: only so many collisions are permitted
-- within a specific distance and for a specific amount of dt.  If a squish
-- condition is detected, then instead of potentially looping forever (and
-- hanging), just abort collisions and let the ball ‘wall-glitch’ through walls
-- to move through them.
--
-- TODO: support moving bodies, since this currently only checks current static
-- position.
physicsBallAdvanceBSP :: StaticConfig -> LevelIB -> SolPhysicsAnalysis -> SolOtherAnalysis -> Double -> Vec3 Double -> GameState -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
physicsBallAdvanceBSP x'cfg level spa soa ballRadius gravityVector gs dt p0 v0
	| Just maxPhysicsStepTime <- x'cfg^.x'cfgMaxPhysicsStepTime, dt > maxPhysicsStepTime =
		let (p1, v1) = advance 0 0.0 0.0 maxPhysicsStepTime p0 v0
		in  (physicsBallAdvanceBSP x'cfg level spa soa ballRadius gravityVector gs) (dt - maxPhysicsStepTime) p1 v1
	| otherwise =
		advance 0 0.0 0.0 dt p0 v0

	where
		bounceReturn = x'cfg^.x'cfgBounceReturn
		gravityAcceleration = x'cfg^.x'cfgGravity

		-- Descend each BSP to find the closest intersecting lump, collecting
		-- all other lumps within threshold distance from the closest
		-- insersection and then checking them (avoiding a wall-glitch) before
		-- proceeding.
		--
		-- Repeat until all remaining dt is expended, or a squish condition is
		-- detected.
		advance :: Integer -> Double -> Double -> Double -> Vec3 Double -> Vec3 Double -> (Vec3 Double, Vec3 Double)
		advance numLocalCollisions localDtExpended localDistance dtn pn vn
			| localDtExpended    >= (x'cfg^.x'cfgMaxFrameCollisionsDtThreshold)        = advance 0 0.0 0.0 dtn pn vn
			| localDistance      >= (x'cfg^.x'cfgMaxFrameCollisionsRDistanceThreshold) = advance 0 0.0 0.0 dtn pn vn
			| numLocalCollisions >= (x'cfg^.x'cfgMaxFrameCollisions)                   = (p1, v1)
			| dt                 <= 0                                                  = (p1, v1)
			| otherwise                                                                = result
			where
				-- | Tentative end-point: advance here if no collisions.
				p1 :: Vec3 Double
				p1 = pn + (dtn `sv3` vn)

				-- | Tentative end-point velocity: used if no collisions.
				v1 :: Vec3 Double
				v1 = vng dtn

				-- | What velocity is if vn has time added to gravity.
				vng edt = vg vn edt
				vg v edt = v + ((edt * gravityAcceleration) `sv3` gravityVector)

				-- | Draw a line segment from the ball's position (center of
				-- sphere) to the tentative end-point if all dt were to be
				-- expended now using its current velocity.
				_lp :: Line3 Double
				_lp = line3Points p0 p1

				-- | Take a position/point, and the given body by index, and
				-- convert the position/coords to be in terms of the body frame
				-- of reference, so that you can use vertices in the body that
				-- could be moving on a path directly.  Return to world coords
				-- with 'ipb' afterwards.
				pb :: Vec3 Double -> Int32 -> Vec3 Double
				pb p bi =
					let bodyTranslation = getBodyTranslation level soa gs bi 0 in
					p - bodyTranslation

				ipb :: Vec3 Double -> Int32 -> Vec3 Double
				ipb p bi =
					let bodyTranslation = getBodyTranslation level soa gs bi 0 in
					p + bodyTranslation

				-- | Lump, collection of lumps to check, distance to closest,
				-- time required to expend to closest.
				nextCollision :: Maybe NextCollision
				nextCollision =
					let addNothing (a, b) = (a, b, Nothing) in
					(^.ncsNc) . flip execState (NextCollisionState Nothing ((addNothing . second ((^.lumpBSP)) <$>) . M.toList $ spa^.spaBodyBSPs)) . fix $ \me -> do
					bspsLeft <- gets (^.ncsBspsLeft)
					case bspsLeft of
						[] -> return ()
						((bi, bsp, mbspParent):bspsLeft') -> do
							let p0' = pb p0 bi
							let p1' = pb p1 bi
							let lp' = line3Points p0' p1'

							-- We now have a current best distance.  If this
							-- BSP partition, including all lumps directly on
							-- it, is entirely further away, we can skip it!
							mcurrentBestDistance <- gets (((^.ncDistanceTo) <$>) . (^.ncsNc))
							--let smallInfiniteLineThreshold = 0.001  -- TODO: move this constant somewhere better, more prominent.  Move it to X3D.  Perhaps make a class like SmallNum.
							let mdistanceToParentPlane = do
								bspParent <- mbspParent
								let parentPlane = bspParent^.lbspppParent.lbsppPlane
								_currentBestDistance <- mcurrentBestDistance
								if' ((lp'^.a0l3.r3) <= smallishInfiniteLineThreshold)
									(return $ plane3PointDistance parentPlane p0')
									$ do
										distanceInFront <- (/ (lp'^.a0l3.r3)) <$> line3CoordAtDistancePlane3 parentPlane lp'   ballRadius
										distanceBehind  <- (/ (lp'^.a0l3.r3)) <$> line3CoordAtDistancePlane3 parentPlane lp' (-ballRadius)
										return $ min distanceInFront distanceBehind
							let canSkipThisBSP = (== Just True) $ do
								currentBestDistance <- mcurrentBestDistance
								distanceToParentPlane <- mdistanceToParentPlane
								return $ distanceToParentPlane > currentBestDistance
							if' canSkipThisBSP me $ do

							modify (ncsBspsLeft .~ bspsLeft')
							let withEmpty _then = do
								me
							let withLeaf bspPartition then_ = do
								then_ bspPartition
							let withFork l bspPartition r then_ = do
								-- Try to eliminate l and r if we can show
								-- there won't be a collision on that side of
								-- the partition, then queue them up for
								-- processing.
								let queueL =
									(plane3PointDistance (bspPartition^.lbsppPlane) p0') <= 0 ||
									(plane3PointDistance (bspPartition^.lbsppPlane) p1') <= 0
								let queueR =
									(plane3PointDistance (bspPartition^.lbsppPlane) p0') >= 0 ||
									(plane3PointDistance (bspPartition^.lbsppPlane) p1') >= 0
								let newParent isRightBranch = LumpBSPPartitionParent {
									_lbspppParent        = bspPartition,
									_lbspppIsRightBranch = isRightBranch
								}
								when queueL $ do
									modify (ncsBspsLeft %~ ((bi, l, Just $ newParent False):))
								when queueR $ do
									modify (ncsBspsLeft %~ ((bi, r, Just $ newParent True):))
								then_ bspPartition
							deconsLabeledBinTree withEmpty withLeaf withFork bsp $ \bspPartition -> do
								-- Check each lump directly intersecting this
								-- BSP partition for a possible collision.
								let (lumpsi :: S.Set Int32) = bspPartition^.lbsppLumps
								(>> me) . forM_ lumpsi $ \lumpi -> do
									-- For now, we don't know all the details
									-- of the collision while we collect the
									-- closest and candidate other close lumps.
									-- Check each plane for a collision, and
									-- take the closest.  We detect a collision
									-- when lp' intersects with a lump's plane
									-- at a point behind all the other planes.

									-- | Assume sorted and non-empty lists
									let unsafeHead xs = case xs of (x:_) -> x; _ -> error "Internal error: physicsBallAdvanceBSP: unsafeHead called on empty list."
									let nclDist ncl = unsafeHead (ncl^.nclLpPlaneIntersections) ^. llpiDistance
									-- | Discard all collisions not close to the nearest.
									let (filterChecks :: NextCollisionLump -> [NextCollisionLump] -> [NextCollisionLump]) = \best checks ->
										flip filter checks $ \candidate ->
											(nclDist candidate) - (nclDist best) <= smallishNum
									-- | Get this lump's planes.
									let (lumpPlanes :: [Plane3 Double]) = M.lookup lumpi (spa^.spaLumpPlanes) `morElse`
										(error $ "Internal error: physicsBallAdvanceBSP: nextCollision: failed to find planes for lump " ++ show lumpi ++ ".")
									-- Query the current best and checks.
									mnc <- gets (^.ncsNc)
									let mncBest   = (^.ncClosestLump) <$> mnc
									let mncChecks = (^.ncCheckLumps)  <$> mnc
									let _mncFilterChecks = (pure filterChecks <*> mncBest) `morElse` id
									-- | Get this lump's intersections with lp'.
									let (lpIntersectionsRaw :: [LumpLpPlaneIntersection]) = do
										-- Get each plane and a list of all
										-- other planes for this lump.
										(planeIdx, (plane, others)) <- zip [0..] $ listOthers lumpPlanes

										{-
										-- Make sure we only register a collision if the
										-- ball is going towards this plane, not away from it: e.g.
										-- you can only collide against a top face from above, not
										-- from underneath.  This prevents the same lump causing
										-- multiple collision events for what should be a single
										-- collision.  We can do this by making sure the dot
										-- product of the direction (axis) of ‘lp’ and the plane
										-- normal is not positive.
										guard $ (lp'^.a0l3) `d3` (plane^.abcp3) <= 0
										-}

										-- To register a collision, make sure
										-- the advancement would bring the ball
										-- behind the plane: either p0 is
										-- outside (plane distance > 1) going
										-- in (== 0 || == 1), or p0 is
										-- strictly intersecting (== 0) going
										-- in (== -1).
										guard $ signum (plane3PointDistance plane p1 - ballRadius) < signum (plane3PointDistance plane p0 - ballRadius)

										-- See if we need to use special logic
										-- for very small steps.  If so, we'll
										-- focus more on just p0' without p1'.
										let approximatelyAPoint = (lp'^.a0l3.r3) <= smallishInfiniteLineThreshold

										-- Find the coord on lp' at which we
										-- find the intersection.
										let intersectionCoord
											| approximatelyAPoint = 0.0
											| otherwise = line3CoordAtDistancePlane3 plane lp' ballRadius `morElse` 0.0
										let intersectionBallPoint = line3Lerp lp' intersectionCoord
										let intersectionPoint
											| approximatelyAPoint = pointToPlane p0' plane
											| otherwise = line3Lerp lp' $ intersectionCoord + ballRadius/(lp'^.a0l3.r3)
										-- Find the distance from p0' across lp'
										-- (or shortest line) to the plane
										-- (where with ballRadius there is an
										-- intersection).  i.e. distance ball
										-- would travel to reach the collision.
										let distance
											| approximatelyAPoint = plane3PointDistance plane p0'
											| otherwise = (intersectionBallPoint - p0')^.r3
										-- Find the time to reach intersectionCoord.
										let timeTo = (intersectionCoord / (lp'^.a0l3.r3)) `florWith` 0.0

										-- Now, make sure the intersection
										-- point is on the lump, and not
										-- outside the lump on some arbitrary
										-- plane, since we have more data to
										-- work with now.
										let behindOthers =
											[
												r |
												other <- others,
												let d = plane3PointDistance other intersectionPoint,
												let r = d <= 0
											]
										guard $ and behindOthers

										-- Construct the result.
										return . fix $ \_lpLumpX -> LumpLpPlaneIntersection {
											_llpiPlaneIdx = planeIdx,
											_llpiPlane    = plane,
											_llpiDistance = distance,
											_llpiTimeTo   = timeTo,

											_llpiLi              = lumpi,
											_llpiBi              = bi,
											_llpiBodyTranslation = getBodyTranslation level soa gs bi 0,

											_llpiLp           = lp',
											_llpiIntersection = intersectionPoint,

											_llpiIntersectionLx   = intersectionCoord,
											_llpiIntersectionBall = intersectionBallPoint
										}
									let lpIntersectionsUnfiltered = sortOn (^.llpiDistance) lpIntersectionsRaw

									-- | If there are any intersections, see if
									-- the best intersection is better than our
									-- current best.  (Otherwise we're done
									-- with this lump.)
									if' (null lpIntersectionsUnfiltered) (return ()) $ do

									-- We found at least one intersection.

									-- We'll only need the closest intersection on
									-- this lump, and intersections close
									-- enough within a smallishNum threshold to
									-- test for edge and vertex collisions
									-- later.
									let lpIntersections = case lpIntersectionsUnfiltered of
										[] -> []
										(closest:rest) -> closest :
											let closestDist = closest^.llpiDistance in
											filter (\inters -> (inters^.llpiDistance) - closestDist <= smallishNum) rest

									-- Get the new best and new checks.
									let (lumpNc :: NextCollisionLump) = NextCollisionLump lumpi lpIntersections
									let (newBest, newChecksRaw)
										| Just oldBest <- mncBest, Just oldChecks <- mncChecks =
											if' (nclDist lumpNc < nclDist oldBest) (lumpNc, oldBest:oldChecks) (oldBest, lumpNc:oldChecks)
										| otherwise = (lumpNc, [])
									let newChecks = filterChecks newBest newChecksRaw

									-- | Update the next collision state.
									modify $ ncsNc .~ Just NextCollision {
										_ncClosestLump = newBest,
										_ncCheckLumps  = newChecks,
										_ncDistanceTo  = nclDist newBest,
										_ncTimeTo      = unsafeHead (newBest^.nclLpPlaneIntersections) ^. llpiTimeTo
									}

				-- Now handle nextCollision and check checkLumps.

				-- | After the next collision, find (pn, vn, edt), position,
				-- velocity, and dt expended.  After we handle this
				-- collision step, then we apply gravity based on the time
				-- expended to travel this distance, and continue looping
				-- through advance until we exhaust all collisions or reach a
				-- squish condition.
				afterNextCollision :: Maybe (Vec3 Double, Vec3 Double, Double)
				afterNextCollision = (<$> nextCollision) $ \nc ->
					let ourFlatten ((a, b), c) = (a, b, c) in
					ourFlatten (foldl' step (pn, vn) $ (nc^.ncClosestLump) : (nc^.ncCheckLumps), nc^.ncTimeTo)
						where
							step :: (Vec3 Double, Vec3 Double) -> NextCollisionLump -> (Vec3 Double, Vec3 Double)
							step (p, v) ncl
							-- 3 possible collision types: vertex, edge, or
							-- plane.  The type depends on how many
							-- intersections there are within threshold
							-- distance to the closest.  If there's only one
							-- planar intersection, it's a plane; if 2, it's an
							-- edge; if 3 or more, it's a vertex.
								| [] <- ncl^.nclLpPlaneIntersections =
									-- Redundant; internally should never happen.
									(p, v)
								| (px:[]) <- ncl^.nclLpPlaneIntersections =
									-- Just a single planar intersection.  So
									-- advance p, and mirror velocity about the
									-- plane.
									--
									-- TODO: handle body velocity!  This for
									-- now just assumes the body is stationary.
									let plane = (px^.llpiPlane) in
									let p'    = ipb (px^.llpiIntersectionBall) (px^.llpiBi) in
									let v'    = plane3ReflectPointAmount (plane & dp3 .~ 0) v bounceReturn in
									(p', v')
								-- TODO: handle edges and vertices!
								| otherwise = error "Internal error: TODO: unimplemented!  Edges and vertices."

				-- | Now apply gravity after advancing to the next collision.
				afterNextCollisionGravity :: Maybe (Vec3 Double, Vec3 Double, Double)
				afterNextCollisionGravity = (<$> afterNextCollision) $ \(pn', vn', edt) ->
					(pn', vg vn' $ edt, edt)

				-- | The result of 'advance'.  Try checking for collisions
				-- again if we found a collision, else we can finish.
				result :: (Vec3 Double, Vec3 Double)
				result
					| Just (pn', vn', edt) <- afterNextCollisionGravity =
						advance
							(numLocalCollisions + 1)
							(localDtExpended    + edt)
							(localDistance      + (pn' - pn)^.r3)
							(dtn                - edt)
							pn'
							vn'
					| otherwise =
						(p1, v1)

-- * Utils.

-- | Given the current game state, and the given _path_ by index (not body
-- index), get the translation of the path you can add body vertices to to get
-- the coords in world coords.
getPathTranslation :: LevelIB -> SolOtherAnalysis -> GameState -> Int32 -> Integer -> Vec3 Double
getPathTranslation _level soa gs pi_ derivativeDegree =
	let translAtTime = M.lookup pi_ ((soa^.soaPathTranslationAtTime.fakeEOS) derivativeDegree) `morElse`
		--(error $ "Error: getPathTranslation: failed to find translation map for path " ++ show pi_ ++ ".")
		const zv3 in
	-- TODO: Implement updating pathsTimeElapsed when stepping, then invert the comments in the next 2 lines.
	let pathTimeElapsed = gs^.gsTimeElapsed in
	--let pathTimeElapsed = flip M.lookup (gs^.gsPathState.psPathsTimeElapsed) pi_ `morElse` 0.0 in
	let bodyTranslation = translAtTime pathTimeElapsed in
	bodyTranslation

-- | Given the current game state, and the given _body_ by index (not path
-- index), get the translation of the path you can add body vertices to to get
-- the coords in words coords.
getBodyTranslation :: LevelIB -> SolOtherAnalysis -> GameState -> Int32 -> Integer -> Vec3 Double
getBodyTranslation level soa gs bi derivativeDegree =
	let body = (level^.solBv) ! bi in
	let bodyPath = body^.bodyP0 in
	let bodyTranslation = getPathTranslation level soa gs bodyPath derivativeDegree in
	bodyTranslation

-- * Local utils.
