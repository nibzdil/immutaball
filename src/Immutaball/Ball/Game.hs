{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Immutaball.Ball.Game
	(
		ChallengeModeState(..), cmsTotalCoins, cmsTotalTimeCs, cmsTotalDeaths,
		initialChallengeModeState,

		GameMode(..), AsGameMode(..),
		isPlaying, isFallOut, isTimesUp, isWin, isPaused, isIntermission,
			isGameEnded, isGameFailed, isGameRunning,
		GameState(..), gsGameMode, gsTimeElapsed, gsPaused, gsPreview,
			gsBallPos, gsBallVel, gsBallRot, gsBallRadius, gsSolRaw, gsSol,
			gsSolAttributes, gsSolAnalysis, gsSwa, gsCameraAngle, gsCameraMode,
			gsCoinState, gsSwitchState, gsPathState, gsTeleporterState,
			gsGoalState, gsGravityState, gsDebugState, gsInputState,
			{- gsAnalysis, -}
		GameStateAnalysis(..), gsaView, gsaNetRightForwardJump,
		mkGameStateAnalysis,
		initialGameState,
		CoinState(..), csCoinsCollected, csTotalCollected, csTotalUncollected,
			csCoinCollectedAt, csCoinsUncollected,
		SwitchState(..), xsSwitchesEnabled, xsSwitchesTimers, xsBallInAnySwitch, xsBallInSwitch,
		PathState(..), psPathsTimeElapsed, psPathsGoing,
		TeleporterState(..), jsBallInAnyTeleporter, jsBallBeingTeleported,
		GoalState(..), zsCoinUnlocked, zsStartUnlocked, zsUnlocked,
		GravityState(..), gravsTiltRightRadians, gravsTiltForwardRadians,
		GameDebugState(..), gdsCameraDebugOn, gdsCameraPosOffset,
			gdsCameraAimRightRadians, gdsCameraAimUpRadians,
		GameInputState(..), ginsRightOn, ginsLeftOn, ginsForwardOn, ginsBackwardOn, ginsVertUpOn, ginsVertDownOn
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))
import Data.Int
import Data.Maybe

import Control.Lens
import Control.Parallel
import Data.Array.IArray
import qualified Data.Map as M
import qualified Data.Set as S

import Immutaball.Ball.LevelSets
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.Level.Analysis
import Immutaball.Share.Level.Attributes
import Immutaball.Share.Level.Base
import Immutaball.Share.Level.Utils
import Immutaball.Share.Math
import Immutaball.Share.State.Context
import Immutaball.Share.Utils

data ChallengeModeState = ChallengeModeState {
	_cmsTotalCoins  :: Integer,
	_cmsTotalTimeCs :: Integer,  -- ^ centiseconds
	_cmsTotalDeaths :: Integer
}
makeLenses ''ChallengeModeState

initialChallengeModeState :: ChallengeModeState
initialChallengeModeState = ChallengeModeState {
	_cmsTotalCoins  = 0,
	_cmsTotalTimeCs = 0,
	_cmsTotalDeaths = 0
}

data GameMode =
	  Playing
	| FallOut
	| TimesUp
	| Win
	| Paused
	| Intermission
	deriving (Eq, Ord, Enum, Bounded, Show)
makeClassyPrisms ''GameMode

isPlaying :: GameMode -> Bool
isPlaying Playing = True
isPlaying _       = False

isFallOut :: GameMode -> Bool
isFallOut FallOut = True
isFallOut _       = False

isTimesUp :: GameMode -> Bool
isTimesUp TimesUp = True
isTimesUp _       = False

isWin :: GameMode -> Bool
isWin Win = True
isWin _   = False

isPaused :: GameMode -> Bool
isPaused Paused = True
isPaused _      = False

isIntermission :: GameMode -> Bool
isIntermission Intermission = True
isIntermission _            = False

isGameEnded :: GameMode -> Bool
isGameEnded FallOut = True
isGameEnded TimesUp = True
isGameEnded Win     = True
isGameEnded _       = False

isGameFailed :: GameMode -> Bool
isGameFailed FallOut = True
isGameFailed TimesUp = True
isGameFailed _       = False

isGameRunning :: GameMode -> Bool
isGameRunning Playing = True
isGameRunning _       = False

data GameState = GameState {
	-- | Are we playing, won, fell out, etc.?
	_gsGameMode :: GameMode,

	-- | Game time elapsed.
	_gsTimeElapsed :: Double,
	-- | Is paused.
	_gsPaused :: Bool,
	-- | Is in intermission.  Time elapsed in intermission.
	_gsPreview :: Maybe Double,
	-- | The position of the ball.
	_gsBallPos :: Vec3 Double,
	-- | The velocity of the ball.
	_gsBallVel :: Vec3 Double,
	-- | The rotation of the ball about each axis.  Radians.
	_gsBallRot :: Vec3 Double,
	-- | The radius of the ball.
	_gsBallRadius :: Double,

	-- | Directly parsed sol, level file.
	_gsSolRaw :: LevelIB,
	-- | Postprocessed sol: apply restoring transformation.
	_gsSol :: LevelIB,
	-- | Read metadata from the sol.
	_gsSolAttributes :: SolAttributes,
	-- | Optionally make our own analysis from the SOL.
	_gsSolAnalysis :: SolAnalysis,
	-- | Convenience pairing of the sol with the analysis.
	_gsSwa :: SolWithAnalysis,

	-- | The position of the camera, as a counter-clock-wise angle relative to
	-- the starting camera position of looking toward the positive y axis, with
	-- the camera orientation specified in the Config rc file.
	_gsCameraAngle :: Double,
	_gsCameraMode :: Integer,

	_gsCoinState :: CoinState,
	_gsSwitchState :: SwitchState,
	_gsPathState :: PathState,
	_gsTeleporterState :: TeleporterState,
	_gsGoalState :: GoalState,

	_gsGravityState :: GravityState,

	_gsDebugState :: GameDebugState,

	_gsInputState :: GameInputState

	{-
	-- | Composite data e.g. for the play state to know where the camera is,
	-- built from game state data.
	_gsAnalysis :: GameStateAnalysis
	-}
}
	deriving (Eq, Ord, Show)
--makeLenses ''GameState

data GameStateAnalysis = GameStateAnalysis {
	-- | Where the camera is.
	_gsaView :: MView,
	-- | Net movement from input state: -1 for opposite, 0 for neutral, 1 for On.
	_gsaNetRightForwardJump :: (Integer, Integer, Integer)
}
	deriving (Eq, Ord, Show)

--mkGameStateAnalysis

data CoinState = CoinState {
	_csCoinsCollected :: M.Map Int32 Bool,

	-- Utility data.
	_csTotalCollected :: Integer,
	_csTotalUncollected :: Integer,
	_csTotalCoinValue :: Integer,
	_csCoinCollectedAt :: M.Map Int32 Double,
	_csCoinsUncollected :: S.Set Int32
}
	deriving (Eq, Ord, Show)
--makeLenses ''CoinState

data SwitchState = SwitchState {
	_xsSwitchesEnabled :: M.Map Int32 Bool,
	-- | Time left.
	_xsSwitchesTimers :: M.Map Int32 Double,

	_xsBallInAnySwitch :: Bool,
	_xsBallInSwitch :: M.Map Int32 Bool
}
	deriving (Eq, Ord, Show)
--makeLenses ''SwitchState

data PathState = PathState {
	_psPathsTimeElapsed :: M.Map Int32 Double,
	_psPathsGoing :: M.Map Int32 Bool
}
	deriving (Eq, Ord, Show)
--makeLenses ''PathState

data TeleporterState = TeleporterState {
	_jsBallInAnyTeleporter :: Bool,
	-- | Which teleporter, and teleportation time elapsed (in seconds, as usual).
	_jsBallBeingTeleported :: Maybe (Int32, Double)
}
	deriving (Eq, Ord, Show)
--makeLenses ''TeleporterState

data GoalState = GoalState {
	_zsCoinUnlocked :: Bool,
	_zsStartUnlocked :: Bool,
	-- | coin || start:
	_zsUnlocked :: Bool
}
	deriving (Eq, Ord, Show)
--makeLenses ''GoalState

data GravityState = GravityState {
	_gravsTiltRightRadians   :: Double,
	_gravsTiltForwardRadians :: Double
}
	deriving (Eq, Ord, Show)
--makeLenses ''GravityState

data GameDebugState = GameDebugState {
	_gdsCameraDebugOn  :: Bool,
	_gdsCameraPosOffset :: Vec3 Double,
	_gdsCameraAimRightRadians :: Double,
	_gdsCameraAimUpRadians :: Double
}
	deriving (Eq, Ord, Show)
--makeLenses ''GameDebugState

-- | For input with greater preservation.
data GameInputState = GameInputState {
	_ginsRightOn    :: Bool,  -- Right arrow pressed?
	_ginsLeftOn     :: Bool,  -- Left arrow pressed?
	_ginsForwardOn  :: Bool,  -- Up arrow?
	_ginsBackwardOn :: Bool,  -- Down arrow?
	_ginsVertUpOn   :: Bool,  -- Spacebar?  (Not used in regular gameplay.)
	_ginsVertDownOn :: Bool   -- ‘c’ for qwerty crouch to move down?  (Not used in regular gameplay.)
}
	deriving (Eq, Ord, Show)
--makeLenses ''GameDebugState

makeLenses ''GameState
makeLenses ''GameStateAnalysis
makeLenses ''CoinState
makeLenses ''SwitchState
makeLenses ''PathState
makeLenses ''TeleporterState
makeLenses ''GoalState
makeLenses ''GravityState
makeLenses ''GameDebugState
makeLenses ''GameInputState

initialGameState :: IBContext' a -> Neverballrc -> Bool -> Maybe LevelSet -> String -> LevelIB -> GameState
initialGameState cxt neverballrc hasLevelBeenCompleted mlevelSet solPath sol = fix $ \gs ->
	let solAnalysis = mkSolAnalysis cxt (gs^.gsSol) in
	par solAnalysis $
	GameState {
		_gsGameMode    = Intermission,

		_gsTimeElapsed = 0.0,
		_gsPaused      = False,
		_gsPreview     = Just 0.0,
		_gsBallPos     = ((^.ballP) <$> ((gs^.gsSol.solUv) !? 0)) & fromMaybe (Vec3 0.0 0.0 0.0),
		_gsBallVel     = Vec3 0.0 0.0 0.0,
		_gsBallRot     = Vec3 0.0 0.0 0.0,
		_gsBallRadius  = ((^.ballR) <$> ((gs^.gsSol.solUv) !? 0)) & fromMaybe (1.0),

		_gsSolRaw        = sol,
		_gsSol           = postprocessSol restoreSolTransformation (gs^.gsSolRaw),
		_gsSolAttributes = mkSolAttributes (gs^.gsSol),
		_gsSolAnalysis   = solAnalysis,
		_gsSwa           = SolWithAnalysis {
			_swaSol  = (gs^.gsSol),
			_swaSa   = solAnalysis,
			_swaMeta = SolMeta {
				_smPath     = solPath,
				_smLevelSet = mlevelSet
			}
		},

		_gsCameraAngle = 0.0,
		_gsCameraMode  = fromIntegral $ (neverballrc^.camera),

		_gsCoinState = CoinState {
			_csCoinsCollected = M.fromList [(k, v) | v <- return False, k <- range . bounds $ (gs^.gsSol.solHv)],

			_csTotalCollected   = 0,
			_csTotalUncollected = (gs^.gsCoinState.csTotalCoinValue),
			_csTotalCoinValue   = sum [v | coin <- elems (gs^.gsSol.solHv), v <- return . fromIntegral $ (coin^.itemN)],
			_csCoinCollectedAt  = M.empty,
			_csCoinsUncollected = S.fromList . range . bounds $ (gs^.gsSol.solHv)
		},
		_gsSwitchState = SwitchState {
			_xsSwitchesEnabled = M.fromList [(k, v) | k <- range . bounds $ (gs^.gsSol.solXv), x <- return $ (gs^.gsSol.solXv) ! k, v <- return $ (x^.swchF) /= 0],
			_xsSwitchesTimers  = M.fromList [(k, v) | k <- range . bounds $ (gs^.gsSol.solXv), x <- return $ (gs^.gsSol.solXv) ! k, v <- return $ (x^.swchT)],

			_xsBallInAnySwitch = False,
			_xsBallInSwitch    = M.fromList [(k, v) | v <- return False, k <- range . bounds $ (gs^.gsSol.solXv)]
		},
		_gsPathState = PathState {
			_psPathsTimeElapsed = M.fromList [(k, v) | v <- return 0, k <- range . bounds $ (gs^.gsSol.solPv)],
			_psPathsGoing       = M.fromList [(k, v) | k <- range . bounds $ (gs^.gsSol.solPv), p <- return $ (gs^.gsSol.solPv) ! k, v <- return $ (p^.pathF) /= 0]
		},
		_gsTeleporterState = TeleporterState {
			_jsBallInAnyTeleporter = False,
			_jsBallBeingTeleported = Nothing
		},
		_gsGoalState = GoalState {
			_zsCoinUnlocked  = 0 >= (gs^.gsSolAttributes.saGoal),
			_zsStartUnlocked = (not (neverballrc^.lockGoals)) && hasLevelBeenCompleted,
			_zsUnlocked      = (gs^.gsGoalState.zsCoinUnlocked) || (gs^.gsGoalState.zsStartUnlocked)
		},

		_gsGravityState = GravityState {
			_gravsTiltRightRadians   = 0.0,
			_gravsTiltForwardRadians = 0.0
		},

		_gsDebugState = GameDebugState {
			_gdsCameraDebugOn         = False,
			_gdsCameraPosOffset       = zv3,
			_gdsCameraAimRightRadians = 0.0,
			_gdsCameraAimUpRadians    = 0.0
		},

		_gsInputState = GameInputState {
			_ginsRightOn    = False,
			_ginsLeftOn     = False,
			_ginsForwardOn  = False,
			_ginsBackwardOn = False,
			_ginsVertUpOn   = False,
			_ginsVertDownOn = False
		}

		{-
		_gsAnalysis = mkGameStateAnalysis gs,
		-}
	}

mkGameStateAnalysis :: IBStateContext -> GameState -> GameStateAnalysis
mkGameStateAnalysis cxt gs = fix $ \_gsa -> GameStateAnalysis {
	_gsaView                = theView,
	_gsaNetRightForwardJump = theNetRightForwardJump
}
	where
		theView :: MView
		theView =
			let (mviewDefault :: MView) = MView {
				_mviewPos    = Vec3 0.0 0.0 0.0,
				_mviewTarget = Vec3 0.0 1.0 0.0,
				_mviewFov    = 2 * (fromIntegral $ cxt^.ibNeverballrc.viewFov)
			} in
			let (maybeView :: Maybe View) = (gs^.gsSol.solWv) !? 0 in
			let gds = gs^.gsDebugState in
			let (mviewIntermission :: MView) = (\f -> maybe mviewDefault f maybeView) $ \view_ ->
				-- TODO: this is now refactored into aim* utils in Math.hs.  Remove commented out part after things are working better.
				{-
				let targetQDiff = (view_^.viewQ) `minusv3` (view_^.viewP) in
				let horizRotatedTargetQDiff = rotatexySimple (gs^.gsDebugState.gdsCameraAimRightRadians) `mv3` targetQDiff in
				-- Finally, vertically rotate.  Treat x and y as a single number by magnitude and rotate with z.  TODO refactor (probably into a Math.hs new function).
				let (xy, z) = (Vec2 (horizRotatedTargetQDiff^.x3) (horizRotatedTargetQDiff^.y3), horizRotatedTargetQDiff^.z3) in
				let xyz = Vec2 (xy^.r2) z in
				let xyzRotated = xyz & t2 %~ (+ (gs^.gsDebugState.gdsCameraAimUpRadians)) in
				let xyzRotatedCapped = xyzRotated & t2 %~ (min (tau/2)) . (max (-tau/2)) in
				--let (Vec2 x' y', z') = (((xyzRotatedCapped^.x2)/(xy^.r2)) `sv2` xy, xyzRotatedCapped^.y2) in
				let (Vec2 x' y', z') = (xy & r2 .~ (xyzRotatedCapped^.x2), xyzRotatedCapped^.y2) in
				--let rotatedTargetQDiff = horizRotatedTargetQDiff in
				let rotatedTargetQDiff = Vec3 x' y' z' in
				-}
				let targetQDiff = (view_^.viewQ) `minusv3` (view_^.viewP) in
				let rotatedTargetQDiff = aimVert3DSimple (Just $ 0.99*(tau/4)) (gds^.gdsCameraAimUpRadians) . aimHoriz3DSimple (gds^.gdsCameraAimRightRadians) $ targetQDiff in

				let
					debugViewTarget = (view_^.viewP) `pv3` rotatedTargetQDiff `pv3` ddebugViewPos
					ddebugViewTarget' = debugViewTarget `minusv3` (view_^.viewQ)

					(ddebugViewPos, ddebugViewTarget) = if' (not (gds^.gdsCameraDebugOn)) (zv3, zv3) $ (gds^.gdsCameraPosOffset, ddebugViewTarget') in
				MView {
					_mviewPos    = (view_^.viewP) `pv3` ddebugViewPos,
					_mviewTarget = (view_^.viewQ) `pv3` ddebugViewTarget,
					-- (The neverballrc fov appears to be half fov, not whole fov, so double the degrees, then convert to radians.)
					_mviewFov    = let deg = 2.0 * (fromIntegral $ cxt^.ibNeverballrc.viewFov) in deg * (tau/360.0)
				} in
			-- TODO: use .neverballrc viewDp, viewDc, and viewDz.
			let (mviewCamera :: MView) = mviewIntermission in  -- TODO calculate view by ball pos, camera angle, and world tilt.  Just start with a 0,-1,0 vector and transform.
			let (mview :: MView) = if' (isIntermission $ gs^.gsGameMode) mviewIntermission mviewCamera in
			{-
			-- TODO DEBUG
			() <- initial -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG0: mkPlayState: mview is " ++ show (mview)) ()
			() <- initial -< liftIBIO . BasicIBIOF $ PutStrLn ("DEBUG0: mkPlayState: viewMat mview is " ++ show (viewMat mview)) ()
			-}
			mview

		theNetRightForwardJump :: (Integer, Integer, Integer)
		theNetRightForwardJump = (netRight, netForward, netVertUp)
			where
				netOf :: Bool -> Bool -> Integer
				netOf True  False = -1
				netOf False False =  0
				netOf False True  =  1
				netOf True  True  =  0

				netRight   = netOf (gs^.gsInputState.ginsLeftOn)     (gs^.gsInputState.ginsRightOn)
				netForward = netOf (gs^.gsInputState.ginsBackwardOn) (gs^.gsInputState.ginsForwardOn)
				netVertUp  = netOf (gs^.gsInputState.ginsVertDownOn) (gs^.gsInputState.ginsVertUpOn)
