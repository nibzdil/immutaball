{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Ball.Game
	(
		ChallengeModeState(..), cmsTotalCoins, cmsTotalTimeCs, cmsTotalDeaths,
		initialChallengeModeState,

		GameMode(..), AsGameMode(..),
		isPlaying, isFallOut, isTimesUp, isWin, isPaused, isIntermission,
			isGameEnded, isGameFailed, isGameRunning,
		GameState(..), gsGameMode, gsTimeElapsed, gsPaused, gsPreview,
			gsBallPos, gsBallVel, gsSolRaw, gsSol, gsSolAttributes,
			gsCameraAngle, gsCameraMode, gsCoinState, gsSwitchState,
			gsPathState, gsTeleporterState, gsGoalState,
		initialGameState,
		CoinState(..), csCoinsCollected, csTotalCollected, csTotalUncollected,
			csCoinCollectedAt, csCoinsUncollected,
		SwitchState(..), xsSwitchesEnabled, xsSwitchesTimers, xsBallInAnySwitch, xsBallInSwitch,
		PathState(..), psPathsTimeElapsed, psPathsGoing,
		TeleporterState(..), jsBallInAnyTeleporter, jsBallBeingTeleported,
		GoalState(..), zsCoinUnlocked, zsStartUnlocked, zsUnlocked
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))
import Data.Int
import Data.Maybe

import Control.Lens
import Data.Array.IArray
import qualified Data.Map as M
import qualified Data.Set as S

import Immutaball.Share.Config
import Immutaball.Share.Level.Attributes
import Immutaball.Share.Level.Base
import Immutaball.Share.Level.Utils
import Immutaball.Share.Math

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

	-- | Directly parsed sol, level file.
	_gsSolRaw :: LevelIB,
	-- | Postprocessed sol: apply restoring transformation.
	_gsSol :: LevelIB,
	-- | Read metadata from the sol.
	_gsSolAttributes :: SolAttributes,

	-- | The position of the camera, as a counter-clock-wise angle relative to
	-- the starting camera position of looking toward the positive y axis, with
	-- the camera orientation specified in the Config rc file.
	_gsCameraAngle :: Double,
	_gsCameraMode :: Integer,

	_gsCoinState :: CoinState,
	_gsSwitchState :: SwitchState,
	_gsPathState :: PathState,
	_gsTeleporterState :: TeleporterState,
	_gsGoalState :: GoalState
}
	deriving (Eq, Ord, Show)
--makeLenses ''GameState

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

makeLenses ''GameState
makeLenses ''CoinState
makeLenses ''SwitchState
makeLenses ''PathState
makeLenses ''TeleporterState
makeLenses ''GoalState

initialGameState :: Neverballrc -> Bool -> LevelIB -> GameState
initialGameState neverballrc hasLevelBeenCompleted sol = fix $ \gs -> GameState {
	_gsGameMode    = Intermission,

	_gsTimeElapsed = 0.0,
	_gsPaused      = False,
	_gsPreview     = Just 0.0,
	_gsBallPos     = ((^.ballP) <$> ((gs^.gsSol.solUv) !? 0)) & fromMaybe (Vec3 0.0 0.0 0.0),
	_gsBallVel     = Vec3 0.0 0.0 0.0,

	_gsSolRaw        = sol,
	_gsSol           = transformSol restoreSolTransformation (gs^.gsSol),
	_gsSolAttributes = mkSolAttributes (gs^.gsSol),

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
	}
}
