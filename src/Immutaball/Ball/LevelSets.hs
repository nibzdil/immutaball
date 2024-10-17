{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- LevelSets.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Ball.LevelSets
	(
		ChallengeModeScores(..), cmsBestTimes, cmsMostCoins,
		LevelSet(..), lsTitle, lsDesc, lsName, lsPict, lsChallengeModeScores, lsLevels,
		LevelSets(..), lsExplicitSets, lsLevelSets
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens
import qualified Data.Map as M

data ChallengeModeScores = ChallengeModeScores {
	_cmsBestTimes :: (Integer, Integer, Integer),
	_cmsMostCoins :: (Integer, Integer, Integer)
}
makeLenses ''ChallengeModeScores

data LevelSet = LevelSet {
	-- | "My Levels"
	_lsTitle :: String,
	-- | "A level set.\\Another paragraph.\Another line in the paragraph."
	_lsDesc  :: String,
	-- | "foo"
	_lsName  :: String,
	-- | "shot-foo/foo.jpg"
	_lsPict  :: String,
	-- | Challenge mode scores: best times (hard, medium, easy), most coins (hard, medium easy).
	_lsChallengeModeScores :: ChallengeModeScores,
	-- | ["map-foo/level.sol", …]
	_lsLevels :: [String]
}
makeLenses ''LevelSet

data LevelSets = LevelSets {
	-- | Lines of ‘data/sets.txt’.
	-- [‘set-foo.txt’, …]
	_lsExplicitSets :: [String],
	-- | Map of ‘set-foo.txt’ to the level set.
	_lsLevelSets    :: M.Map String LevelSet
}
makeLenses ''LevelSets
