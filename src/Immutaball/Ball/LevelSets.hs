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
		LevelSets(..), lsExplicitSets, lsLevelSets,
		getLevelSets,
		getLevelSet
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow

import Control.Lens
import qualified Data.Text as T
import qualified Data.Map as M
import System.FilePath
import Text.Parsec

import Immutaball.Share.Config.Parser
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO

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

-- | Get level sets or fail.
getLevelSets :: IBContext' a -> ImmutaballIOF LevelSets
getLevelSets cxt = do
	asets <- BasicIBIOF $ ReadText ((cxt^.ibDirs.ibStaticDataDir) </> "sets.txt") id
	let err ioErr = BasicIBIOF (PutStrLn ("Error: getLevelSets: failed to read data/sets.txt!  Ensure ‘--static-data-dir=’ is set to a compiled neverball data directory.  Error: " ++ (show ioErr)) ()) >> BasicIBIOF ExitFailureBasicIOF
	msets <- Wait asets id
	sets_ <- err ||| return $ msets
	let setPathList = lines $ T.unpack sets_

	levelSets <- M.fromList . zip setPathList <$> mapM (getLevelSet cxt) setPathList
	return $ LevelSets {
		_lsExplicitSets = setPathList,
		_lsLevelSets    = levelSets
	}

-- | Get level set or fail.
--
-- e.g. ‘getLevelSet cxt "set-foo.txt"’
getLevelSet :: IBContext' a -> String -> ImmutaballIOF LevelSet
getLevelSet cxt path = do
	alevelSetContents <- BasicIBIOF $ ReadText (((cxt^.ibDirs.ibStaticDataDir)) </> path) id
	let err ioErr = BasicIBIOF (PutStrLn ("Error: getLevelSet: failed to read data/"++path++"!  Ensure ‘--static-data-dir=’ is set to a compiled neverball data directory.  Error: " ++ (show ioErr)) ()) >> BasicIBIOF ExitFailureBasicIOF
	mlevelSetContents <- Wait alevelSetContents id
	levelSetContents <- err ||| return $ mlevelSetContents

	let mlevelSet = parseLevelSetFile path (T.unpack $ levelSetContents)
	let perr errMsg = BasicIBIOF (PutStrLn ("Error: getLevelSet: failed to parse data/"++path++"!  (But we could read its contents.)  Error: " ++ errMsg) ()) >> BasicIBIOF ExitFailureBasicIOF
	levelSet <- perr ||| return $ mlevelSet
	return levelSet

parseLevelSetFile :: String -> String -> Either String LevelSet
parseLevelSetFile inputName inputContents = show +++ id $ parseLevelSetFile' inputName inputContents

parseLevelSetFile' :: SourceName -> String -> Either ParseError LevelSet
parseLevelSetFile' inputName inputContents = parse levelSetFileParser inputName inputContents

levelSetFileParser :: Parsec String () LevelSet
levelSetFileParser = error "TODO: unimplemented."
