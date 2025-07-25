{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, InstanceSigs #-}

module Immutaball.Ball.CLI.Config
	(
		CLIConfig(..), cliCfgHelp, cliCfgVersion, cliCfgStaticDataDir,
			cliCfgUserDataDir, cliCfgUserConfigDir, cliCfgHeadless,
			cliCfgSkipIntroText, cliCfgHelpDetailed, cliCfgOverridePhysics,
		defaultCLIConfig,
		CLIConfigBuilder(..), modifyCLIConfig,
		buildCLIConfig
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

data CLIConfig = CLIConfig {
	_cliCfgHelp            :: Bool,
	_cliCfgVersion         :: Bool,
	_cliCfgStaticDataDir   :: Maybe FilePath,
	_cliCfgUserDataDir     :: Maybe FilePath,
	_cliCfgUserConfigDir   :: Maybe FilePath,
	_cliCfgHeadless        :: Bool,
	_cliCfgSkipIntroText   :: Bool,
	_cliCfgHelpDetailed    :: Bool,
	_cliCfgOverridePhysics :: Maybe (Maybe String)
}
	deriving (Eq, Ord)
makeLenses ''CLIConfig

defaultCLIConfig :: CLIConfig
defaultCLIConfig = CLIConfig {
	_cliCfgHelp            = False,
	_cliCfgVersion         = False,
	_cliCfgStaticDataDir   = Nothing,
	_cliCfgUserDataDir     = Nothing,
	_cliCfgUserConfigDir   = Nothing,
	_cliCfgHeadless        = False,
	_cliCfgSkipIntroText   = False,
	_cliCfgHelpDetailed    = False,
	_cliCfgOverridePhysics = Nothing
}

newtype CLIConfigBuilder = CLIConfigBuilder {_modifyCLIConfig :: CLIConfig -> CLIConfig}
makeLenses ''CLIConfigBuilder

instance Semigroup CLIConfigBuilder where
	(<>) :: CLIConfigBuilder -> CLIConfigBuilder -> CLIConfigBuilder
	a <> b = CLIConfigBuilder $ (b^.modifyCLIConfig) . (a^.modifyCLIConfig)
instance Monoid CLIConfigBuilder where
	mempty = CLIConfigBuilder $ id

buildCLIConfig :: CLIConfigBuilder -> CLIConfig
buildCLIConfig builder = (builder^.modifyCLIConfig) defaultCLIConfig
