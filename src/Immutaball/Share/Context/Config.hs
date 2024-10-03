{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Context.Config
	(
		IBDirs(..), ibStaticDataDir, ibUserDataDir, ibUserConfigDir,
		ContextConfig(..), ctxCfgStaticConfig, ctxCfgDirs, ctxCfgNeverballrc
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Lens

import Immutaball.Share.Config

data IBDirs = IBDirs {
	_ibStaticDataDir :: FilePath,
	_ibUserDataDir   :: FilePath,
	_ibUserConfigDir :: FilePath
}
	deriving (Eq, Ord, Show)
makeLenses ''IBDirs

data ContextConfig = ContextConfig {
	_ctxCfgStaticConfig :: StaticConfig,
	_ctxCfgDirs         :: IBDirs,
	_ctxCfgNeverballrc  :: Neverballrc
}
makeLenses ''ContextConfig
