{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Context.Config
	(
		ContextConfig(..), ctxCfgStaticDataDir, ctxCfgUserDataDir,
			ctxCfgConfigDataDir,
		defaultContextConfig
	) where

import Control.Lens

data ContextConfig = ContextConfig {
	_ctxCfgStaticDataDir :: Maybe FilePath,
	_ctxCfgUserDataDir   :: Maybe FilePath,
	_ctxCfgConfigDataDir :: Maybe FilePath
}
	deriving (Eq, Ord)
makeLenses ''ContextConfig

defaultContextConfig :: ContextConfig
defaultContextConfig = ContextConfig {
	_ctxCfgStaticDataDir = Nothing,
	_ctxCfgUserDataDir   = Nothing,
	_ctxCfgConfigDataDir = Nothing
}
