{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Putt.CLI.Config
	(
		CLIConfig(..),
		defaultCLIConfig
	) where

import Control.Lens

data CLIConfig = CLIConfig {
}
	deriving (Eq, Ord)
makeLenses ''CLIConfig

defaultCLIConfig :: CLIConfig
defaultCLIConfig = CLIConfig {
}
