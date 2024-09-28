{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Ball.CLI
	(
		main,
		immutaballMain,
		immutaballCLIMain,
		mainImmutaballIO,
		immutaballOptions,
		immutaballWithArgs,
		immutaballHelp,
		immutaballVersion,
		immutaballWithCLIConfig,
		immutaballWithCLIConfig'
	) where

import Data.List
import System.Console.GetOpt
import Text.Printf

import Control.Lens

import Immutaball.Ball.CLI.Config
import Immutaball.Share.ImmutaballIO
--import Immutaball.Share.Utils

main :: IO ()
main = immutaballMain

immutaballMain :: IO ()
immutaballMain = immutaballCLIMain

immutaballCLIMain :: IO ()
immutaballCLIMain = do
	runImmutaballIO mainImmutaballIO

mainImmutaballIO :: ImmutaballIO
mainImmutaballIO =
	mkGetArgs immutaballWithArgs

immutaballOptions :: [OptDescr CLIConfigBuilder]
immutaballOptions =
	[
		Option ['H', '?'] ["help"]       (NoArg . b $ cliCfgHelp    .~ True)
			"Show help and exit",
		Option []         ["no-help"]    (NoArg . b $ cliCfgHelp    .~ False)
			"",
		Option ['V']      ["version"]    (NoArg . b $ cliCfgVersion .~ True)
			"Show version and exit",
		Option []         ["no-version"] (NoArg . b $ cliCfgVersion .~ False)
			"",

		Option ['d'] ["static-data-dir", "data"] (ReqArg (\path -> b $ cliCfgStaticDataDir .~ Just path) "")
			"Set static data directory path",
		Option []    ["user-data-dir"]           (ReqArg (\path -> b $ cliCfgUserDataDir   .~ Just path) "")
			"Set user data directory path",
		Option []    ["user-config-dir"]         (ReqArg (\path -> b $ cliCfgUserConfigDir .~ Just path) "")
			"Set user config directory path",

		Option [] ["no-static-data-dir"] (NoArg . b $ cliCfgStaticDataDir .~ Nothing)
			"",
		Option [] ["no-user-data-dir"]   (NoArg . b $ cliCfgUserDataDir   .~ Nothing)
			"",
		Option [] ["no-user-config-dir"] (NoArg . b $ cliCfgUserConfigDir .~ Nothing)
			""
	]
	where b = CLIConfigBuilder

immutaballWithArgs :: [String] -> ImmutaballIO
immutaballWithArgs args =
	result
	where
		opts :: [CLIConfigBuilder]
		nonopts, errs :: [String]
		(opts, nonopts, errs) = getOpt Permute immutaballOptions args
		cliCfg = buildCLIConfig (mconcat opts)
		showErrs :: [String] -> ImmutaballIO
		showErrs errs_ = foldr (<>>) mempty $ showErr <$> errs_
		showErr :: String -> ImmutaballIO
		showErr errMsg = mkPutStrLn $ printf "Error: CLI getOpt error: %s" errMsg
		result :: ImmutaballIO
		result
			| (not . null) errs    = showErrs errs <>> mkExitFailureImmutaballIO
			| (not . null) nonopts = showErrs $ map (\nonopt -> printf "nonoptions currently not supported; received nonoption ‘%s’" nonopt) nonopts
			| otherwise            = immutaballWithCLIConfig cliCfg

immutaballHelp :: String
immutaballHelp = intercalate "\n" $
	[
		"Usage: immutaball [options…]",
		"",
		"Options:",
		"\t--help: Show usage and exit.",
		"\t--version: Show version and exit.",
		"\t-d PATH, --data PATH, --static-data-dir PATH:",
		"\t\tSet static data directory path.",
		"\t--user-data-dir PATH:",
		"\t\tSet user data directory path.",
		"\t--user-config-dir PATH:",
		"\t\tSet user config directory path."
	]

immutaballVersion :: String
immutaballVersion = "0.1.0.1-dev"

-- | Run immutaball.
immutaballWithCLIConfig :: CLIConfig -> ImmutaballIO
immutaballWithCLIConfig cliCfg =
	result
	where
		showHelp :: ImmutaballIO
		showHelp = mkPutStrLn immutaballHelp <>> mkExitSuccessImmutaballIO
		showVersion :: ImmutaballIO
		showVersion = mkPutStrLn immutaballVersion <>> mkExitSuccessImmutaballIO
		result :: ImmutaballIO
		result
			| cliCfg^.cliCfgHelp    = showHelp
			| cliCfg^.cliCfgVersion = showVersion
			| otherwise             = immutaballWithCLIConfig' cliCfg

-- | Run immutaball after basic setup like checking for ‘--help’.
immutaballWithCLIConfig' :: CLIConfig -> ImmutaballIO
immutaballWithCLIConfig' _cliCfg =
	result
	where
		result :: ImmutaballIO
		result = mkPutStrLn "Internal error: unimplemented." <>> mkExitFailureImmutaballIO
