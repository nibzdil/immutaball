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
		cliIBDirs,
		getDefaultIBDirs,
		immutaballWithCLIConfig',
		immutaballWithNeverballrc,
		immutaballWithContext,
		initialImmutaball
	) where

-- Prelude imports
import Prelude ()
import Immutaball.Prelude

-- base imports
import Data.List
import System.Console.GetOpt
import Text.Printf

-- external imports
import Control.Lens
import qualified Data.Text as T
import System.FilePath

-- internal imports
import Immutaball.Ball.CLI.Config
import Immutaball.Ball.State.Title
import Immutaball.Share.Config
import Immutaball.Share.Config.Parser
import Immutaball.Share.Config.Printer
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
import Immutaball.Share.Controller
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.State

import Debug.Trace as D  ---------------------------- TODO--
import Control.Monad.Fix
import Immutaball.Share.Utils

main :: IO ()
main = immutaballMain

immutaballMain :: IO ()
immutaballMain = immutaballCLIMain

immutaballCLIMain :: IO ()
immutaballCLIMain = do
	--runImmutaballIO mainImmutaballIO
	runImmutaballIO dbg

-- TODO: FIXME: okay, we've narrowed down the mfix bug.  This hangs and never prints.
dbg :: ImmutaballIO
dbg = Fixed . mfix $ \a -> BasicImmutaballIOF . PutStrLn "TODO: DEBUG dbg this mfix works!" $ mkBIO ExitFailureBasicIOF

mainImmutaballIO :: ImmutaballIO
mainImmutaballIO =
	mkBIO . GetArgsSync $ immutaballWithArgs defaultStaticConfig

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

immutaballWithArgs :: StaticConfig -> [String] -> ImmutaballIO
immutaballWithArgs x'cfg args =
	result
	where
		opts :: [CLIConfigBuilder]
		nonopts, errs :: [String]
		(opts, nonopts, errs) = getOpt Permute immutaballOptions args
		cliCfg = buildCLIConfig (mconcat opts)
		showErrs :: [String] -> ImmutaballIO
		showErrs errs_ = foldr (<>>) mempty $ showErr <$> errs_
		showErr :: String -> ImmutaballIO
		showErr errMsg = mkBIO . PutStrLn (printf "Error: CLI getOpt error: %s" errMsg) $ mkBIO ExitFailureBasicIOF
		result :: ImmutaballIO
		result
			| (not . null) errs    = showErrs errs <>> mkBIO ExitFailureBasicIOF
			| (not . null) nonopts = showErrs $ map (\nonopt -> printf "nonoptions currently not supported; received nonoption ‘%s’" nonopt) nonopts
			| otherwise            = immutaballWithCLIConfig x'cfg cliCfg

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
immutaballWithCLIConfig :: StaticConfig -> CLIConfig -> ImmutaballIO
immutaballWithCLIConfig x'cfg cliCfg =
	result
	where
		showHelp :: ImmutaballIO
		showHelp = mkBIO . PutStrLn immutaballHelp $ mkBIO ExitSuccessBasicIOF
		showVersion :: ImmutaballIO
		showVersion = mkBIO . PutStrLn immutaballVersion $ mkBIO ExitSuccessBasicIOF
		result :: ImmutaballIO
		result
			| cliCfg^.cliCfgHelp    = showHelp
			| cliCfg^.cliCfgVersion = showVersion
			| otherwise             = immutaballWithCLIConfig' x'cfg cliCfg

cliIBDirs :: CLIConfig -> IBDirs -> IBDirs
cliIBDirs cliCfg defaultIBDirs = IBDirs {
	_ibStaticDataDir = maybe (defaultIBDirs^.ibStaticDataDir) id (cliCfg^.cliCfgStaticDataDir),
	_ibUserDataDir   = maybe (defaultIBDirs^.ibUserDataDir)   id (cliCfg^.cliCfgUserDataDir),
	_ibUserConfigDir = maybe (defaultIBDirs^.ibUserConfigDir) id (cliCfg^.cliCfgUserConfigDir)
}

getDefaultIBDirs :: StaticConfig -> (IBDirs -> ImmutaballIO) -> ImmutaballIO
getDefaultIBDirs x'cfg withIBDirs =
	either (\d f -> mkBIO . DirectoryIO . fmap f $ d) (&) (x'cfg^.defaultStaticDataDir) $ \defaultStaticDataDir_ ->
	either (\d f -> mkBIO . DirectoryIO . fmap f $ d) (&) (x'cfg^.defaultUserDataDir)   $ \defaultUserDataDir_ ->
	either (\d f -> mkBIO . DirectoryIO . fmap f $ d) (&) (x'cfg^.defaultUserConfigDir) $ \defaultUserConfigDir_ ->
	withIBDirs $ IBDirs {
		_ibStaticDataDir = defaultStaticDataDir_,
		_ibUserDataDir   = defaultUserDataDir_,
		_ibUserConfigDir = defaultUserConfigDir_
	}

-- | Run immutaball after basic setup like checking for ‘--help’.
immutaballWithCLIConfig' :: StaticConfig -> CLIConfig -> ImmutaballIO
immutaballWithCLIConfig' x'cfg cliCfg =
	result
	where
		result :: ImmutaballIO
		result = getDefaultIBDirs x'cfg withDefaultIBDirs
		withDefaultIBDirs :: IBDirs -> ImmutaballIO
		withDefaultIBDirs defaultIBDirs = result_
			where
				result_ :: ImmutaballIO
				result_ = createUserDirsIfMissing <>> (mkBIO . DoesPathExistSync neverballrcPath $ withNeverballrcExists)
				createUserDirsIfMissing :: ImmutaballIO
				createUserDirsIfMissing = mconcat . map (mkBIO . flip CreateDirectoryIfMissing mempty) $ [ibDirs_^.ibUserDataDir, ibDirs_^.ibUserConfigDir]
				ibDirs_ :: IBDirs
				ibDirs_ = cliIBDirs cliCfg defaultIBDirs
				neverballrcPath :: FilePath
				neverballrcPath = (ibDirs_^.ibUserConfigDir) </> (x'cfg^.configFilename)
				writeDefaultNeverballrc :: ImmutaballIO
				writeDefaultNeverballrc = mkBIO $ WriteText neverballrcPath (T.pack . showNeverballrc $ defaultNeverballrc) mempty
				defaultNeverballrc :: Neverballrc
				defaultNeverballrc = defaultConfig
				withNeverballrcExists :: Bool -> ImmutaballIO
				withNeverballrcExists False = writeDefaultNeverballrc <>> withNeverballrcExists True
				withNeverballrcExists True = result_2
					where
						result_2 :: ImmutaballIO
						result_2 = mkBIO . ReadTextSync neverballrcPath Nothing $ \neverballrcContents -> withParse $ parseNeverballrc neverballrcPath (T.unpack neverballrcContents)
						withParse :: Either String Neverballrc -> ImmutaballIO
						withParse (Left  parseError)   = mkBIO . PutStrLn (printf "Error: failed to parse neverballrc: %s" parseError) $ mkBIO ExitFailureBasicIOF
						withParse (Right neverballrc_) = immutaballWithNeverballrc x'cfg cliCfg ibDirs_ neverballrc_

--- | Run immutaball after getting neverballrc and dirs.
immutaballWithNeverballrc :: StaticConfig -> CLIConfig -> IBDirs -> Neverballrc -> ImmutaballIO
immutaballWithNeverballrc x'cfg _cliCfg ibDirs_ nrcCfg =
	result
	where
		result :: ImmutaballIO
		result = withSDL ctxCfg immutaballWithContext
		ctxCfg :: ContextConfig
		ctxCfg = ContextConfig {
			_ctxCfgStaticConfig = x'cfg,
			_ctxCfgDirs         = ibDirs_,
			_ctxCfgNeverballrc  = nrcCfg
		}

--- | Run immutaball after setting up an initial immutaball context.
immutaballWithContext :: IBContext -> ImmutaballIO
immutaballWithContext cxt0 =
	result
	where
		result :: ImmutaballIO
		result = controlImmutaball cxt0 (initialImmutaball cxt0)

initialImmutaball :: IBContext -> Immutaball
initialImmutaball cxt0 = mkTitleState cxt0
