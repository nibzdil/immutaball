{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Putt.CLI
	(
		main,
		immutaballMain,
		immutaballCLIMain',
		immutaballCLIMain,
		mainImmutaballIO',
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
		immutaballWithContext
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
import Immutaball.Share.Config
import Immutaball.Share.Config.Parser
import Immutaball.Share.Config.Printer
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.State
import Immutaball.Share.Utils

main :: IO ()
main = immutaballMain

immutaballMain :: IO ()
immutaballMain = immutaballCLIMain'

immutaballCLIMain' :: IO ()
immutaballCLIMain' = immutaballCLIMain defaultStaticConfig

immutaballCLIMain :: StaticConfig -> IO ()
immutaballCLIMain x'cfg = do
	runImmutaballIO (mainImmutaballIO x'cfg)

mainImmutaballIO' :: ImmutaballIO
mainImmutaballIO' = mainImmutaballIO defaultStaticConfig

mainImmutaballIO :: StaticConfig -> ImmutaballIO
mainImmutaballIO x'cfg =
	mkBIO . GetArgsSync $ immutaballWithArgs x'cfg

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
			"",

		Option [] ["headless"] (NoArg . b $ cliCfgHeadless .~ True)
			"",
		Option [] ["no-headless"] (NoArg . b $ cliCfgHeadless .~ False)
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
		"Usage: immutaputt [options…]",
		"",
		"Options:",
		"\t--help: Show usage and exit.",
		"\t--version: Show version and exit.",
		"\t-d PATH, --data PATH, --static-data-dir PATH:",
		"\t\tSet static data directory path.",
		"\t--user-data-dir PATH:",
		"\t\tSet user data directory path.",
		"\t--user-config-dir PATH:",
		"\t\tSet user config directory path.",
		"\t--headless:",
		"\t\tDisable video and audio.  Useful for automated testing."
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
immutaballWithNeverballrc x'cfg cliCfg ibDirs_ nrcCfg =
	result
	where
		result :: ImmutaballIO
		result = withSDL cxtCfg immutaballWithContext
		cxtCfg :: ContextConfig
		cxtCfg = ContextConfig {
			_cxtCfgStaticConfig = x'cfg,
			_cxtCfgDirs         = ibDirs_,
			_cxtCfgNeverballrc  = nrcCfg,
			_cxtCfgInitialWire  = joinMaybeResult $ (x'cfg^.x'cfgInitialWireWithCxt),
			_cxtCfgHeadless     = (cliCfg^.cliCfgHeadless)
		}

--- | Run immutaball after setting up an immutaball context.
immutaballWithContext :: IBContext -> ImmutaballIO
immutaballWithContext _cxt0 =
	result
	where
		result :: ImmutaballIO
		result = mkBIO . PutStrLn "Internal error: unimplemented." $ mkBIO ExitFailureBasicIOF
