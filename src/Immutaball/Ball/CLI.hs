{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Ball.CLI
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
		immutaballHelp',
		immutaballIntroText,
		immutaballVersion,
		immutaballWithCLIConfig,
		cliIBDirs,
		getDefaultIBDirs,
		immutaballWithCLIConfig',
		immutaballWithNeverballrc,
		immutaballWithContext,
		initialImmutaball,
		defaultInitialImmutaball
	) where

-- Prelude imports
import Prelude ()
import Immutaball.Prelude

-- base imports
import Control.Arrow
import Data.List
import System.Console.GetOpt
import Text.Printf

-- external imports
import Control.Lens
import Control.Wire.Meta (isFallbackWire)
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
			"",

		Option ['Q'] ["skip-intro-text"] (NoArg . b $ cliCfgSkipIntroText .~ True)
			"",
		Option [] ["no-skip-intro-text"] (NoArg . b $ cliCfgSkipIntroText .~ False)
			"",

		Option [] ["help-detailed"] (NoArg . b $ cliCfgHelpDetailed .~ True)
			"",
		Option [] ["no-help-detailed"] (NoArg . b $ cliCfgHelpDetailed .~ False)
			"",

		Option [] ["request-override-physics"] (ReqArg (\str -> b $ cliCfgOverridePhysics .~ Just (Just str)) "")
			"",
		Option [] ["no-request-override-physics"] (NoArg . b $ cliCfgOverridePhysics .~ Nothing)
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
immutaballHelp = immutaballHelp' False

immutaballHelp' :: Bool -> String
immutaballHelp' detailed = intercalate "\n" $
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
		"\t\tSet user config directory path.",
		"\t--headless:",
		"\t\tDisable video and audio.  Useful for automated testing.",
		"\t-Q, --skip-intro-text:",
		"\t\tSilence the intro text on startup.",
		"\t--help-detailed:",
		"\t\tShow additional usage and exit."
	] ++ if' (not detailed) []
	[
		"",
		"Additional options:",
		"\t--request-override-physics=default:",
		"\t\tFor debugging or development, select the default physics algorithm.",
		"\t--request-override-physics=stationary:",
		"\t\tFor debugging or development, select the stationary physics algorithm.",
		"\t--request-override-physics=ghostly:",
		"\t\tFor debugging or development, select the ghostly physics algorithm.",
		"\t--request-override-physics=brute-force:",
		"\t\tFor debugging or development, select the brute-force physics algorithm.",
		"\t\tWARNING: does not perform well on large levels and may freeze!",
		"\t--request-override-physics=bsp:",
		"\t\tFor debugging or development, select the bsp physics algorithm."
	]

immutaballIntroText :: String
immutaballIntroText = intercalate "\n" $
	[
		"Welcome to Immutaball v" ++ immutaballVersion ++ "!",
		"",
		"WARNING: This prototype version currently only implements",
		"         basic functionality; much of the game is yet unimplemented.",
		"         A basic GUI, renderer, and physics is currently supported.",
		"         Currently, the physics does not fully support moving bodies,",
		"         and currently seems to have a few bugs.",
		"",
		"To use Immutaball, you'll need to build Neverball and call Immutaball",
		"with the path to the data dir in the Neverball build:",
		"\tcd ~/git",
		"\tgit clone https://github.com/Neverball/neverball",
		"\tcd ~/git/neverball",
		"\t# git checkout 1.7.0-alpha.3  # Optionally check out the most recent version known to be supported if there are issues.",
		"\tmake -j7",
		"",
		"\tcd ~/git",
		"\tgit clone https://github.com/nibzdil/immutaball",
		"\tcd ~/git/immutaball",
		"\tcabal build --enable-tests",
		"\tcabal run immutaball -- -d ~/git/neverball/data",
		"",
		"Alternatively, if there is a system-installed Neverball package",
		"already built, you can also use its data path instead when passing",
		"‘-d PATH’.",
		""
	] ++
	(if' (not isFallbackWire)
		[
			"Using external dependency ‘wires’ (not fallback implementation)."
		]
		[
			"Not using external package ‘wires’ for dependency",
			"(perhaps it failed to build); using fallback implementation",
			"(performance not necessarily optimized)."
		]
	) ++
	[
		"",
		"Pass ‘--help’ for usage information.",
		"Pass ‘-Q’ to silence this intro text."
	]

immutaballVersion :: String
immutaballVersion = "0.1.0.6-dev"

-- | Run immutaball.
immutaballWithCLIConfig :: StaticConfig -> CLIConfig -> ImmutaballIO
immutaballWithCLIConfig x'cfg cliCfg =
	result
	where
		showHelp :: ImmutaballIO
		showHelp = mkBIO . PutStrLn immutaballHelp $ mkBIO ExitSuccessBasicIOF
		showHelp' :: Bool -> ImmutaballIO
		showHelp' detailed = mkBIO . PutStrLn (immutaballHelp' detailed) $ mkBIO ExitSuccessBasicIOF
		showVersion :: ImmutaballIO
		showVersion = mkBIO . PutStrLn immutaballVersion $ mkBIO ExitSuccessBasicIOF
		showIntroTextThen :: ImmutaballIO -> ImmutaballIO
		showIntroTextThen
			| null immutaballIntroText      = id
			| (cliCfg^.cliCfgSkipIntroText) = id
			| otherwise =
				mkBIO . PutStrLn immutaballIntroText
		-- Allow CLI options to update the static config.
		x'cfg' :: StaticConfig
		x'cfg'
			| Just val <- cliCfg^.cliCfgOverridePhysics = x'cfg & x'cfgRequestAlternativePhysics .~ val
			| otherwise = x'cfg
		result :: ImmutaballIO
		result
			| cliCfg^.cliCfgHelpDetailed = showHelp' True
			| cliCfg^.cliCfgHelp         = showHelp
			| cliCfg^.cliCfgVersion      = showVersion
			| otherwise                  = showIntroTextThen $ immutaballWithCLIConfig' x'cfg' cliCfg

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
						result_2 = mkBIO . ReadTextSync neverballrcPath . (mkThrowIO |||) $ \neverballrcContents -> withParse $ parseNeverballrc neverballrcPath (T.unpack neverballrcContents)
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
			_cxtCfgHeadless     = (cliCfg^.cliCfgHeadless),

			_cxtCfgUseExistingSDLManager = (x'cfg^.x'cfgUseExistingSDLManager),
			_cxtCfgUseExistingGLManager  = (x'cfg^.x'cfgUseExistingGLManager)
		}

--- | Run immutaball after setting up an initial immutaball context.
immutaballWithContext :: IBContext -> ImmutaballIO
immutaballWithContext cxt0 =
	result
	where
		result :: ImmutaballIO
		result = controlImmutaball cxt0 (initialImmutaball cxt0)

initialImmutaball :: IBContext -> Immutaball
initialImmutaball cxt0 = maybe (defaultInitialImmutaball cxt0) id $ (cxt0^.ibInitialWire) cxt0

defaultInitialImmutaball :: IBContext -> Immutaball
defaultInitialImmutaball cxt0 = mkTitleState (Left cxt0)
