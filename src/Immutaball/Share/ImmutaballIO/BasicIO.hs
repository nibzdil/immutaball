{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO/Basic.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}

module Immutaball.Share.ImmutaballIO.BasicIO
	(
		-- * BasicIO
		BasicIO,
		BasicIOF(..),
		runBasicIO,
		(<>>-),
		extractMesBasicIOF,

		-- * Runners
		runBasicIOIO,
		runDirectoryBasicIO,
		runSDLBasicIO,

		-- * BasicIO aliases that apply the Fixed wrapper
		mkEmptyBasicIO,
		mkAndBasicIO,
		mkThenBasicIO,
		mkExitSuccessBasicIO,
		mkExitFailureBasicIO,
		mkDirectoryIO,
		mkGetArgs,
		mkGetArgsSync,
		mkGetEnvironment,
		mkGetEnvironmentSync,
		mkPutStrLn,
		mkGetContents,
		mkGetContentsSync,
		mkDoesPathExist,
		mkDoesPathExistSync,
		mkWriteBytes,
		mkWriteText,
		mkReadBytes,
		mkReadBytesSync,
		mkReadText,
		mkReadTextSync,
		mkCreateDirectoryIfMissing,
		mkForkOS,
		mkSDLIO,
		mkDelayUs,
		mkGetUs,

		-- * Utils
		getUsIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent
import Control.Exception (catch, throwIO)
import Control.Monad
import Data.Time.Clock.System
import System.Environment
import System.Exit

import Control.Concurrent.Async
import Control.Concurrent.Thread.Delay
import Control.Parallel
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import System.Directory

import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Utils

-- * BasicIO

type BasicIO = Fixed BasicIOF
data BasicIOF me =
	  EmptyBasicIOF
	| AndBasicIOF me me
	| ThenBasicIOF me me
	| ExitSuccessBasicIOF
	| ExitFailureBasicIOF

	| DirectoryIO (DirectoryIOF me)

	| GetArgs (Async [String] -> me)
	| GetArgsSync ([String] -> me)
	| GetEnvironment (Async [(String, String)] -> me)
	| GetEnvironmentSync ([(String, String)] -> me)
	| PutStrLn String me
	| GetContents (Async String -> me)
	| GetContentsSync (String -> me)

	| DoesPathExist FilePath (Async Bool -> me)
	| DoesPathExistSync FilePath (Bool -> me)
	| WriteBytes FilePath BL.ByteString me
	| WriteText FilePath T.Text me
	-- | Optional error handler.
	| ReadBytes FilePath (Maybe (String -> me)) (Async (Maybe BL.ByteString) -> me)
	| ReadBytesSync FilePath (Maybe (String -> me)) (BL.ByteString -> me)
	-- | Optional error handler.
	| ReadText FilePath (Maybe (String -> me)) (Async (Maybe T.Text) -> me)
	| ReadTextSync FilePath (Maybe (String -> me)) (T.Text -> me)
	| CreateDirectoryIfMissing FilePath me
	| ForkOS me me

	| SDLIO (SDLIOF me)

	| DelayUs Integer me
	| GetUs (Integer -> me)

runBasicIO :: BasicIO -> IO ()
runBasicIO bio = cata runBasicIOIO bio

instance Semigroup (BasicIOF BasicIO) where
	a <> b = Fixed a `AndBasicIOF` Fixed b
instance Monoid (BasicIOF BasicIO) where
	mempty = EmptyBasicIOF

instance Semigroup BasicIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid BasicIO where
	mempty = Fixed mempty

instance Functor BasicIOF where
	fmap :: (a -> b) -> (BasicIOF a -> BasicIOF b)
	fmap _f (EmptyBasicIOF)       = EmptyBasicIOF
	fmap  f (AndBasicIOF a b)     = AndBasicIOF (f a) (f b)
	fmap  f (ThenBasicIOF a b)    = ThenBasicIOF (f a) (f b)
	fmap _f (ExitFailureBasicIOF) = ExitFailureBasicIOF
	fmap _f (ExitSuccessBasicIOF) = ExitSuccessBasicIOF

	fmap  f (DirectoryIO dio) = DirectoryIO (f <$> dio)

	fmap  f (GetArgs withArgs_)                  = GetArgs (f . withArgs_)
	fmap  f (GetArgsSync withArgs_)              = GetArgsSync (f . withArgs_)
	fmap  f (GetEnvironment withEnvironment)     = GetEnvironment (f . withEnvironment)
	fmap  f (GetEnvironmentSync withEnvironment) = GetEnvironmentSync (f . withEnvironment)
	fmap  f (PutStrLn str withUnit)              = PutStrLn str (f withUnit)
	fmap  f (GetContents withContents)           = GetContents (f . withContents)
	fmap  f (GetContentsSync withContents)       = GetContentsSync (f . withContents)

	fmap  f (DoesPathExist path withExists)            = DoesPathExist path (f .  withExists)
	fmap  f (DoesPathExistSync path withExists)        = DoesPathExistSync path (f .  withExists)
	fmap  f (WriteBytes path contents withUnit)        = WriteBytes path contents (f withUnit)
	fmap  f (WriteText path contents withUnit)         = WriteText path contents (f withUnit)
	fmap  f (ReadBytes path mwithErr withContents)     = ReadBytes path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadBytesSync path mwithErr withContents) = ReadBytesSync path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadText path mwithErr withContents)      = ReadText path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadTextSync path mwithErr withContents)  = ReadTextSync path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (CreateDirectoryIfMissing path withUnit)   = CreateDirectoryIfMissing path (f withUnit)
	fmap  f (ForkOS bio withUnit)                      = ForkOS (f bio) (f withUnit)

	fmap  f (SDLIO sdlio) = SDLIO (f <$> sdlio)

	fmap  f (DelayUs us withUnit) = DelayUs us (f withUnit)
	fmap  f (GetUs withUs)        = GetUs (f . withUs)

{-
instance Foldable BasicIOF where
	-- TODO
instance Traversable BasicIOF where
	-- TODO
-}

-- | Add an ordering constraint.
infixr 6 <>>-
(<>>-) :: BasicIO -> BasicIO -> BasicIO
(<>>-) = mkThenBasicIO

extractMesBasicIOF :: BasicIOF me -> [me]
extractMesBasicIOF (EmptyBasicIOF)                               = []
extractMesBasicIOF (AndBasicIOF a b)                             = [a, b]
extractMesBasicIOF (ThenBasicIOF a b)                            = [a, b]
extractMesBasicIOF (ExitSuccessBasicIOF)                         = []
extractMesBasicIOF (ExitFailureBasicIOF)                         = []
extractMesBasicIOF (DirectoryIO dio)                             = extractMesDirectoryIOF dio
extractMesBasicIOF (GetArgs _withArgs_)                          = []
extractMesBasicIOF (GetArgsSync _withArgs_)                      = []
extractMesBasicIOF (GetEnvironment _withEnvironment)             = []
extractMesBasicIOF (GetEnvironmentSync _withEnvironment)         = []
extractMesBasicIOF (PutStrLn _str withUnit)                      = [withUnit]
extractMesBasicIOF (GetContents _withContents)                   = []
extractMesBasicIOF (GetContentsSync _withContents)               = []
extractMesBasicIOF (DoesPathExist _path _withExists)             = []
extractMesBasicIOF (DoesPathExistSync _path _withExists)         = []
extractMesBasicIOF (WriteBytes _path _contents withUnit)         = [withUnit]
extractMesBasicIOF (WriteText _path _contents withUnit)          = [withUnit]
extractMesBasicIOF (ReadBytes _path _mwithErr _withContents)     = []
extractMesBasicIOF (ReadBytesSync _path _mwithErr _withContents) = []
extractMesBasicIOF (ReadText _path _mwithErr _withContents)      = []
extractMesBasicIOF (ReadTextSync _path _mwithErr _withContents)  = []
extractMesBasicIOF (CreateDirectoryIfMissing _path withUnit)     = [withUnit]
extractMesBasicIOF (ForkOS bio withUnit)                         = [bio, withUnit]
extractMesBasicIOF (SDLIO sdlio)                                 = extractMesSDLIOF sdlio
extractMesBasicIOF (DelayUs _us withUnit)                        = [withUnit]
extractMesBasicIOF (GetUs _withUs)                               = []

-- * Runners

runBasicIOIO :: BasicIOF (IO ()) -> IO ()
runBasicIOIO (EmptyBasicIOF)                               = return ()
runBasicIOIO (AndBasicIOF a b)                             = a `par` b `par` concurrently_ a b
runBasicIOIO (ThenBasicIOF a b)                            = a >> b
runBasicIOIO (ExitSuccessBasicIOF)                         = exitSuccess
runBasicIOIO (ExitFailureBasicIOF)                         = exitFailure
runBasicIOIO (DirectoryIO dio)                             = runDirectoryIOIO $ dio
runBasicIOIO (GetArgs withArgs_)                           = withAsync getArgs withArgs_
runBasicIOIO (GetArgsSync withArgs_)                       = getArgs >>= withArgs_
runBasicIOIO (GetEnvironment withEnvironment)              = withAsync getEnvironment withEnvironment
runBasicIOIO (GetEnvironmentSync withEnvironment)          = getEnvironment >>= withEnvironment
runBasicIOIO (PutStrLn str withUnit)                       = putStrLn str >> withUnit
runBasicIOIO (GetContents withContents)                    = withAsync getContents withContents
runBasicIOIO (GetContentsSync withContents)                = getContents >>= withContents
runBasicIOIO (DoesPathExist path withExists)               = withAsync (doesPathExist path) withExists
runBasicIOIO (DoesPathExistSync path withExists)           = doesPathExist path >>= withExists
runBasicIOIO (WriteBytes path contents withUnit)           = BL.writeFile path contents >> withUnit
runBasicIOIO (WriteText path contents withUnit)            = TIO.writeFile path contents >> withUnit
runBasicIOIO (ReadBytes path mwithErr withContents)        =
	withAsync (((Just <$> BL.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e))) withContents
runBasicIOIO (ReadBytesSync path mwithErr withContents)    =
	((Just <$> BL.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e)) >>= maybe (return ()) (id . withContents)
runBasicIOIO (ReadText path mwithErr withContents)         =
	withAsync (((Just <$> TIO.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e))) withContents
runBasicIOIO (ReadTextSync path mwithErr withContents)     =
	((Just <$> TIO.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e)) >>= maybe (return ()) (id . withContents)
runBasicIOIO (CreateDirectoryIfMissing path withUnit)      = createDirectoryIfMissing True path >> withUnit
runBasicIOIO (ForkOS bio withUnit)                         = (void . forkOS) bio >> withUnit
runBasicIOIO (SDLIO sdlio)                                 = runSDLIOIO $ sdlio
runBasicIOIO (DelayUs us withUnit)                         = delay us >> withUnit
runBasicIOIO (GetUs withUs)                                = getUsIO >>= withUs

runDirectoryBasicIO :: DirectoryIO -> BasicIO
runDirectoryBasicIO dio = Fixed . DirectoryIO $ runDirectoryBasicIO <$> getFixed dio

runSDLBasicIO :: SDLIO -> BasicIO
runSDLBasicIO sdlio = Fixed . SDLIO $ runSDLBasicIO <$> getFixed sdlio
--runSDLBasicIO (Fixed (SDLInit subsystems sdlio)) = Fixed . SDLIO $ SDLInit subsystems (runSDLBasicIO sdlio)

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyBasicIO :: BasicIO
mkEmptyBasicIO = Fixed $ EmptyBasicIOF

mkAndBasicIO :: BasicIO -> BasicIO -> BasicIO
mkAndBasicIO a b = Fixed $ AndBasicIOF a b

mkThenBasicIO :: BasicIO -> BasicIO -> BasicIO
mkThenBasicIO a b = Fixed $ ThenBasicIOF a b

mkExitSuccessBasicIO :: BasicIO
mkExitSuccessBasicIO = Fixed $ ExitFailureBasicIOF

mkExitFailureBasicIO :: BasicIO
mkExitFailureBasicIO = Fixed $ ExitFailureBasicIOF

mkDirectoryIO :: DirectoryIOF BasicIO -> BasicIO
mkDirectoryIO dio = Fixed $ DirectoryIO dio

mkGetArgs :: (Async [String] -> BasicIO) -> BasicIO
mkGetArgs withArgs_ = Fixed $ GetArgs withArgs_

mkGetArgsSync :: ([String] -> BasicIO) -> BasicIO
mkGetArgsSync withArgs_ = Fixed $ GetArgsSync withArgs_

mkGetEnvironment :: (Async [(String, String)] -> BasicIO) -> BasicIO
mkGetEnvironment withEnvironment = Fixed $ GetEnvironment withEnvironment

mkGetEnvironmentSync :: ([(String, String)] -> BasicIO) -> BasicIO
mkGetEnvironmentSync withEnvironment = Fixed $ GetEnvironmentSync withEnvironment

mkPutStrLn :: String -> BasicIO -> BasicIO
mkPutStrLn str bio = Fixed $ PutStrLn str bio

mkGetContents :: (Async String -> BasicIO) -> BasicIO
mkGetContents withContents = Fixed $ GetContents withContents

mkGetContentsSync :: (String -> BasicIO) -> BasicIO
mkGetContentsSync withContents = Fixed $ GetContentsSync withContents

mkDoesPathExist :: FilePath -> (Async Bool -> BasicIO) -> BasicIO
mkDoesPathExist path withExists = Fixed $ DoesPathExist path withExists

mkDoesPathExistSync :: FilePath -> (Bool -> BasicIO) -> BasicIO
mkDoesPathExistSync path withExists = Fixed $ DoesPathExistSync path withExists

mkWriteBytes :: FilePath -> BL.ByteString -> BasicIO -> BasicIO
mkWriteBytes path contents withUnit = Fixed $ WriteBytes path contents withUnit

mkWriteText :: FilePath -> T.Text -> BasicIO -> BasicIO
mkWriteText path contents withUnit = Fixed $ WriteText path contents withUnit

mkReadBytes :: FilePath -> Maybe (String -> BasicIO) -> (Async (Maybe BL.ByteString) -> BasicIO) -> BasicIO
mkReadBytes path mwithErr withContents = Fixed $ ReadBytes path mwithErr withContents

mkReadBytesSync :: FilePath -> Maybe (String -> BasicIO) -> (BL.ByteString -> BasicIO) -> BasicIO
mkReadBytesSync path mwithErr withContents = Fixed $ ReadBytesSync path mwithErr withContents

mkReadText :: FilePath -> Maybe (String -> BasicIO) -> (Async (Maybe T.Text) -> BasicIO) -> BasicIO
mkReadText path mwithErr withContents = Fixed $ ReadText path mwithErr withContents

mkReadTextSync :: FilePath -> Maybe (String -> BasicIO) -> (T.Text -> BasicIO) -> BasicIO
mkReadTextSync path mwithErr withContents = Fixed $ ReadTextSync path mwithErr withContents

mkCreateDirectoryIfMissing :: FilePath -> BasicIO -> BasicIO
mkCreateDirectoryIfMissing path withUnit = Fixed $ CreateDirectoryIfMissing path withUnit

mkForkOS :: BasicIO -> BasicIO -> BasicIO
mkForkOS bio withUnit = Fixed $ ForkOS bio withUnit

mkSDLIO :: SDLIOF BasicIO -> BasicIO
mkSDLIO sdlio = Fixed $ SDLIO sdlio

-- | Microseconds thread delay, with ‘unbounded-delays’.
mkDelayUs :: Integer -> BasicIO -> BasicIO
mkDelayUs us withUnit = Fixed $ DelayUs us withUnit

mkGetUs :: (Integer -> BasicIO) -> BasicIO
mkGetUs withUs = Fixed $ GetUs withUs

-- * Utils

getUsIO :: IO Integer
getUsIO = getSystemTime >>= \(MkSystemTime secs nanosecs) -> return $ (1000000 * (fromIntegral secs)) + ((fromIntegral nanosecs) `div` 1000)
