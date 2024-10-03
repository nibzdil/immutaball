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
		mkSDLIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent
import Control.Exception (catch, throwIO)
import Control.Monad
import System.Environment
import System.Exit

import Control.Concurrent.Async
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
	| PutStrLn String
	| GetContents (Async String -> me)
	| GetContentsSync (String -> me)

	| DoesPathExist FilePath (Async Bool -> me)
	| DoesPathExistSync FilePath (Bool -> me)
	| WriteBytes FilePath BL.ByteString
	| WriteText FilePath T.Text
	-- | Optional error handler.
	| ReadBytes FilePath (Maybe (String -> me)) (Async (Maybe BL.ByteString) -> me)
	| ReadBytesSync FilePath (Maybe (String -> me)) (BL.ByteString -> me)
	-- | Optional error handler.
	| ReadText FilePath (Maybe (String -> me)) (Async (Maybe T.Text) -> me)
	| ReadTextSync FilePath (Maybe (String -> me)) (T.Text -> me)
	| CreateDirectoryIfMissing FilePath
	| ForkOS me

	| SDLIO (SDLIOF me)

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
	fmap _f (PutStrLn str)                       = PutStrLn str
	fmap  f (GetContents withContents)           = GetContents (f . withContents)
	fmap  f (GetContentsSync withContents)       = GetContentsSync (f . withContents)

	fmap  f (DoesPathExist path withExists)            = DoesPathExist path (f .  withExists)
	fmap  f (DoesPathExistSync path withExists)        = DoesPathExistSync path (f .  withExists)
	fmap _f (WriteBytes path contents)                 = WriteBytes path contents
	fmap _f (WriteText path contents)                  = WriteText path contents
	fmap  f (ReadBytes path mwithErr withContents)     = ReadBytes path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadBytesSync path mwithErr withContents) = ReadBytesSync path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadText path mwithErr withContents)      = ReadText path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadTextSync path mwithErr withContents)  = ReadTextSync path ((f .) <$> mwithErr) (f . withContents)
	fmap _f (CreateDirectoryIfMissing path)            = CreateDirectoryIfMissing path
	fmap  f (ForkOS ibio)                              = ForkOS (f ibio)

	fmap  f (SDLIO sdlio) = SDLIO (f <$> sdlio)

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
runBasicIOIO (PutStrLn str)                                = putStrLn str
runBasicIOIO (GetContents withContents)                    = withAsync getContents withContents
runBasicIOIO (GetContentsSync withContents)                = getContents >>= withContents
runBasicIOIO (DoesPathExist path withExists)               = withAsync (doesPathExist path) withExists
runBasicIOIO (DoesPathExistSync path withExists)           = doesPathExist path >>= withExists
runBasicIOIO (WriteBytes path contents)                    = BL.writeFile path contents
runBasicIOIO (WriteText path contents)                     = TIO.writeFile path contents
runBasicIOIO (ReadBytes path mwithErr withContents)        =
	withAsync (((Just <$> BL.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e))) withContents
runBasicIOIO (ReadBytesSync path mwithErr withContents)    =
	((Just <$> BL.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e)) >>= maybe (return ()) (id . withContents)
runBasicIOIO (ReadText path mwithErr withContents)         =
	withAsync (((Just <$> TIO.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e))) withContents
runBasicIOIO (ReadTextSync path mwithErr withContents)     =
	((Just <$> TIO.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e)) >>= maybe (return ()) (id . withContents)
runBasicIOIO (CreateDirectoryIfMissing path)               = createDirectoryIfMissing True path
runBasicIOIO (ForkOS ibio)                                 = void . forkOS $ ibio
runBasicIOIO (SDLIO sdlio)                                 = runSDLIOIO $ sdlio

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

mkPutStrLn :: String -> BasicIO
mkPutStrLn str = Fixed $ PutStrLn str

mkGetContents :: (Async String -> BasicIO) -> BasicIO
mkGetContents withContents = Fixed $ GetContents withContents

mkGetContentsSync :: (String -> BasicIO) -> BasicIO
mkGetContentsSync withContents = Fixed $ GetContentsSync withContents

mkDoesPathExist :: FilePath -> (Async Bool -> BasicIO) -> BasicIO
mkDoesPathExist path withExists = Fixed $ DoesPathExist path withExists

mkDoesPathExistSync :: FilePath -> (Bool -> BasicIO) -> BasicIO
mkDoesPathExistSync path withExists = Fixed $ DoesPathExistSync path withExists

mkWriteBytes :: FilePath -> BL.ByteString -> BasicIO
mkWriteBytes path contents = Fixed $ WriteBytes path contents

mkWriteText :: FilePath -> T.Text -> BasicIO
mkWriteText path contents = Fixed $ WriteText path contents

mkReadBytes :: FilePath -> Maybe (String -> BasicIO) -> (Async (Maybe BL.ByteString) -> BasicIO) -> BasicIO
mkReadBytes path mwithErr withContents = Fixed $ ReadBytes path mwithErr withContents

mkReadBytesSync :: FilePath -> Maybe (String -> BasicIO) -> (BL.ByteString -> BasicIO) -> BasicIO
mkReadBytesSync path mwithErr withContents = Fixed $ ReadBytesSync path mwithErr withContents

mkReadText :: FilePath -> Maybe (String -> BasicIO) -> (Async (Maybe T.Text) -> BasicIO) -> BasicIO
mkReadText path mwithErr withContents = Fixed $ ReadText path mwithErr withContents

mkReadTextSync :: FilePath -> Maybe (String -> BasicIO) -> (T.Text -> BasicIO) -> BasicIO
mkReadTextSync path mwithErr withContents = Fixed $ ReadTextSync path mwithErr withContents

mkCreateDirectoryIfMissing :: FilePath -> BasicIO
mkCreateDirectoryIfMissing path = Fixed $ CreateDirectoryIfMissing path

mkForkOS :: BasicIO -> BasicIO
mkForkOS ibio = Fixed $ ForkOS ibio

mkSDLIO :: SDLIOF BasicIO -> BasicIO
mkSDLIO sdlio = Fixed $ SDLIO sdlio
