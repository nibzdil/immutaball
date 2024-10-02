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
		(<>>),

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
		mkGetDirectory,
		mkGetArgs,
		mkGetEnvironment,
		mkPutStrLn,
		mkGetContents,
		mkDoesPathExist,
		mkWriteBytes,
		mkWriteText,
		mkReadBytes,
		mkReadText,
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

	| GetDirectory (DirectoryIOF me) (FilePath -> me)

	| GetArgs ([String] -> me)
	| GetEnvironment ([(String, String)] -> me)
	| PutStrLn String
	| GetContents (String -> me)

	| DoesPathExist FilePath (Bool -> me)
	| WriteBytes FilePath BL.ByteString
	| WriteText FilePath T.Text
	-- | Optional error handler.
	| ReadBytes FilePath (Maybe (String -> me)) (BL.ByteString -> me)
	-- | Optional error handler.
	| ReadText FilePath (Maybe (String -> me)) (T.Text -> me)
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

	fmap  f (GetDirectory getDirectory withDirectory) = GetDirectory (fmap f getDirectory) (f . withDirectory)

	fmap  f (GetArgs withArgs_)              = GetArgs (f . withArgs_)
	fmap  f (GetEnvironment withEnvironment) = GetEnvironment (f . withEnvironment)
	fmap _f (PutStrLn str)                   = PutStrLn str
	fmap  f (GetContents withContents)       = GetContents (f . withContents)

	fmap  f (DoesPathExist path withExists)        = DoesPathExist path (f .  withExists)
	fmap _f (WriteBytes path contents)             = WriteBytes path contents
	fmap _f (WriteText path contents)              = WriteText path contents
	fmap  f (ReadBytes path mwithErr withContents) = ReadBytes path ((f .) <$> mwithErr) (f . withContents)
	fmap  f (ReadText path mwithErr withContents)  = ReadText path ((f .) <$> mwithErr) (f . withContents)
	fmap _f (CreateDirectoryIfMissing path)        = CreateDirectoryIfMissing path
	fmap  f (ForkOS ibio)                          = ForkOS (f ibio)

	fmap  f (SDLIO sdlio) = SDLIO (f <$> sdlio)

{-
instance Foldable BasicIOF where
	-- TODO
instance Traversable BasicIOF where
	-- TODO
-}

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: BasicIO -> BasicIO -> BasicIO
(<>>) = mkThenBasicIO

-- * Runners

runBasicIOIO :: BasicIOF (IO ()) -> IO ()
runBasicIOIO (EmptyBasicIOF)                           = return ()
runBasicIOIO (AndBasicIOF a b)                         = a `par` b `par` concurrently_ a b
runBasicIOIO (ThenBasicIOF a b)                        = a >> b
runBasicIOIO (ExitSuccessBasicIOF)                     = exitSuccess
runBasicIOIO (ExitFailureBasicIOF)                     = exitFailure
runBasicIOIO (GetDirectory getDirectory withDirectory) = runDirectoryIO (runDirectoryAnyIO getDirectory) >>= withDirectory
runBasicIOIO (GetArgs withArgs_)                       = getArgs >>= withArgs_
runBasicIOIO (GetEnvironment withEnvironment)          = getEnvironment >>= withEnvironment
runBasicIOIO (PutStrLn str)                            = putStrLn str
runBasicIOIO (GetContents withContents)                = getContents >>= withContents
runBasicIOIO (DoesPathExist path withExists)           = doesPathExist path >>= withExists
runBasicIOIO (WriteBytes path contents)                = BL.writeFile path contents
runBasicIOIO (WriteText path contents)                 = TIO.writeFile path contents
runBasicIOIO (ReadBytes path mwithErr withContents)    =
	((Just <$> BL.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e)) >>= maybe (return ()) (id . withContents)
runBasicIOIO (ReadText path mwithErr withContents)     =
	((Just <$> TIO.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . withErr . show) mwithErr e)) >>= maybe (return ()) (id . withContents)
runBasicIOIO (CreateDirectoryIfMissing path)           = createDirectoryIfMissing True path
runBasicIOIO (ForkOS ibio)                             = void . forkOS $ ibio
runBasicIOIO (SDLIO sdlio)                             = runSDLIOIO $ sdlio

runDirectoryBasicIO :: DirectoryIO -> (FilePath -> BasicIO) -> BasicIO
runDirectoryBasicIO (Fixed (GetXdgDirectoryData   path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryData   path) withDirectory
runDirectoryBasicIO (Fixed (GetXdgDirectoryConfig path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryConfig path) withDirectory
runDirectoryBasicIO (Fixed (GetXdgDirectoryCache  path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryCache  path) withDirectory
runDirectoryBasicIO (Fixed (GetXdgDirectoryState  path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryState  path) withDirectory

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

mkGetDirectory :: DirectoryIOF BasicIO -> (FilePath -> BasicIO) -> BasicIO
mkGetDirectory getDirectory withDirectory = Fixed $ GetDirectory getDirectory withDirectory

mkGetArgs :: ([String] -> BasicIO) -> BasicIO
mkGetArgs withArgs_ = Fixed $ GetArgs withArgs_

mkGetEnvironment :: ([(String, String)] -> BasicIO) -> BasicIO
mkGetEnvironment withEnvironment = Fixed $ GetEnvironment withEnvironment

mkPutStrLn :: String -> BasicIO
mkPutStrLn str = Fixed $ PutStrLn str

mkGetContents :: (String -> BasicIO) -> BasicIO
mkGetContents withContents = Fixed $ GetContents withContents

mkDoesPathExist :: FilePath -> (Bool -> BasicIO) -> BasicIO
mkDoesPathExist path withExists = Fixed $ DoesPathExist path withExists

mkWriteBytes :: FilePath -> BL.ByteString -> BasicIO
mkWriteBytes path contents = Fixed $ WriteBytes path contents

mkWriteText :: FilePath -> T.Text -> BasicIO
mkWriteText path contents = Fixed $ WriteText path contents

mkReadBytes :: FilePath -> Maybe (String -> BasicIO) -> (BL.ByteString -> BasicIO) -> BasicIO
mkReadBytes path mwithErr withContents = Fixed $ ReadBytes path mwithErr withContents

mkReadText :: FilePath -> Maybe (String -> BasicIO) -> (T.Text -> BasicIO) -> BasicIO
mkReadText path mwithErr withContents = Fixed $ ReadText path mwithErr withContents

mkCreateDirectoryIfMissing :: FilePath -> BasicIO
mkCreateDirectoryIfMissing path = Fixed $ CreateDirectoryIfMissing path

mkForkOS :: BasicIO -> BasicIO
mkForkOS ibio = Fixed $ ForkOS ibio

mkSDLIO :: SDLIOF BasicIO -> BasicIO
mkSDLIO sdlio = Fixed $ SDLIO sdlio
