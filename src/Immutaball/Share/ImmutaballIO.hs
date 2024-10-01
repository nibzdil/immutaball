{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}

module Immutaball.Share.ImmutaballIO
	(
		-- * ImmutaballIO
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO,
		(<>>),

		-- * Runners
		runDirectoryImmutaballIO,
		runSDLImmutaballIO,

		-- * ImutaballIO aliases that apply the Fixed wrapper
		mkEmptyImmutaballIO,
		mkAndImmutaballIO,
		mkThenImmutaballIO,
		mkExitSuccessImmutaballIO,
		mkExitFailureImmutaballIO,
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
		mkSDLIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Exception (catch, throwIO)
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

-- * ImmutaballIO

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF a =
	  EmptyImmutaballIOF
	| AndImmutaballIOF a a
	| ThenImmutaballIOF a a
	| ExitSuccessImmutaballIOF
	| ExitFailureImmutaballIOF

	| GetDirectory (DirectoryIOF a) (FilePath -> a)

	| GetArgs ([String] -> a)
	| GetEnvironment ([(String, String)] -> a)
	| PutStrLn String
	| GetContents (String -> a)

	| DoesPathExist FilePath (Bool -> a)
	| WriteBytes FilePath BL.ByteString
	| WriteText FilePath T.Text
	-- | Optional error handler.
	| ReadBytes FilePath (Maybe (String -> a)) (BL.ByteString -> a)
	-- | Optional error handler.
	| ReadText FilePath (Maybe (String -> a)) (T.Text -> a)
	| CreateDirectoryIfMissing FilePath

	| SDLIO (SDLIOF a)

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO (Fixed (EmptyImmutaballIOF))       = return ()
runImmutaballIO (Fixed (AndImmutaballIOF a b))     = a `par` b `par` concurrently_ (runImmutaballIO a) (runImmutaballIO b)
runImmutaballIO (Fixed (ThenImmutaballIOF a b))    = runImmutaballIO a >> runImmutaballIO b
runImmutaballIO (Fixed (ExitSuccessImmutaballIOF)) = exitSuccess
runImmutaballIO (Fixed (ExitFailureImmutaballIOF)) = exitFailure
runImmutaballIO (Fixed (GetDirectory getDirectory withDirectory)) = runDirectoryIO (runDirectoryAnyIO getDirectory) >>= runImmutaballIO . withDirectory
runImmutaballIO (Fixed (GetArgs withArgs_))        = getArgs >>= runImmutaballIO . withArgs_
runImmutaballIO (Fixed (GetEnvironment withEnvironment)) = getEnvironment >>= runImmutaballIO . withEnvironment
runImmutaballIO (Fixed (PutStrLn str))             = putStrLn str
runImmutaballIO (Fixed (GetContents withContents)) = getContents >>= runImmutaballIO . withContents
runImmutaballIO (Fixed (DoesPathExist path withExists)) = doesPathExist path >>= runImmutaballIO . withExists
runImmutaballIO (Fixed (WriteBytes path contents)) = BL.writeFile path contents
runImmutaballIO (Fixed (WriteText path contents))  = TIO.writeFile path contents
runImmutaballIO (Fixed (ReadBytes path mwithErr withContents)) =
	((Just <$> BL.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . runImmutaballIO . withErr . show) mwithErr e)) >>= maybe (return ()) (id . runImmutaballIO . withContents)
runImmutaballIO (Fixed (ReadText path mwithErr withContents)) =
	((Just <$> TIO.readFile path) `catch` (\e -> flip const (e :: IOError) $ maybe throwIO (\withErr -> (const Nothing <$>) . runImmutaballIO . withErr . show) mwithErr e)) >>= maybe (return ()) (id . runImmutaballIO . withContents)
runImmutaballIO (Fixed (CreateDirectoryIfMissing path)) = createDirectoryIfMissing True path
runImmutaballIO (Fixed (SDLIO sdlio))              = runSDLIOIO $ runImmutaballIO <$> sdlio

instance Semigroup (ImmutaballIOF ImmutaballIO) where
	a <> b = Fixed a `AndImmutaballIOF` Fixed b
instance Monoid (ImmutaballIOF ImmutaballIO) where
	mempty = EmptyImmutaballIOF

instance Semigroup ImmutaballIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid ImmutaballIO where
	mempty = Fixed mempty

instance Functor (ImmutaballIOF) where
	fmap :: (a -> b) -> (ImmutaballIOF a -> ImmutaballIOF b)
	fmap _f (EmptyImmutaballIOF)       = EmptyImmutaballIOF
	fmap  f (AndImmutaballIOF a b)     = AndImmutaballIOF (f a) (f b)
	fmap  f (ThenImmutaballIOF a b)    = ThenImmutaballIOF (f a) (f b)
	fmap _f (ExitFailureImmutaballIOF) = ExitFailureImmutaballIOF
	fmap _f (ExitSuccessImmutaballIOF) = ExitSuccessImmutaballIOF

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

	fmap  f (SDLIO sdlio) = SDLIO (f <$> sdlio)

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
(<>>) = mkThenImmutaballIO

-- * Runners

runDirectoryImmutaballIO :: DirectoryIO -> (FilePath -> ImmutaballIO) -> ImmutaballIO
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryData   path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryData   path) withDirectory
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryConfig path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryConfig path) withDirectory
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryCache  path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryCache  path) withDirectory
runDirectoryImmutaballIO (Fixed (GetXdgDirectoryState  path)) withDirectory = Fixed $ GetDirectory (GetXdgDirectoryState  path) withDirectory

runSDLImmutaballIO :: SDLIO -> ImmutaballIO
runSDLImmutaballIO sdlio = Fixed . SDLIO $ runSDLImmutaballIO <$> getFixed sdlio
--runSDLImmutaballIO (Fixed (SDLInit subsystems sdlio)) = Fixed . SDLIO $ SDLInit subsystems (runSDLImmutaballIO sdlio)

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyImmutaballIO :: ImmutaballIO
mkEmptyImmutaballIO = Fixed $ EmptyImmutaballIOF

mkAndImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndImmutaballIO a b = Fixed $ AndImmutaballIOF a b

mkThenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkThenImmutaballIO a b = Fixed $ ThenImmutaballIOF a b

mkExitSuccessImmutaballIO :: ImmutaballIO
mkExitSuccessImmutaballIO = Fixed $ ExitFailureImmutaballIOF

mkExitFailureImmutaballIO :: ImmutaballIO
mkExitFailureImmutaballIO = Fixed $ ExitFailureImmutaballIOF

mkGetDirectory :: DirectoryIOF ImmutaballIO -> (FilePath -> ImmutaballIO) -> ImmutaballIO
mkGetDirectory getDirectory withDirectory = Fixed $ GetDirectory getDirectory withDirectory

mkGetArgs :: ([String] -> ImmutaballIO) -> ImmutaballIO
mkGetArgs withArgs_ = Fixed $ GetArgs withArgs_

mkGetEnvironment :: ([(String, String)] -> ImmutaballIO) -> ImmutaballIO
mkGetEnvironment withEnvironment = Fixed $ GetEnvironment withEnvironment

mkPutStrLn :: String -> ImmutaballIO
mkPutStrLn str = Fixed $ PutStrLn str

mkGetContents :: (String -> ImmutaballIO) -> ImmutaballIO
mkGetContents withContents = Fixed $ GetContents withContents

mkDoesPathExist :: FilePath -> (Bool -> ImmutaballIO) -> ImmutaballIO
mkDoesPathExist path withExists = Fixed $ DoesPathExist path withExists

mkWriteBytes :: FilePath -> BL.ByteString -> ImmutaballIO
mkWriteBytes path contents = Fixed $ WriteBytes path contents

mkWriteText :: FilePath -> T.Text -> ImmutaballIO
mkWriteText path contents = Fixed $ WriteText path contents

mkReadBytes :: FilePath -> Maybe (String -> ImmutaballIO) -> (BL.ByteString -> ImmutaballIO) -> ImmutaballIO
mkReadBytes path mwithErr withContents = Fixed $ ReadBytes path mwithErr withContents

mkReadText :: FilePath -> Maybe (String -> ImmutaballIO) -> (T.Text -> ImmutaballIO) -> ImmutaballIO
mkReadText path mwithErr withContents = Fixed $ ReadText path mwithErr withContents

mkCreateDirectoryIfMissing :: FilePath -> ImmutaballIO
mkCreateDirectoryIfMissing path = Fixed $ CreateDirectoryIfMissing path

mkSDLIO :: SDLIOF ImmutaballIO -> ImmutaballIO
mkSDLIO sdlio = Fixed $ SDLIO sdlio
