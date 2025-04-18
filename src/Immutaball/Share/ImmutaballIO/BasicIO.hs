{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO/Basic.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, ExistentialQuantification #-}

module Immutaball.Share.ImmutaballIO.BasicIO
	(
		-- * BasicIO
		BasicIO,
		BasicIOF(..),
		runBasicIO,
		(-<>>),

		-- * mfix
		FixBasicIOException(..),
		fixBasicIOExceptionToException,
		fixBasicIOExceptionFromException,
		PrematureEvaluationFixBasicIOException(..),
		EmptyFixBasicIOException(..),
		fixBasicIOF,
		unsafeFixBasicIOFTo,

		-- * Runners
		runBasicIOIO,
		runDirectoryBasicIO,
		runSDLBasicIO,
		runGLBasicIO,
		hReadBytesSync,
		hReadTextSync,

		-- * BasicIO aliases that apply the Fixed wrapper
		mkEmptyBasicIO,
		mkPureBasicIO,
		mkUnfixBasicIO,
		mkJoinBasicIO,
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
		mkGetDirectoryContents,
		mkGetDirectoryContentsSync,
		mkForkIO,
		mkForkOS,
		mkSDLIO,
		mkGLIO,
		mkDelayUs,
		mkGetUs,

		-- * Utils
		getUsIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent
--import Control.Exception (catch, throwIO)
import Control.Monad
import Control.Monad.Fix
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
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Utils

-- (mfix imports.)
--import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)

-- * BasicIO

type BasicIO = Fixed BasicIOF
data BasicIOF me =
	  EmptyBasicIOF
	| PureBasicIOF me
	| UnfixBasicIOF (BasicIOF me)
	| JoinBasicIOF (BasicIOF (BasicIOF me))
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
	| ReadBytes FilePath (Async (Either IOException BL.ByteString) -> me)
	| ReadBytesSync FilePath (Either IOException BL.ByteString -> me)
	-- | Optional error handler.
	| ReadText FilePath (Async (Either IOException T.Text) -> me)
	| ReadTextSync FilePath (Either IOException T.Text -> me)
	| CreateDirectoryIfMissing FilePath me
	| GetDirectoryContents FilePath (Async [FilePath] -> me)
	| GetDirectoryContentsSync FilePath ([FilePath] -> me)
	| ForkIO me me
	| ForkOS me me

	| SDLIO (SDLIOF me)
	| GLIO (GLIOF me)

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
	fmap  f (PureBasicIOF a)      = PureBasicIOF (f a)
	fmap  f (UnfixBasicIOF bio)   = UnfixBasicIOF (f <$> bio)
	fmap  f (JoinBasicIOF bio)    = JoinBasicIOF (fmap f <$> bio)
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

	fmap  f (DoesPathExist path withExists)             = DoesPathExist path (f .  withExists)
	fmap  f (DoesPathExistSync path withExists)         = DoesPathExistSync path (f .  withExists)
	fmap  f (WriteBytes path contents withUnit)         = WriteBytes path contents (f withUnit)
	fmap  f (WriteText path contents withUnit)          = WriteText path contents (f withUnit)
	fmap  f (ReadBytes path withContents)               = ReadBytes path (f . withContents)
	fmap  f (ReadBytesSync path withContents)           = ReadBytesSync path (f . withContents)
	fmap  f (ReadText path withContents)                = ReadText path (f . withContents)
	fmap  f (ReadTextSync path withContents)            = ReadTextSync path (f . withContents)
	fmap  f (CreateDirectoryIfMissing path withUnit)    = CreateDirectoryIfMissing path (f withUnit)
	fmap  f (GetDirectoryContents path withEntries)     = GetDirectoryContents path (f . withEntries)
	fmap  f (GetDirectoryContentsSync path withEntries) = GetDirectoryContentsSync path (f . withEntries)
	fmap  f (ForkIO bio withUnit)                       = ForkIO (f bio) (f withUnit)
	fmap  f (ForkOS bio withUnit)                       = ForkOS (f bio) (f withUnit)

	fmap  f (SDLIO sdlio) = SDLIO (f <$> sdlio)
	fmap  f (GLIO glio)   = GLIO  (f <$> glio)

	fmap  f (DelayUs us withUnit) = DelayUs us (f withUnit)
	fmap  f (GetUs withUs)        = GetUs (f . withUs)

instance Applicative BasicIOF where
	pure = PureBasicIOF
	mf <*> ma = JoinBasicIOF . flip fmap mf $ \f -> JoinBasicIOF .  flip fmap ma $ \a -> pure (f a)
instance Monad BasicIOF where
	return = pure
	m >>= f = JoinBasicIOF $ f <$> m
instance MonadFix BasicIOF where
	mfix :: (a -> BasicIOF a) -> BasicIOF a
	mfix = fixBasicIOF

{-
instance Foldable BasicIOF where
	-- TODO
instance Traversable BasicIOF where
	-- TODO
-}

-- | Add an ordering constraint.
infixr 6 -<>>
(-<>>) :: BasicIO -> BasicIO -> BasicIO
(-<>>) = mkThenBasicIO

-- * mfix

data FixBasicIOException = forall e. Exception e => FixBasicIOException e
instance Show FixBasicIOException where
	show (FixBasicIOException e) = show e
instance Exception FixBasicIOException
fixBasicIOExceptionToException :: Exception e => e -> SomeException
fixBasicIOExceptionToException = toException . FixBasicIOException
fixBasicIOExceptionFromException :: Exception e => SomeException -> Maybe e
fixBasicIOExceptionFromException x = do
	FixBasicIOException a <- fromException x
	cast a

data PrematureEvaluationFixBasicIOException = PrematureEvaluationFixBasicIOException
	deriving (Show)
instance Exception PrematureEvaluationFixBasicIOException where
	toException = fixBasicIOExceptionToException
	fromException = fixBasicIOExceptionFromException

data EmptyFixBasicIOException = EmptyFixBasicIOException
	deriving (Show)
instance Exception EmptyFixBasicIOException where
	toException = fixBasicIOExceptionToException
	fromException = fixBasicIOExceptionFromException

--    mfix f = mfix f >>= f
-- => mfix f = join $ f <$> mfix f
-- Incorrect: runs f twice.
	--x -> f undefined >>= mfix f
{-
fixBasicIOF :: (me -> BasicIOF me) -> BasicIOF me
fixBasicIOF f = case f (error "Error: fixBasicIOF: premature evaluation of result before we could start it!") of
	x -> joinBasicIOF $ f <$> x
-}
-- Do it like fixIO and fixST (see also their notes; it's a little tricky).
-- Use a lazily read MVar.
fixBasicIOF :: (me -> BasicIOF me) -> BasicIOF me
fixBasicIOF f = unsafePerformIO $ do
	mme <- newEmptyMVar
	return $ unsafeFixBasicIOFTo mme f

-- | Helper for fixBasicIOF.
unsafeFixBasicIOFTo :: MVar me -> (me -> BasicIOF me) -> BasicIOF me
unsafeFixBasicIOFTo mme f = unsafePerformIO $ do
	me_ <- unsafeDupableInterleaveIO (readMVar mme `catch` \BlockedIndefinitelyOnMVar -> throwIO PrematureEvaluationFixBasicIOException)
	case f me_ of
		--_y@(EmptyBasicIOF)       -> throwIO EmptyFixBasicIOException
		_y@(EmptyBasicIOF)       -> return $ EmptyBasicIOF
		y@( PureBasicIOF a)      -> putMVar mme a >> return y
		_y@(UnfixBasicIOF bio)   -> return . UnfixBasicIOF . unsafeFixBasicIOFTo mme $ const bio
		-- Join: Cover all multi-branching (or else we could hang on multiple putMVars), then just fmap for all other cases.
		_y@(JoinBasicIOF (AndBasicIOF a b)) -> return (JoinBasicIOF (AndBasicIOF (unsafeFixBasicIOFTo mme (const a)) (JoinBasicIOF $ f <$> b)))
		_y@(JoinBasicIOF (ThenBasicIOF a b)) -> return (JoinBasicIOF (ThenBasicIOF (unsafeFixBasicIOFTo mme (const a)) (JoinBasicIOF $ f <$> b)))
		_y@(JoinBasicIOF ibio)    -> return $ JoinBasicIOF (unsafeFixBasicIOFTo mme . const <$> ibio)
		_y@(AndBasicIOF  a b)    -> putMVar mme a >> return (JoinBasicIOF $ AndBasicIOF (PureBasicIOF a) (f b))
		_y@(ThenBasicIOF a b)    -> putMVar mme a >> return (JoinBasicIOF $ ThenBasicIOF (PureBasicIOF a) (f b))
		--_y@(ExitSuccessBasicIOF) -> throwIO EmptyFixBasicIOException
		--_y@(ExitFailureBasicIOF) -> throwIO EmptyFixBasicIOException
		_y@(ExitSuccessBasicIOF) -> return $ ExitSuccessBasicIOF
		_y@(ExitFailureBasicIOF) -> return $ ExitFailureBasicIOF

		_y@(DirectoryIO dio) -> return . DirectoryIO . unsafeFixDirectoryIOFTo mme $ const dio

		_y@(GetArgs            withArgs_)       -> return $ GetArgs            ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withArgs_)
		_y@(GetArgsSync        withArgs_)       -> return $ GetArgsSync        ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withArgs_)
		_y@(GetEnvironment     withEnvironment) -> return $ GetEnvironment     ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withEnvironment)
		_y@(GetEnvironmentSync withEnvironment) -> return $ GetEnvironmentSync ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withEnvironment)
		y@( PutStrLn           _str me)         -> putMVar mme me >> return y
		_y@(GetContents        withContents)    -> return $ GetContents        ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withContents)
		_y@(GetContentsSync    withContents)    -> return $ GetContentsSync    ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withContents)

		_y@(DoesPathExist     path withExists)            -> return $ DoesPathExist     path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withExists)
		_y@(DoesPathExistSync path withExists)            -> return $ DoesPathExistSync path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withExists)
		y@( WriteBytes        _path _contents me)         -> putMVar mme me >> return y
		y@( WriteText         _path _contents me)         -> putMVar mme me >> return y
		_y@(ReadBytes         path withContents)          -> return $ ReadBytes         path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withContents)
		_y@(ReadBytesSync     path withContents)          -> return $ ReadBytesSync     path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withContents)
		_y@(ReadText          path withContents)          -> return $ ReadText          path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withContents)
		_y@(ReadTextSync      path withContents)          -> return $ ReadTextSync      path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withContents)
		y@( CreateDirectoryIfMissing _path me)            -> putMVar mme me >> return y
		_y@(GetDirectoryContents path withEntries)        -> return $ GetDirectoryContents path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withEntries)
		_y@(GetDirectoryContentsSync path withEntries)    -> return $ GetDirectoryContentsSync path ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withEntries)
		--y@( ForkIO            _bio me)                    -> putMVar mme me >> return y
		_y@(ForkIO            bio me)                     -> putMVar mme me >> return (JoinBasicIOF $ ForkIO (f bio) (PureBasicIOF me))
		--y@( ForkOS            _os me)                     -> putMVar mme me >> return y
		_y@(ForkOS            os me)                      -> putMVar mme me >> return (JoinBasicIOF $ ForkOS (f os)  (PureBasicIOF me))

		_y@(SDLIO sdlio) -> return . SDLIO . unsafeFixSDLIOFTo mme $ const sdlio
		_y@(GLIO  glio)  -> return . GLIO  . unsafeFixGLIOFTo  mme $ const glio

		y@( DelayUs _us me) -> putMVar mme me >> return y
		_y@(GetUs   withUs) -> return $ GetUs ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withUs)

-- * Runners

runBasicIOIO :: BasicIOF (IO ()) -> IO ()
runBasicIOIO (EmptyBasicIOF)                          = return ()
runBasicIOIO (PureBasicIOF bio)                       = bio
runBasicIOIO (UnfixBasicIOF bio)                      = runBasicIOIO bio
runBasicIOIO (JoinBasicIOF bio)                       = runBasicIOIO $ runBasicIOIO <$> bio
runBasicIOIO (AndBasicIOF a b)                        = a `par` b `par` concurrently_ a b
runBasicIOIO (ThenBasicIOF a b)                       = a >> b
runBasicIOIO (ExitSuccessBasicIOF)                    = exitSuccess
runBasicIOIO (ExitFailureBasicIOF)                    = exitFailure
runBasicIOIO (DirectoryIO dio)                        = runDirectoryIOIO $ dio
runBasicIOIO (GetArgs withArgs_)                      = withAsync getArgs withArgs_
runBasicIOIO (GetArgsSync withArgs_)                  = getArgs >>= withArgs_
runBasicIOIO (GetEnvironment withEnvironment)         = withAsync getEnvironment withEnvironment
runBasicIOIO (GetEnvironmentSync withEnvironment)     = getEnvironment >>= withEnvironment
runBasicIOIO (PutStrLn str withUnit)                  = putStrLn str >> withUnit
runBasicIOIO (GetContents withContents)               = withAsync getContents withContents
runBasicIOIO (GetContentsSync withContents)           = getContents >>= withContents
runBasicIOIO (DoesPathExist path withExists)          = withAsync (doesPathExist path) withExists
runBasicIOIO (DoesPathExistSync path withExists)      = doesPathExist path >>= withExists
runBasicIOIO (WriteBytes path contents withUnit)      = BL.writeFile path contents >> withUnit
runBasicIOIO (WriteText path contents withUnit)       = TIO.writeFile path contents >> withUnit
runBasicIOIO (ReadBytes path withContents)            = withAsync (hReadBytesSync path) withContents
runBasicIOIO (ReadBytesSync path withContents)        = hReadBytesSync path >>= withContents
runBasicIOIO (ReadText path withContents)             = withAsync (hReadTextSync path) withContents
runBasicIOIO (ReadTextSync path withContents)         = hReadTextSync path >>= withContents
runBasicIOIO (CreateDirectoryIfMissing path withUnit) = createDirectoryIfMissing True path >> withUnit
runBasicIOIO (GetDirectoryContents path withEntries)  = withAsync (getDirectoryContents path) withEntries
runBasicIOIO (GetDirectoryContentsSync path withEntries) = getDirectoryContents path >>= withEntries
runBasicIOIO (ForkIO bio withUnit)                    = (void . forkIO) bio >> withUnit
runBasicIOIO (ForkOS bio withUnit)                    = (void . forkOS) bio >> withUnit
runBasicIOIO (SDLIO sdlio)                            = runSDLIOIO $ sdlio
runBasicIOIO (GLIO  glio)                             = runGLIOIO  $ glio
runBasicIOIO (DelayUs us withUnit)                    = delay us >> withUnit
runBasicIOIO (GetUs withUs)                           = getUsIO >>= withUs

runDirectoryBasicIO :: DirectoryIO -> BasicIO
runDirectoryBasicIO dio = Fixed . DirectoryIO $ runDirectoryBasicIO <$> getFixed dio

runSDLBasicIO :: SDLIO -> BasicIO
runSDLBasicIO sdlio = Fixed . SDLIO $ runSDLBasicIO <$> getFixed sdlio

runGLBasicIO :: GLIO -> BasicIO
runGLBasicIO glio = Fixed . GLIO $ runGLBasicIO <$> getFixed glio

hReadBytesSync :: FilePath -> IO (Either IOException BL.ByteString)
hReadBytesSync path = try $ BL.readFile path

hReadTextSync :: FilePath -> IO (Either IOException T.Text)
hReadTextSync path = try $ TIO.readFile path

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyBasicIO :: BasicIO
mkEmptyBasicIO = Fixed $ EmptyBasicIOF

mkPureBasicIO :: BasicIO -> BasicIO
mkPureBasicIO bio = Fixed $ PureBasicIOF bio

mkUnfixBasicIO :: BasicIO -> BasicIO
mkUnfixBasicIO bio = Fixed $ UnfixBasicIOF (getFixed bio)

mkJoinBasicIO :: BasicIO -> BasicIO
mkJoinBasicIO bio = Fixed $ JoinBasicIOF (getFixed <$> getFixed bio)

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

mkReadBytes :: FilePath -> (Async (Either IOException BL.ByteString) -> BasicIO) -> BasicIO
mkReadBytes path withContents = Fixed $ ReadBytes path withContents

mkReadBytesSync :: FilePath -> (Either IOException BL.ByteString -> BasicIO) -> BasicIO
mkReadBytesSync path withContents = Fixed $ ReadBytesSync path withContents

mkReadText :: FilePath -> (Async (Either IOException T.Text) -> BasicIO) -> BasicIO
mkReadText path withContents = Fixed $ ReadText path withContents

mkReadTextSync :: FilePath -> (Either IOException T.Text -> BasicIO) -> BasicIO
mkReadTextSync path withContents = Fixed $ ReadTextSync path withContents

mkCreateDirectoryIfMissing :: FilePath -> BasicIO -> BasicIO
mkCreateDirectoryIfMissing path withUnit = Fixed $ CreateDirectoryIfMissing path withUnit

mkGetDirectoryContents :: FilePath -> (Async [FilePath] -> BasicIO) -> BasicIO
mkGetDirectoryContents path withEntries = Fixed $ GetDirectoryContents path withEntries

mkGetDirectoryContentsSync :: FilePath -> ([FilePath] -> BasicIO) -> BasicIO
mkGetDirectoryContentsSync path withEntries = Fixed $ GetDirectoryContentsSync path withEntries

mkForkIO :: BasicIO -> BasicIO -> BasicIO
mkForkIO bio withUnit = Fixed $ ForkIO bio withUnit

mkForkOS :: BasicIO -> BasicIO -> BasicIO
mkForkOS bio withUnit = Fixed $ ForkOS bio withUnit

mkSDLIO :: SDLIOF BasicIO -> BasicIO
mkSDLIO sdlio = Fixed $ SDLIO sdlio

mkGLIO :: GLIOF BasicIO -> BasicIO
mkGLIO glio = Fixed $ GLIO glio

-- | Microseconds thread delay, with ‘unbounded-delays’.
mkDelayUs :: Integer -> BasicIO -> BasicIO
mkDelayUs us withUnit = Fixed $ DelayUs us withUnit

mkGetUs :: (Integer -> BasicIO) -> BasicIO
mkGetUs withUs = Fixed $ GetUs withUs

-- * Utils

getUsIO :: IO Integer
getUsIO = getSystemTime >>= \(MkSystemTime secs nanosecs) -> return $ (1000000 * (fromIntegral secs)) + ((fromIntegral nanosecs) `div` 1000)
