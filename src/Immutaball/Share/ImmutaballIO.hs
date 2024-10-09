{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, ExistentialQuantification #-}

-- TODO: also add monad and like instances for BasicIO and SDLIO with a Join
-- like we do here.
-- (Simple IOs like directoryIO probably don't need it.)

module Immutaball.Share.ImmutaballIO
	(
		-- * ImmutaballIO
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO,
		andImmutaballIO,
		thenImmutaballIO,
		(<>>),
		joinImmutaballIOF,

		-- * mfix
		FixImmutaballIOException(..),
		fixImmutaballIOExceptionToException,
		fixImmutaballIOExceptionFromException,
		PrematureEvaluationFixImmutaballIOException(..),
		EmptyFixImmutaballIOException(..),
		fixImmutaballIOF,
		unsafeFixImmutaballIOFTo,

		-- * Runners
		runImmutaballIOIO,
		runBasicImmutaballIO,
		runDirectoryImmutaballIO,
		runSDLImmutaballIO,

		-- * ImmutaballIO aliases that apply the Fixed wrapper
		mkEmptyImmutaballIO,
		mkPureImmutaballIO,
		mkUnfixImmutaballIO,
		mkJoinImmutaballIO,
		mkAndImmutaballIO,
		mkThenImmutaballIO,
		mkBasicImmutaballIO,
		mkWait,
		mkWithAsync,
		mkAtomically,

		-- * Utils
		mkBIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.Async
import Control.Monad.Fix
import Control.Monad.STM
import Control.Parallel

import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Utils

-- (mfix imports.)
import Control.Concurrent.MVar
import Control.Exception
import Data.Typeable
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import System.IO.Unsafe (unsafePerformIO)

-- * ImmutaballIO

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF me =
	  EmptyIBIOF
	| PureIBIOF me
	| UnfixIBIOF (ImmutaballIOF me)
	| JoinIBIOF (ImmutaballIOF (ImmutaballIOF me))
	| AndIBIOF me me
	| ThenIBIOF me me
	| BasicIBIOF (BasicIOF me)

	| forall hiddenTypeField. Wait (Async hiddenTypeField) (hiddenTypeField -> me)
	| WithAsync me (Async () -> me)
	| forall hiddenTypeField. Atomically (STM hiddenTypeField) (hiddenTypeField -> me)

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO bio = cata runImmutaballIOIO bio

andImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO 
andImmutaballIO x y = Fixed $ AndIBIOF x y

thenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
thenImmutaballIO x y = Fixed $ ThenIBIOF x y

instance Semigroup (ImmutaballIOF ImmutaballIO) where
	(<>) :: ImmutaballIOF ImmutaballIO -> ImmutaballIOF ImmutaballIO -> ImmutaballIOF ImmutaballIO
	a <> b = AndIBIOF (Fixed a) (Fixed b)
instance (Semigroup (ImmutaballIOF me)) => Monoid (ImmutaballIOF me) where
	mempty = EmptyIBIOF

instance Semigroup ImmutaballIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid ImmutaballIO where
	mempty = Fixed EmptyIBIOF

instance Functor ImmutaballIOF where
	fmap :: (a -> b) -> (ImmutaballIOF a -> ImmutaballIOF b)
	fmap _f   (EmptyIBIOF)      = EmptyIBIOF
	fmap  f   (PureIBIOF a)     = PureIBIOF (f a)
	fmap  f   (UnfixIBIOF ibio) = UnfixIBIOF (f <$> ibio)
	fmap  f   (JoinIBIOF ibio)  = JoinIBIOF (fmap f <$> ibio)
	fmap  f   (AndIBIOF a b)    = AndIBIOF (f a) (f b)
	fmap  f   (ThenIBIOF a b)   = ThenIBIOF (f a) (f b)
	fmap  f   (BasicIBIOF bio)  = BasicIBIOF $ f <$> bio

	fmap  f   (Wait async_ withAsync_)    = Wait async_ (f . withAsync_)
	fmap  f   (WithAsync ibio withAsync_) = WithAsync (f ibio) (f . withAsync_)
	fmap  f   (Atomically stm withStm)    = Atomically stm (f . withStm)

joinImmutaballIOF :: ImmutaballIOF (ImmutaballIOF a) -> ImmutaballIOF a
joinImmutaballIOF = JoinIBIOF

-- * mfix

data FixImmutaballIOException = forall e. Exception e => FixImmutaballIOException e
instance Show FixImmutaballIOException where
	show (FixImmutaballIOException e) = show e
instance Exception FixImmutaballIOException
fixImmutaballIOExceptionToException :: Exception e => e -> SomeException
fixImmutaballIOExceptionToException = toException . FixImmutaballIOException
fixImmutaballIOExceptionFromException :: Exception e => SomeException -> Maybe e
fixImmutaballIOExceptionFromException x = do
	FixImmutaballIOException a <- fromException x
	cast a

data PrematureEvaluationFixImmutaballIOException = PrematureEvaluationFixImmutaballIOException
	deriving (Show)
instance Exception PrematureEvaluationFixImmutaballIOException where
	toException = fixImmutaballIOExceptionToException
	fromException = fixImmutaballIOExceptionFromException

data EmptyFixImmutaballIOException = EmptyFixImmutaballIOException
	deriving (Show)
instance Exception EmptyFixImmutaballIOException where
	toException = fixImmutaballIOExceptionToException
	fromException = fixImmutaballIOExceptionFromException

--    mfix f = mfix f >>= f
-- => mfix f = join $ f <$> mfix f
-- Incorrect: runs f twice.
	--x -> f undefined >>= mfix f
{-
fixImmutaballIOF :: (me -> ImmutaballIOF me) -> ImmutaballIOF me
fixImmutaballIOF f = case f (error "Error: fixImmutaballIOF: premature evaluation of result before we could start it!") of
	x -> joinImmutaballIOF $ f <$> x
-}
-- Do it like fixIO and fixST (see also their notes; it's a little tricky).
-- Use a lazily read MVar.
fixImmutaballIOF :: (me -> ImmutaballIOF me) -> ImmutaballIOF me
fixImmutaballIOF f = unsafePerformIO $ do
	mme <- newEmptyMVar
	return $ unsafeFixImmutaballIOFTo mme f

-- | Helper for fixImmutaballIOF.
unsafeFixImmutaballIOFTo :: MVar me -> (me -> ImmutaballIOF me) -> ImmutaballIOF me
unsafeFixImmutaballIOFTo mme f = unsafePerformIO $ do
	me_ <- unsafeDupableInterleaveIO (readMVar mme `catch` \BlockedIndefinitelyOnMVar -> throwIO PrematureEvaluationFixImmutaballIOException)
	case f me_ of
		_y@(EmptyIBIOF)        -> throwIO EmptyFixImmutaballIOException
		y@(PureIBIOF a)        -> putMVar mme a >> return y
		_y@(UnfixIBIOF ibio)   -> return . UnfixIBIOF . unsafeFixImmutaballIOFTo mme $ const ibio
		-- Join: Cover all multi-branching (or else we could hang on multiple putMVars), then just fmap for all other cases.
		_y@(JoinIBIOF (AndIBIOF a b)) -> return (JoinIBIOF (AndIBIOF (unsafeFixImmutaballIOFTo mme (const a)) (JoinIBIOF $ f <$> b)))
		_y@(JoinIBIOF (ThenIBIOF a b)) -> return (JoinIBIOF (ThenIBIOF (unsafeFixImmutaballIOFTo mme (const a)) (JoinIBIOF $ f <$> b)))
		_y@(JoinIBIOF ibio)    -> return $ JoinIBIOF (unsafeFixImmutaballIOFTo mme . const <$> ibio)
		_y@(AndIBIOF  a b)     -> putMVar mme a >> return (JoinIBIOF $ AndIBIOF  (PureIBIOF a) (f b))
		_y@(ThenIBIOF a b)     -> putMVar mme a >> return (JoinIBIOF $ ThenIBIOF (PureIBIOF a) (f b))
		_y@(BasicIBIOF bio)    -> return . BasicIBIOF . unsafeFixBasicIOFTo mme $ const bio
		_y@(Wait async_ withAsync_)    -> return $ Wait       async_ ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withAsync_)
		_y@(WithAsync ibio withAsync_) -> return $ WithAsync  ibio   ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withAsync_)
		_y@(Atomically stm withStm)    -> return $ Atomically stm    ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withStm)

instance Applicative ImmutaballIOF where
	pure = PureIBIOF
	mf <*> ma = joinImmutaballIOF . flip fmap mf $ \f -> joinImmutaballIOF .  flip fmap ma $ \a -> pure (f a)
instance Monad ImmutaballIOF where
	return = pure
	m >>= f = joinImmutaballIOF $ f <$> m
instance MonadFix ImmutaballIOF where
	mfix :: (a -> ImmutaballIOF a) -> ImmutaballIOF a
	mfix = fixImmutaballIOF

{-
instance Foldable ImmutaballIOF where
	foldr :: (a -> b -> b) -> b -> ImmutaballIOF a -> b
	foldr _reduce reduction0 (EmptyIBIOF)  = reduction0
	foldr  reduce reduction0 (PureIBIOF a) = reduce a reduction0
	foldr  reduce reduction0 (JoinIBIOF ibio) = ??? $ foldr reduce reduction0 <$> ibio
-}

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
(<>>) = thenImmutaballIO

-- * Runners

runImmutaballIOIO :: ImmutaballIOF (IO ()) -> IO ()

runImmutaballIOIO (EmptyIBIOF)      = return ()
runImmutaballIOIO (PureIBIOF a)     = a
runImmutaballIOIO (UnfixIBIOF ibio) = runImmutaballIOIO ibio
runImmutaballIOIO (JoinIBIOF ibio)  = runImmutaballIOIO $ runImmutaballIOIO <$> ibio
runImmutaballIOIO (AndIBIOF a b)    = a `par` b `par` concurrently_ a b
runImmutaballIOIO (ThenIBIOF a b)   = a >> b
runImmutaballIOIO (BasicIBIOF bio)  = runBasicIOIO bio

runImmutaballIOIO (Wait async_ withAsync_)    = wait async_ >>= withAsync_
runImmutaballIOIO (WithAsync ibio withAsync_) = withAsync ibio withAsync_
runImmutaballIOIO (Atomically stm withStm)    = atomically stm >>= withStm

runBasicImmutaballIO :: BasicIO -> ImmutaballIO
runBasicImmutaballIO bio = Fixed $ BasicIBIOF (runBasicImmutaballIO <$> getFixed bio)

runDirectoryImmutaballIO :: DirectoryIO -> ImmutaballIO
runDirectoryImmutaballIO dio = runBasicImmutaballIO . runDirectoryBasicIO $ dio

runSDLImmutaballIO :: SDLIO -> ImmutaballIO
runSDLImmutaballIO sdlio = runBasicImmutaballIO . runSDLBasicIO $ sdlio

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyImmutaballIO :: ImmutaballIO
mkEmptyImmutaballIO = Fixed $ EmptyIBIOF

mkPureImmutaballIO :: ImmutaballIO -> ImmutaballIO
mkPureImmutaballIO ibio = Fixed $ PureIBIOF ibio

mkUnfixImmutaballIO :: ImmutaballIO -> ImmutaballIO
mkUnfixImmutaballIO ibio = Fixed $ UnfixIBIOF (getFixed ibio)

mkJoinImmutaballIO :: ImmutaballIO -> ImmutaballIO
mkJoinImmutaballIO ibio = Fixed $ JoinIBIOF (getFixed <$> getFixed ibio)

mkAndImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndImmutaballIO a b = Fixed $ AndIBIOF a b

mkThenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkThenImmutaballIO a b = Fixed $ ThenIBIOF a b

mkBasicImmutaballIO :: BasicIOF ImmutaballIO -> ImmutaballIO
mkBasicImmutaballIO bio = Fixed $ BasicIBIOF bio

mkWait :: Async a -> (a -> ImmutaballIO) -> ImmutaballIO
mkWait async_ withAsync_ = Fixed $ Wait async_ withAsync_

mkWithAsync :: ImmutaballIO -> (Async () -> ImmutaballIO) -> ImmutaballIO
mkWithAsync ibio withAsync_ = Fixed $ WithAsync ibio withAsync_

mkAtomically :: STM a -> (a -> ImmutaballIO) -> ImmutaballIO
mkAtomically stm withStm = Fixed $ Atomically stm withStm

-- * Utils

-- | Short alias for 'mkBasicImmutaballIO'.
mkBIO :: BasicIOF ImmutaballIO -> ImmutaballIO
mkBIO = mkBasicImmutaballIO
