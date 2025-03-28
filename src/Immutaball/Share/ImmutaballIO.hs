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
		(<>>-),
		(<>-),
		joinImmutaballIOF,
		forkIBIOF,
		forkBoundIBIOF,
		forkBothBoundIBIOF,

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
		hArrayToBS,

		-- * ImmutaballIO aliases that apply the Fixed wrapper
		mkEmptyIBIO,
		mkPureIBIO,
		mkUnfixIBIO,
		mkJoinIBIO,
		mkAndIBIO,
		mkThenIBIO,
		mkBasicIBIO,
		mkWait,
		mkWithAsync,
		mkAtomically,
		mkThrowIO,
		mkArrayToBS,
		mkThawIO,
		mkFreezeIO,
		mkPoke,
		mkPeek,

		-- * Utils
		mkBIO
	) where

import Prelude ()
import Immutaball.Prelude

import GHC.Stack        (HasCallStack)      -- For ThrowIO.
--import Foreign.Ptr      (castPtr)           -- (For ArrayToBS.)
--import Foreign.Storable (sizeOf, Storable)  -- (For ArrayToBS.)

import Control.Concurrent.Async
import Control.Monad.Fix
import Control.Monad.STM
import Control.Parallel
import Data.Array.Base
import Data.Array.Storable
import qualified Data.ByteString as BS
import Foreign.Ptr
import Foreign.Storable

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

	| forall e. (HasCallStack, Exception e) => ThrowIO e

	| forall i e. (Integral i, Ix i, Storable e) => ArrayToBS (StorableArray i e) (BS.ByteString -> me)

	| forall a b i e. (Ix i, IArray a e,    MArray b e IO) => ThawIO   (a i e) (b i e -> me)
	| forall a b i e. (Ix i, MArray a e IO, IArray b e)    => FreezeIO (a i e) (b i e -> me)

	| forall a. (Storable a) => Poke (Ptr a) a me
	| forall a. (Storable a) => Peek (Ptr a) (a -> me)

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
	fmap _f (EmptyIBIOF)      = EmptyIBIOF
	fmap  f (PureIBIOF a)     = PureIBIOF (f a)
	fmap  f (UnfixIBIOF ibio) = UnfixIBIOF (f <$> ibio)
	fmap  f (JoinIBIOF ibio)  = JoinIBIOF (fmap f <$> ibio)
	fmap  f (AndIBIOF a b)    = AndIBIOF (f a) (f b)
	fmap  f (ThenIBIOF a b)   = ThenIBIOF (f a) (f b)
	fmap  f (BasicIBIOF bio)  = BasicIBIOF $ f <$> bio

	fmap  f (Wait async_ withAsync_)    = Wait async_ (f . withAsync_)
	fmap  f (WithAsync ibio withAsync_) = WithAsync (f ibio) (f . withAsync_)
	fmap  f (Atomically stm withStm)    = Atomically stm (f . withStm)

	fmap _f (ThrowIO e)       = ThrowIO e

	fmap  f (ArrayToBS array_ withBS) = ArrayToBS array_ (f . withBS)

	fmap  f (ThawIO   iarray withMArray) = ThawIO   iarray (f . withMArray)
	fmap  f (FreezeIO marray withIArray) = FreezeIO marray (f . withIArray)

	fmap  f (Poke ptr val withUnit) = Poke ptr val (f withUnit)
	fmap  f (Peek ptr     withVal)  = Peek ptr     (f . withVal)

joinImmutaballIOF :: ImmutaballIOF (ImmutaballIOF a) -> ImmutaballIOF a
joinImmutaballIOF = JoinIBIOF

-- | Fork the first IBIOF, with the result associated with the second IBIOF.
--
-- This is represented as a flipped And, since And is normally understood to
-- have its first _ordinary_ ‘me’ (e.g. not a bound thread like ForkOS) to be
-- the result.  This mostly pertains to mfix: e.g. fix with And takes the first
-- argument as the result, but still expands the tree to traverse through the
-- second too, preserving ‘let ma = ma >>= f’ in a computable manner.  mfix
-- with forkOS takes the second argument as result first, but still traverses
-- the tree all the same.
--
-- Note: due to the underlying 'concurrently_' runner, both branches are run
-- under an unbound thread.
--
-- Use 'forkBoundIBIOF' or 'ForkIO' if instead you wish to keep the primary
-- thread in the second argument bound, but fork the first unbound.  Use
-- 'forkBothboundIBIOF' or 'ForkOS' if you wish to keep both bound.  Ensuring
-- some commands are run in a same bound thread may be necessary e.g. for
-- OpenGL.
forkIBIOF :: ImmutaballIOF me -> ImmutaballIOF me -> ImmutaballIOF me
forkIBIOF fork withUnit = JoinIBIOF $ flip AndIBIOF fork withUnit

-- | Keep the second, main thread in a bound thread if it is running in a bound thread.
--
-- Note the underlying runner does not use 'concurrently_' to manage automatic
-- thread clean-up.  Thus normally 'forkIBIOF' is preferable unless you want to
-- keep the second argument running in a bound thread, e.g. for OpenGL rendering.
--
-- (See 'Control.Concurrent' documentation for what a bound versus unbound
-- thread means.)
forkBoundIBIOF :: ImmutaballIOF me -> ImmutaballIOF me -> ImmutaballIOF me
forkBoundIBIOF fork withUnit = JoinIBIOF . BasicIBIOF $ ForkIO fork withUnit

-- | Fork into a bound thread with 'forkOS'.
forkBothBoundIBIOF :: ImmutaballIOF me -> ImmutaballIOF me -> ImmutaballIOF me
forkBothBoundIBIOF fork withUnit = JoinIBIOF . BasicIBIOF $ ForkOS fork withUnit

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

-- | Premature evaluation or attempt to access an empty value (e.g. mfix with EmptyIBIOF).
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
		--_y@(EmptyIBIOF)        -> throwIO EmptyFixImmutaballIOException
		_y@(EmptyIBIOF)        -> return $ EmptyIBIOF
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

		--y@(ThrowIO _e me) -> putMVar mme me >> return y
		_y@(ThrowIO e) -> return $ ThrowIO e

		_y@(ArrayToBS array_ withBS) -> return $ ArrayToBS array_ ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withBS)

		_y@(ThawIO   iarray withMArray) -> return $ ThawIO   iarray ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withMArray)
		_y@(FreezeIO marray withIArray) -> return $ FreezeIO marray ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withIArray)

		y@( Poke _ptr _val me)      -> putMVar mme me >> return y
		_y@(Peek ptr       withVal) -> return $ Peek ptr ((\me -> unsafePerformIO $ putMVar mme me >> return me) . withVal)

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

-- | A more general version.
infixr 6 <>>-
(<>>-) :: ImmutaballIOF me -> ImmutaballIOF me -> ImmutaballIOF me
a <>>- b = JoinIBIOF $ a `ThenIBIOF` b

-- | Without an ordering constraint.
infixr 6 <>-
(<>-) :: ImmutaballIOF me -> ImmutaballIOF me -> ImmutaballIOF me
a <>- b = JoinIBIOF $ a `AndIBIOF` b

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

runImmutaballIOIO (ThrowIO e) = throwIO e

runImmutaballIOIO (ArrayToBS array_ withBS) = hArrayToBS array_ >>= withBS

runImmutaballIOIO (ThawIO   iarray withMArray) = thaw   iarray >>= withMArray
runImmutaballIOIO (FreezeIO marray withIArray) = freeze marray >>= withIArray

runImmutaballIOIO (Poke ptr val ibio)    = poke ptr val >> ibio
runImmutaballIOIO (Peek ptr     withVal) = peek ptr     >>= withVal

runBasicImmutaballIO :: BasicIO -> ImmutaballIO
runBasicImmutaballIO bio = Fixed $ BasicIBIOF (runBasicImmutaballIO <$> getFixed bio)

runDirectoryImmutaballIO :: DirectoryIO -> ImmutaballIO
runDirectoryImmutaballIO dio = runBasicImmutaballIO . runDirectoryBasicIO $ dio

runSDLImmutaballIO :: SDLIO -> ImmutaballIO
runSDLImmutaballIO sdlio = runBasicImmutaballIO . runSDLBasicIO $ sdlio

-- | Copy an array to an immutable strict bytestring.
hArrayToBS :: (Integral i, Ix i, Storable e) => StorableArray i e -> IO BS.ByteString
hArrayToBS array_ = do
	(len :: Integer) <- fromIntegral <$> getNumElements array_
	size <- do
		if len <= 0
			then return 0
			else do
				arrayHead <- readArray array_ (fromIntegral (0 :: Integer))
				let elemSize = fromIntegral $ sizeOf arrayHead
				let size_ = len * elemSize
				return size_
	bs <- withStorableArray array_ $ \ptr -> BS.packCStringLen ((castPtr ptr), fromIntegral $ size)
	return bs

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyIBIO :: ImmutaballIO
mkEmptyIBIO = Fixed $ EmptyIBIOF

mkPureIBIO :: ImmutaballIO -> ImmutaballIO
mkPureIBIO ibio = Fixed $ PureIBIOF ibio

mkUnfixIBIO :: ImmutaballIO -> ImmutaballIO
mkUnfixIBIO ibio = Fixed $ UnfixIBIOF (getFixed ibio)

mkJoinIBIO :: ImmutaballIO -> ImmutaballIO
mkJoinIBIO ibio = Fixed $ JoinIBIOF (getFixed <$> getFixed ibio)

mkAndIBIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndIBIO a b = Fixed $ AndIBIOF a b

mkThenIBIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkThenIBIO a b = Fixed $ ThenIBIOF a b

mkBasicIBIO :: BasicIOF ImmutaballIO -> ImmutaballIO
mkBasicIBIO bio = Fixed $ BasicIBIOF bio

mkWait :: Async a -> (a -> ImmutaballIO) -> ImmutaballIO
mkWait async_ withAsync_ = Fixed $ Wait async_ withAsync_

mkWithAsync :: ImmutaballIO -> (Async () -> ImmutaballIO) -> ImmutaballIO
mkWithAsync ibio withAsync_ = Fixed $ WithAsync ibio withAsync_

mkAtomically :: STM a -> (a -> ImmutaballIO) -> ImmutaballIO
mkAtomically stm withStm = Fixed $ Atomically stm withStm

mkThrowIO :: (HasCallStack, Exception e) => e -> ImmutaballIO
mkThrowIO e = Fixed $ ThrowIO e

mkArrayToBS :: (Integral i, Ix i, Storable e) => StorableArray i e -> (BS.ByteString -> ImmutaballIO) -> ImmutaballIO
mkArrayToBS array_ withBS = Fixed $ ArrayToBS array_ withBS

mkThawIO :: (Ix i, IArray a e, MArray b e IO) => a i e -> (b i e -> ImmutaballIO) -> ImmutaballIO
mkThawIO iarray withMArray = Fixed $ ThawIO iarray withMArray

mkFreezeIO :: (Ix i, MArray a e IO, IArray b e) => a i e -> (b i e -> ImmutaballIO) -> ImmutaballIO
mkFreezeIO marray withIArray = Fixed $ FreezeIO marray withIArray

mkPoke :: (Storable a) => Ptr a -> a -> ImmutaballIO -> ImmutaballIO
mkPoke ptr val ibio = Fixed $ Poke ptr val ibio

mkPeek :: (Storable a) => Ptr a -> (a -> ImmutaballIO) -> ImmutaballIO
mkPeek ptr withVal = Fixed $ Peek ptr withVal

-- * Utils

-- | Short alias for 'mkBasicImmutaballIO'.
mkBIO :: BasicIOF ImmutaballIO -> ImmutaballIO
mkBIO = mkBasicIBIO
