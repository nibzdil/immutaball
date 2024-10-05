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
		fixImmutaballIOF,
		extractMesImmutaballIOF,
		extractFirstMeImmutaballIOF,

		-- * Runners
		runImmutaballIOIO,
		runBasicImmutaballIO,
		runDirectoryImmutaballIO,
		runSDLImmutaballIO,

		-- * ImmutaballIO aliases that apply the Fixed wrapper
		mkEmptyImmutaballIO,
		mkPureImmutaballIO,
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

import Debug.Trace as D  ---------------------------- TODO--
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
import Control.Concurrent.MVar
import Control.Exception
import Control.Exception.Base

-- * ImmutaballIO

type ImmutaballIO = Fixed ImmutaballIOF
data ImmutaballIOF me =
	  EmptyImmutaballIOF
	| PureImmutaballIOF me
	| JoinImmutaballIOF (ImmutaballIOF (ImmutaballIOF me))
	| AndImmutaballIOF me me
	| ThenImmutaballIOF me me
	| BasicImmutaballIOF (BasicIOF me)

	| forall hiddenTypeField. Wait (Async hiddenTypeField) (hiddenTypeField -> me)
	| WithAsync me (Async () -> me)
	| forall hiddenTypeField. Atomically (STM hiddenTypeField) (hiddenTypeField -> me)

runImmutaballIO :: ImmutaballIO -> IO ()
runImmutaballIO bio = cata runImmutaballIOIO bio

andImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO 
andImmutaballIO x y = Fixed $ AndImmutaballIOF x y

thenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
thenImmutaballIO x y = Fixed $ ThenImmutaballIOF x y

instance Semigroup (ImmutaballIOF ImmutaballIO) where
	(<>) :: ImmutaballIOF ImmutaballIO -> ImmutaballIOF ImmutaballIO -> ImmutaballIOF ImmutaballIO
	a <> b = AndImmutaballIOF (Fixed a) (Fixed b)
instance (Semigroup (ImmutaballIOF me)) => Monoid (ImmutaballIOF me) where
	mempty = EmptyImmutaballIOF

instance Semigroup ImmutaballIO where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance Monoid ImmutaballIO where
	mempty = Fixed EmptyImmutaballIOF

instance Functor ImmutaballIOF where
	fmap :: (a -> b) -> (ImmutaballIOF a -> ImmutaballIOF b)
	fmap _f (EmptyImmutaballIOF)     = EmptyImmutaballIOF
	fmap  f (PureImmutaballIOF a)    = PureImmutaballIOF (f a)
	fmap  f (JoinImmutaballIOF ibio) = JoinImmutaballIOF (fmap f <$> ibio)
	fmap  f (AndImmutaballIOF a b)   = AndImmutaballIOF (f a) (f b)
	fmap  f (ThenImmutaballIOF a b)  = ThenImmutaballIOF (f a) (f b)
	fmap  f (BasicImmutaballIOF bio) = BasicImmutaballIOF $ f <$> bio

	fmap  f (Wait async_ withAsync_)    = Wait async_ (f . withAsync_)
	fmap  f (WithAsync ibio withAsync_) = WithAsync (f ibio) (f . withAsync_)
	fmap  f (Atomically stm withStm)    = Atomically stm (f . withStm)

joinImmutaballIOF :: ImmutaballIOF (ImmutaballIOF a) -> ImmutaballIOF a
joinImmutaballIOF = JoinImmutaballIOF

{-
-- Do it like fixIO.
fixImmutaballIOF :: (me -> ImmutaballIOF me) -> ImmutaballIOF me
--fixImmutaballIOF f = D.trace "DEBUG: IBIO mfix" $ f <$> fixImmutaballIOF f
fixImmutaballIOF f = unsafePerformIO $ do
	m <- newEmptyMVar
	ans <- unsafeDupableInterleaveIO (readMVar m `catch` \BlockedIndefinitelyOnMVar -> throwIO FixIOException)
	let result = fmap (\me -> unsafePerformIO $ putMVar m me >> return me) $ f ans
	return result
-}
-- TODO: add a test case for fixm with ImmutaballIO (e.g. could be just
-- something simple that ignores the input).
fixImmutaballIOF :: (me -> ImmutaballIOF me) -> ImmutaballIOF me
--fixImmutaballIOF f = D.trace "DEBUG: IBIO mfix" $ f <$> fixImmutaballIOF f
-- | Try extracting the first ‘me’ to spark things off.
-- Then if multiple ‘me’s are observed in the result, apply f to each, and join.
-- e.g. if f returns an And, then the left output (or if empty the right (no
-- mults in this case)) will first spark f, and then the result is an And of
-- left of f applied to f, and the right of f applied to f, and then joining.
-- Note: accessing \me in ‘fix ImmutaballIO $ \me -> ’ across Wait, Atomically,
-- or similar callback boundaries is an error.  What would \me refer to?
-- TODO remove next line after debugging.
--fixImmutaballIOF f = fix $ \me -> f (maybe emptyErr id $ extractFirstMeImmutaballIOF me)
fixImmutaballIOF f = fix $ \me -> case f (maybe emptyErr id $ extractFirstMeImmutaballIOF me) of
	--    mfix f = mfix f >>= f
	-- => mfix f = join $ f <$> mfix f
	x -> joinImmutaballIOF $ f <$> x
	{-
	{-
	EmptyImmutaballIOF -> EmptyImmutaballIOF
	(PureImmutaballIOF a) -> joinImmutaballIOF $ PureImmutaballIOF (f a)
	(JoinImmutaballIOF ibio) -> joinImmutaballIOF $ JoinImmutaballIOF (fmap (fmap f) ibio)
	(AndImmutaballIOF a b) -> joinImmutaballIOF $ AndImmutaballIOF (f a) (f b)
	(ThenImmutaballIOF a b) -> joinImmutaballIOF $ ThenImmutaballIOF (f a) (f b)
	(BasicImmutaballIOF bio) -> joinImmutaballIOF $ BasicImmutaballIOF (fmap f bio)
	(Wait async_ withAsync_) -> joinImmutaballIOF $ Wait async_ (f . withAsync_)
	(WithAsync async_ withAsync_) -> joinImmutaballIOF $ WithAsync (f async_) (f . withAsync_)
	(Atomically stm withStm) -> joinImmutaballIOF $ Atomically stm (f . withStm)
	-}
	EmptyImmutaballIOF -> EmptyImmutaballIOF
	(PureImmutaballIOF a) -> joinImmutaballIOF $ PureImmutaballIOF (PureImmutaballIOF a)
	(JoinImmutaballIOF ibio) -> joinImmutaballIOF $ JoinImmutaballIOF (fmap (fmap f) ibio)
	(AndImmutaballIOF a b) -> joinImmutaballIOF $ AndImmutaballIOF (PureImmutaballIOF a) (f b)
	(ThenImmutaballIOF a b) -> joinImmutaballIOF $ ThenImmutaballIOF (PureImmutaballIOF a) (f b)
	(BasicImmutaballIOF bio) -> joinImmutaballIOF $ BasicImmutaballIOF (fmap f bio)
	(Wait async_ withAsync_) -> joinImmutaballIOF $ Wait async_ (f . withAsync_)
	(WithAsync async_ withAsync_) -> joinImmutaballIOF $ WithAsync (f async_) (f . withAsync_)
	(Atomically stm withStm) -> joinImmutaballIOF $ Atomically stm (f . withStm)
	-}
	where
		emptyErr = error "Error: fixImmutaballIOF: there is no non-empty value in the result"
{-
mfix (return . h) = return (fix h)

  mfix (return . h)
= mfix (PureImmutaballIOF . h)
= case PureImmutaballIOF (h (mfix (PureImmutaballIOF . h))) of PureImmutaballIO a -> joinImmutaballIOF $ PureImmutaballIOF ((return . h) a)
= joinImmutaballIOF $ PureImmutaballIOF ((return . h) (h (mfix (PureImmutaballIOF . h))))
= PureImmutaballIOF ((return . h) (h (mfix (PureImmutaballIOF . h))))
= return ((return . h) (h (mfix (PureImmutaballIOF . h))))
= h (h (mfix (PureImmutaballIOF . h)))
-}

extractMesImmutaballIOF :: ImmutaballIOF me -> [me]
extractMesImmutaballIOF (EmptyImmutaballIOF)          = []
extractMesImmutaballIOF (PureImmutaballIOF a)         = [a]
extractMesImmutaballIOF (JoinImmutaballIOF ibio)      = extractMesImmutaballIOF ibio >>= extractMesImmutaballIOF
extractMesImmutaballIOF (AndImmutaballIOF a b)        = [a, b]
extractMesImmutaballIOF (ThenImmutaballIOF a b)       = [a, b]
extractMesImmutaballIOF (BasicImmutaballIOF bio)      = extractMesBasicIOF bio
extractMesImmutaballIOF (Wait _async_ _withAsync_)    = []
extractMesImmutaballIOF (WithAsync async_ withAsync_) = [async_]
extractMesImmutaballIOF (Atomically _stm _withStm)    = []

extractFirstMeImmutaballIOF :: ImmutaballIOF me -> Maybe me
extractFirstMeImmutaballIOF = safeHead . extractMesImmutaballIOF

instance Applicative ImmutaballIOF where
	pure = PureImmutaballIOF
	mf <*> ma = joinImmutaballIOF . flip fmap mf $ \f -> joinImmutaballIOF .  flip fmap ma $ \a -> pure (f a)
instance Monad ImmutaballIOF where
	return = pure
	m >>= f = joinImmutaballIOF $ f <$> m
instance MonadFix ImmutaballIOF where
	--mfix = mfix'
	--mfix f = let ma = D.trace "DEBUG: IBIO mfix" ma >>= f in ma
	--mfix f = let ~ma = D.trace "DEBUG: IBIO mfix" ma >>= f in ma
	--mfix f = let ma = D.trace "DEBUG: IBIO mfix" ma >>= f in ma
	mfix :: (a -> ImmutaballIOF a) -> ImmutaballIOF a
	mfix = fixImmutaballIOF
	--mfix f = D.trace "DEBUG: IBIO mfix" $ f ()
	-- Spams IBIO mfix.
	--mfix f = D.trace "DEBUG: IBIO mfix" $ joinImmutaballIOF $ f <$> mfix f
	--mfix f = D.trace "DEBUG: IBIO mfix" $ f <$> mfix f
--mfix' f = fix $ \me -> me >>= f
--mfix' f = let ma = ma >>= f in ma

{-
instance Foldable ImmutaballIOF where
	foldr :: (a -> b -> b) -> b -> ImmutaballIOF a -> b
	foldr _reduce reduction0 (EmptyImmutaballIOF)  = reduction0
	foldr  reduce reduction0 (PureImmutaballIOF a) = reduce a reduction0
	foldr  reduce reduction0 (JoinImmutaballIOF ibio) = ??? $ foldr reduce reduction0 <$> ibio
-}

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
(<>>) = thenImmutaballIO

-- * Runners

runImmutaballIOIO :: ImmutaballIOF (IO ()) -> IO ()

runImmutaballIOIO (EmptyImmutaballIOF)     = return ()
runImmutaballIOIO (PureImmutaballIOF a)    = a
runImmutaballIOIO (JoinImmutaballIOF ibio) = runImmutaballIOIO $ runImmutaballIOIO <$> ibio
runImmutaballIOIO (AndImmutaballIOF a b)   = a `par` b `par` concurrently_ a b
runImmutaballIOIO (ThenImmutaballIOF a b)  = a >> b
runImmutaballIOIO (BasicImmutaballIOF bio) = runBasicIOIO bio

runImmutaballIOIO (Wait async_ withAsync_)    = wait async_ >>= withAsync_
runImmutaballIOIO (WithAsync ibio withAsync_) = withAsync ibio withAsync_
runImmutaballIOIO (Atomically stm withStm)    = atomically stm >>= withStm

runBasicImmutaballIO :: BasicIO -> ImmutaballIO
runBasicImmutaballIO bio = Fixed $ BasicImmutaballIOF (runBasicImmutaballIO <$> getFixed bio)

runDirectoryImmutaballIO :: DirectoryIO -> ImmutaballIO
runDirectoryImmutaballIO dio = runBasicImmutaballIO . runDirectoryBasicIO $ dio

runSDLImmutaballIO :: SDLIO -> ImmutaballIO
runSDLImmutaballIO sdlio = runBasicImmutaballIO . runSDLBasicIO $ sdlio

-- * ImutaballIO aliases that apply the Fixed wrapper

mkEmptyImmutaballIO :: ImmutaballIO
mkEmptyImmutaballIO = Fixed $ EmptyImmutaballIOF

mkPureImmutaballIO :: ImmutaballIO -> ImmutaballIO
mkPureImmutaballIO ibio = Fixed $ PureImmutaballIOF ibio

mkJoinImmutaballIO :: ImmutaballIO -> ImmutaballIO
mkJoinImmutaballIO ibio = Fixed $ JoinImmutaballIOF (getFixed <$> getFixed ibio)

mkAndImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkAndImmutaballIO a b = Fixed $ AndImmutaballIOF a b

mkThenImmutaballIO :: ImmutaballIO -> ImmutaballIO -> ImmutaballIO
mkThenImmutaballIO a b = Fixed $ ThenImmutaballIOF a b

mkBasicImmutaballIO :: BasicIOF ImmutaballIO -> ImmutaballIO
mkBasicImmutaballIO bio = Fixed $ BasicImmutaballIOF bio

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
