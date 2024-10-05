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
import Control.Monad.STM
import Control.Parallel

import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Utils

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

instance Applicative ImmutaballIOF where
	pure = PureImmutaballIOF
	mf <*> ma = joinImmutaballIOF . flip fmap mf $ \f -> joinImmutaballIOF .  flip fmap ma $ \a -> pure (f a)
instance Monad ImmutaballIOF where
	return = pure
	m >>= f = joinImmutaballIOF $ f <$> m

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
