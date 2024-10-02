{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- ImmutaballIO.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, ScopedTypeVariables, FlexibleContexts, UndecidableInstances #-}

module Immutaball.Share.ImmutaballIO
	(
		-- * ImmutaballIO
		ImmutaballIO,
		ImmutaballIOF(..),
		runImmutaballIO,
		composeImmutaballIO,
		fmapImmutaballIO,
		fmapImmutaballIOFFixed,
		fmapImmutaballIOF,
		bindImmutaballIO,
		andImmutaballIO,
		thenImmutaballIO,
		voidAndImmutaballIO,
		voidThenImmutaballIO,
		joinImmutaballIO,
		(<>>),

		-- * Runners
		runImmutaballIOIO,
		runBasicImmutaballIO,
		runDirectoryImmutaballIO,
		runSDLImmutaballIO,

		-- * ImmutaballIO aliases that apply the Fixed wrapper
		mkPureImmutaballIO,
		mkAndImmutaballIO,
		mkThenImmutaballIO,
		mkArrImmutaballIO,
		mkBasicImmutaballIO
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Concurrent.Async
import Control.Parallel

import Immutaball.Share.ImmutaballIO.BasicIO hiding ((<>>))
import Immutaball.Share.ImmutaballIO.DirectoryIO
import Immutaball.Share.ImmutaballIO.SDLIO
import Immutaball.Share.Utils

-- * ImmutaballIO

type ImmutaballIO in_ out = Fixed (ImmutaballIOF in_ out)
data ImmutaballIOF in_ out me =
	  PureImmutaballIOF out
	| AndImmutaballIOF me me
	| ThenImmutaballIOF me me
	| ArrImmutaballIOF (in_ -> me)
	| BasicImmutaballIOF out (BasicIOF me)

	-- TODO:
	{-
	| Wait (Async ())
	| WithAsync (Async () -> me)
	| Atomically (STM ())
	-}

runImmutaballIO :: ImmutaballIO () () -> IO ()
runImmutaballIO bio = cata runImmutaballIOIO bio

composeImmutaballIO :: ImmutaballIO in_ mid -> ImmutaballIO mid out -> ImmutaballIO in_ out

composeImmutaballIO _x@(Fixed (PureImmutaballIOF _mid)) _y@(Fixed (PureImmutaballIOF out))      = Fixed (PureImmutaballIOF out)
composeImmutaballIO  x@(Fixed (PureImmutaballIOF _mid)) _y@(Fixed (AndImmutaballIOF a b))       = Fixed (AndImmutaballIOF (composeImmutaballIO x a) (composeImmutaballIO x b))
composeImmutaballIO  x@(Fixed (PureImmutaballIOF _mid)) _y@(Fixed (ThenImmutaballIOF a b))      = Fixed (ThenImmutaballIOF (composeImmutaballIO x a) (composeImmutaballIO x b))
composeImmutaballIO  x@(Fixed (PureImmutaballIOF  mid)) _y@(Fixed (ArrImmutaballIOF f))         = composeImmutaballIO x $ f mid
composeImmutaballIO  x@(Fixed (PureImmutaballIOF _mid)) _y@(Fixed (BasicImmutaballIOF out bio)) = Fixed (BasicImmutaballIOF out (composeImmutaballIO x <$> bio))

composeImmutaballIO _x@(Fixed (AndImmutaballIOF a b)) y = Fixed (AndImmutaballIOF (composeImmutaballIO a y) (composeImmutaballIO b y))

composeImmutaballIO _x@(Fixed (ThenImmutaballIOF a b)) y = Fixed (ThenImmutaballIOF (composeImmutaballIO a y) (composeImmutaballIO b y))

composeImmutaballIO _x@(Fixed (ArrImmutaballIOF f)) y = Fixed (ArrImmutaballIOF $ \in_ -> f in_ `composeImmutaballIO` y)

composeImmutaballIO _x@(Fixed (BasicImmutaballIOF _mid  bio))  y@(Fixed (PureImmutaballIOF out))       = Fixed (BasicImmutaballIOF out ((`composeImmutaballIO` y) <$> bio))
composeImmutaballIO  x@(Fixed (BasicImmutaballIOF _mid _bio)) _y@(Fixed (AndImmutaballIOF a b))        = Fixed (AndImmutaballIOF (composeImmutaballIO x a) (composeImmutaballIO x b))
composeImmutaballIO  x@(Fixed (BasicImmutaballIOF _mid _bio)) _y@(Fixed (ThenImmutaballIOF a b))       = Fixed (ThenImmutaballIOF (composeImmutaballIO x a) (composeImmutaballIO x b))
composeImmutaballIO  x@(Fixed (BasicImmutaballIOF  mid _bio)) _y@(Fixed (ArrImmutaballIOF f))          = x `composeImmutaballIO` f mid
composeImmutaballIO  x@(Fixed (BasicImmutaballIOF _mid  bio))  y@(Fixed (BasicImmutaballIOF out bio2)) = Fixed (BasicImmutaballIOF out (AndBasicIOF (Fixed . BasicImmutaballIOF out $ (`composeImmutaballIO` y) <$> bio) (Fixed . BasicImmutaballIOF out $ (x `composeImmutaballIO`) <$> bio2)))

fmapImmutaballIO :: (a -> b) -> (ImmutaballIO in_ a -> ImmutaballIO in_ b)
--fmapImmutaballIO f = (`composeImmutaballIO` (Fixed (ArrImmutaballIOF (Fixed . PureImmutaballIOF . f))))
fmapImmutaballIO f = Fixed . fmapImmutaballIOF (fmapImmutaballIO f) f . getFixed

fmapImmutaballIOFFixed :: (a -> b) -> (ImmutaballIOF in_ a me -> ImmutaballIOF in_ b me)
fmapImmutaballIOFFixed = fmapImmutaballIOF id

fmapImmutaballIOF :: (me0 -> me1) -> (a -> b) -> (ImmutaballIOF in_ a me0 -> ImmutaballIOF in_ b me1)
fmapImmutaballIOF _mef  f (PureImmutaballIOF a)      = PureImmutaballIOF (f a)
fmapImmutaballIOF  mef _f (AndImmutaballIOF a b)     = AndImmutaballIOF (mef a) (mef b)
fmapImmutaballIOF  mef _f (ThenImmutaballIOF a b)    = ThenImmutaballIOF (mef a) (mef b)
fmapImmutaballIOF  mef _f (ArrImmutaballIOF g)       = ArrImmutaballIOF (mef . g)
fmapImmutaballIOF  mef  f (BasicImmutaballIOF a bio) = BasicImmutaballIOF (f a) (mef <$> bio)

bindImmutaballIO :: ImmutaballIO in_ mid -> (mid -> ImmutaballIO in_ out) -> ImmutaballIO in_ out
bindImmutaballIO x fy = joinImmutaballIO $ fmapImmutaballIO fy x

andImmutaballIO :: ImmutaballIO in_ out -> ImmutaballIO in_ out -> ImmutaballIO in_ out
andImmutaballIO x y = Fixed $ AndImmutaballIOF x y

thenImmutaballIO :: ImmutaballIO in_ out -> ImmutaballIO in_ out -> ImmutaballIO in_ out
thenImmutaballIO x y = Fixed $ ThenImmutaballIOF x y

voidAndImmutaballIO :: (Monoid out) => ImmutaballIO in_ () -> ImmutaballIO in_ out -> ImmutaballIO in_ out
voidAndImmutaballIO x y = andImmutaballIO (fmapImmutaballIO (const mempty) x) y

voidThenImmutaballIO :: (Monoid out) => ImmutaballIO in_ () -> ImmutaballIO in_ out -> ImmutaballIO in_ out
voidThenImmutaballIO x y = thenImmutaballIO (fmapImmutaballIO (const mempty) x) y

joinImmutaballIO :: ImmutaballIO in_ (ImmutaballIO in_ out) -> ImmutaballIO in_ out
joinImmutaballIO (Fixed (PureImmutaballIOF ibio))     = ibio
joinImmutaballIO (Fixed (AndImmutaballIOF a b))       = Fixed $ AndImmutaballIOF (joinImmutaballIO a) (joinImmutaballIO b)
joinImmutaballIO (Fixed (ThenImmutaballIOF a b))      = Fixed $ ThenImmutaballIOF (joinImmutaballIO a) (joinImmutaballIO b)
joinImmutaballIO (Fixed (ArrImmutaballIOF f))         = Fixed $ ArrImmutaballIOF (\in_ -> joinImmutaballIO (f in_))
joinImmutaballIO (Fixed (BasicImmutaballIOF out bio)) = Fixed $ ArrImmutaballIOF (\in_ -> out `composeImmutaballIO` (Fixed (ArrImmutaballIOF (\out_ -> Fixed $ BasicImmutaballIOF out_ ((((Fixed $ PureImmutaballIOF in_) `composeImmutaballIO`) . joinImmutaballIO) <$> bio)))))

instance Semigroup (ImmutaballIOF in_ out (ImmutaballIO in_ out)) where
	(<>) :: ImmutaballIOF in_ out (ImmutaballIO in_ out) -> ImmutaballIOF in_ out (ImmutaballIO in_ out) -> ImmutaballIOF in_ out (ImmutaballIO in_ out)
	a <> b = getFixed $ andImmutaballIO (Fixed a) (Fixed b)
--instance (Monoid out) => Monoid (ImmutaballIOF in_ out (ImmutaballIO in_ out)) where
instance (Monoid out, Semigroup (ImmutaballIOF in_ out me)) => Monoid (ImmutaballIOF in_ out me) where
	mempty = PureImmutaballIOF mempty

instance Semigroup (ImmutaballIO in_ out) where
	(Fixed a) <> (Fixed b) = Fixed (a <> b)
instance (Monoid out) => Monoid (ImmutaballIO in_ out) where
	mempty = Fixed mempty

instance Functor (ImmutaballIOF in_ out) where
	fmap :: (a -> b) -> (ImmutaballIOF in_ out a -> ImmutaballIOF in_ out b)
	fmap _f (PureImmutaballIOF a)      = PureImmutaballIOF a
	fmap  f (AndImmutaballIOF a b)     = AndImmutaballIOF (f a) (f b)
	fmap  f (ThenImmutaballIOF a b)    = ThenImmutaballIOF (f a) (f b)
	fmap  f (ArrImmutaballIOF g)       = ArrImmutaballIOF (f . g)
	fmap  f (BasicImmutaballIOF out bio) = BasicImmutaballIOF out $ f <$> bio

-- | Add an ordering constraint.
infixr 6 <>>
(<>>) :: ImmutaballIO in_ out -> ImmutaballIO in_ out -> ImmutaballIO in_ out
(<>>) = thenImmutaballIO

-- * Runners

runImmutaballIOIO :: ImmutaballIOF () () (IO ()) -> IO ()
runImmutaballIOIO (PureImmutaballIOF ())      = return ()
runImmutaballIOIO (AndImmutaballIOF a b)      = a `par` b `par` concurrently_ a b
runImmutaballIOIO (ThenImmutaballIOF a b)     = a >> b
runImmutaballIOIO (ArrImmutaballIOF f)        = f ()
runImmutaballIOIO (BasicImmutaballIOF () bio) = runBasicIOIO bio

runBasicImmutaballIO :: BasicIO -> ImmutaballIO () ()
runBasicImmutaballIO bio = Fixed $ BasicImmutaballIOF () (runBasicImmutaballIO <$> getFixed bio)

runDirectoryImmutaballIO :: DirectoryIO -> (FilePath -> ImmutaballIO () ()) -> ImmutaballIO () ()
--runDirectoryImmutaballIO dio withPath = runBasicImmutaballIO $ runDirectoryBasicIO dio withPath
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryData   path)) withPath = Fixed $ BasicImmutaballIOF () (GetDirectory (GetXdgDirectoryData   path) withPath)
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryConfig path)) withPath = Fixed $ BasicImmutaballIOF () (GetDirectory (GetXdgDirectoryConfig path) withPath)
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryCache  path)) withPath = Fixed $ BasicImmutaballIOF () (GetDirectory (GetXdgDirectoryCache  path) withPath)
runDirectoryImmutaballIO _dio@(Fixed (GetXdgDirectoryState  path)) withPath = Fixed $ BasicImmutaballIOF () (GetDirectory (GetXdgDirectoryState  path) withPath)

runSDLImmutaballIO :: SDLIO -> ImmutaballIO () ()
runSDLImmutaballIO sdlio = runBasicImmutaballIO . runSDLBasicIO $ sdlio

-- * ImutaballIO aliases that apply the Fixed wrapper

mkPureImmutaballIO :: out -> ImmutaballIO in_ out
mkPureImmutaballIO out = Fixed $ PureImmutaballIOF out

mkAndImmutaballIO :: ImmutaballIO in_ out -> ImmutaballIO in_ out -> ImmutaballIO in_ out
mkAndImmutaballIO a b = Fixed $ AndImmutaballIOF a b

mkThenImmutaballIO :: ImmutaballIO in_ out -> ImmutaballIO in_ out -> ImmutaballIO in_ out
mkThenImmutaballIO a b = Fixed $ ThenImmutaballIOF a b

mkArrImmutaballIO :: (in_ -> ImmutaballIO in_ out) -> ImmutaballIO in_ out
mkArrImmutaballIO f = Fixed $ ArrImmutaballIOF f

mkBasicImmutaballIO :: out -> BasicIOF (ImmutaballIO in_ out) -> ImmutaballIO in_ out
mkBasicImmutaballIO out bio = Fixed $ BasicImmutaballIOF out bio
