{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The part of Level.Analysis that uses low-level memory management to interface with the GPU.
module Immutaball.Share.Level.Analysis.LowLevel
	(
		gpuEncodeArray
	) where

-- Prelude imports.
import Prelude ()
import Immutaball.Prelude

-- base imports.
import Data.Int
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

-- external imports.
import Data.Array.IArray
import Data.Array.Storable
import qualified Data.Array.Unsafe
import qualified Data.ByteString as BS

-- internal (local) imports.
import Immutaball.Share.ImmutaballIO.GLIO

-- Low-level imports.
import System.IO.Unsafe (unsafePerformIO)

gpuEncodeArray :: forall a. (Storable a) => Array Int32 a -> GLData
gpuEncodeArray array_ = unsafePerformIO $ do
	(storableArray :: StorableArray Int32 a) <- Data.Array.Unsafe.unsafeThaw array_
	bs <- withStorableArray storableArray $ \(ptr :: Ptr a) -> do
		let (bsPtr :: Ptr CChar) = castPtr ptr
		let (numElems :: Int) = rangeSize . bounds $ array_
		let (bsLen :: Int) = numElems * sizeOf ((array_ ! 0)  :: a)
		bs <- BS.packCStringLen (bsPtr, bsLen)
		return bs
	let data_ = bsToGLData bs
	return $ data_
