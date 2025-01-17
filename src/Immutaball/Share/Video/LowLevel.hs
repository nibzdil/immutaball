{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Video/LowLevel.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | gpuEncodeArray and reverseRowsImage.
module Immutaball.Share.Video.LowLevel
	(
		gpuEncodeArray,
		reverseRowsImage,
		reverseRowsImageBuilderRows,
		reverseRowsImageBuilderBytes,
		reverseRowsImageLowLevel
	) where

-- Prelude imports.
import Prelude ()
import Immutaball.Prelude

-- base imports.
import Control.Arrow
import Control.Monad
import qualified Data.Array.Unsafe
import Data.Function hiding (id, (.))
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

-- external imports.
import Data.Array.IArray
import Data.Array.Storable
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
--import qualified Data.ByteString.Builder.Extra as BB
import qualified Data.ByteString.Lazy as BL

-- internal (local) imports.
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.Math
import Immutaball.Share.Utils

-- Low-level imports.
import qualified Data.ByteString.Unsafe as UnsafeBS
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

reverseRowsImage :: (WidthHeightI, BS.ByteString) -> BS.ByteString
--reverseRowsImage = reverseRowsImageLowLevel
--reverseRowsImage = reverseRowsImageBuilderBytes
reverseRowsImage = reverseRowsImageBuilderRows

-- This is much better.  And safe!
reverseRowsImageBuilderRows :: (WidthHeightI, BS.ByteString) -> BS.ByteString
reverseRowsImageBuilderRows ((w, h), image)
	| BS.length image <= 0 = image
	| otherwise = BL.toStrict . BB.toLazyByteString $
		flip fix 0 $ \withRow row ->
			if' (row >= h') mempty $
			(BB.byteString . BS.take (w'*4) . BS.drop (((h'-1)-row)*w'*4) $ image) <> (withRow (row+1))
	where
		w', h' :: Int
		(w', h') = join (***) fromIntegral (w, h)

-- This is still noticeably slower than reverseRowsImageLowLevel.
reverseRowsImageBuilderBytes :: (WidthHeightI, BS.ByteString) -> BS.ByteString
reverseRowsImageBuilderBytes ((w, h), image)
	| BS.length image <= 0 = image
	| otherwise = BL.toStrict . BB.toLazyByteString $
	-- | otherwise = BL.toStrict . BB.toLazyByteStringWith (BB.safeStrategy chunkSize chunkSize) BL.empty $
		flip fix 0 $ \withRow row ->
			if' (row >= h') mempty $
			flip fix 0 $ \withCol col ->
				if' (col >= w') (withRow (row+1)) $
				flip fix 0 $ \withComponent component ->
					if' (component >= 4) (withCol (col+1)) $
					let idx = ((h'-1)-row)*w'*4 + col*4 + component in
					if' (idx >= BS.length image) mempty $
					--(BB.word8 $ image `UnsafeBS.unsafeIndex` idx) <> (withComponent (component+1))  -- This doesn't seem to be a major improvement on the core performance issue.  Keep it to the safe API.
					(BB.word8 $ image `BS.index` idx) <> (withComponent (component+1))
	where
		w', h' :: Int
		(w', h') = join (***) fromIntegral (w, h)
		--chunkSize :: Int
		--chunkSize = 2^(20 :: Int)

-- | The old version was really slow.
--
-- Use unsafe lower-level functions to manually implement a faster version,
-- given the limitations of the safe bytestring interface provided along with
-- the low-level implementation of it.
--
-- But now we have a safe _and_ faster version that uses bytestring builders; see 'reverseRowsImageBuilderRows'.
reverseRowsImageLowLevel :: (WidthHeightI, BS.ByteString) -> BS.ByteString
reverseRowsImageLowLevel ((w, h), image)
	| BS.length image <= 0 = image
	| otherwise = unsafePerformIO $ do
		-- Unsafe optimization to avoid a copy: use unsafeUseAsCStringLen with the promise we won't mutate the data.
		--useAsCStringLen image $ \(ptr, len) -> do
		UnsafeBS.unsafeUseAsCStringLen image $ \(ptr, len_) -> do
			let len = max 1 $ len_
			let (imageBufferI :: Ptr Word8) = castPtr ptr
			(imageBufferO :: Ptr Word8) <- mallocArray len
			flip fix 0 $ \withRow rowI -> let rowOffsetI = rowI * 4*w' in if' (rowOffsetI >= len_) (return ()) $ do
				let rowO = (h' - 1) - rowI; rowOffsetO = rowO * 4*w'
				flip fix 0 $ \withCol colI -> let colOffsetI = 4*colI in if' (rowOffsetI + colOffsetI + 3 >= len_) (return ()) $ do
					if' (colI >= w') (withRow (rowI+1)) $ do
						let colO = colI; colOffsetO = 4*colO
						peek (imageBufferI `plusPtr'` (rowOffsetI + colOffsetI + 0)) >>= poke (imageBufferO `plusPtr'` (rowOffsetO + colOffsetO + 0))
						peek (imageBufferI `plusPtr'` (rowOffsetI + colOffsetI + 1)) >>= poke (imageBufferO `plusPtr'` (rowOffsetO + colOffsetO + 1))
						peek (imageBufferI `plusPtr'` (rowOffsetI + colOffsetI + 2)) >>= poke (imageBufferO `plusPtr'` (rowOffsetO + colOffsetO + 2))
						peek (imageBufferI `plusPtr'` (rowOffsetI + colOffsetI + 3)) >>= poke (imageBufferO `plusPtr'` (rowOffsetO + colOffsetO + 3))
						withCol (colI+1)
			UnsafeBS.unsafePackMallocCStringLen $ (castPtr imageBufferO, len)
	where
		w', h' :: Int
		(w', h') = join (***) fromIntegral (w, h)
		plusPtr' :: Ptr a -> Int -> Ptr a
		plusPtr' = plusPtr
