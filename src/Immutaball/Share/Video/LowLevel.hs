{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Video/LowLevel.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Immutaball.Share.Video.LowLevel
	(
		reverseRowsImage
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad
import Data.Function
import Data.Word

import qualified Data.ByteString as BS

import Immutaball.Share.Math
import Immutaball.Share.Utils

-- reverseRowsImage low-level imports.
import qualified Data.ByteString.Unsafe as UnsafeBS
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)

-- | The old version was really slow.
--
-- Use unsafe lower-level functions to manually implement a faster version,
-- given the limitations of the safe bytestring interface provided along with
-- the low-level implementation of it.
reverseRowsImage :: (WidthHeightI, BS.ByteString) -> BS.ByteString
reverseRowsImage ((w, h), image)
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
