{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Video
	(
		reverseRowsImage
	) where

import Prelude ()
import Immutaball.Prelude

import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word

import Immutaball.Share.Math

-- TODO: learn the new bytestring builders and probably use them.
reverseRowsImage :: (WidthHeightI, BL.ByteString) -> BL.ByteString
reverseRowsImage ((w, _h), image) = glImage
	where
		glImage = BL.pack glBytes
		bytes :: [Word8]
		bytes = BL.unpack image
		glBytes :: [Word8]
		glBytes = concat glRows
		glRows :: [[Word8]]
		glRows = reverse rows
		rows :: [[Word8]]
		rows = withRemaining bytes
		withRemaining :: [Word8] -> [[Word8]]
		withRemaining [] = []
		withRemaining xs = genericTake w xs : withRemaining (genericDrop w xs)
