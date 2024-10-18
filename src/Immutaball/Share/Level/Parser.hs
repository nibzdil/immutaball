{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Parser.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables, NondecreasingIndentation #-}

module Immutaball.Share.Level.Parser
	(
		parseLevelFile
	) where

import Prelude ()
import Immutaball.Prelude

import Foreign.Ptr
import Text.Printf

import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BL

import Immutaball.Share.Level.Base
import Immutaball.Share.Utils

-- Low-level imports.
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Unsafe as UB

-- | Uses low-level memory management.
parseLevelFile :: String -> BS.ByteString -> Either String LevelIB
parseLevelFile inputName inputContents
	| inputSize < lengthSolSize =
		Left "Error: parseLevelFile: the data string does not have enough data to read lengths!"
	| otherwise = unsafePerformIO $
		UB.unsafeUseAsCString inputContents $ \inputPtr_ -> do
			let (inputPtr :: Ptr Sol) = castPtr inputPtr_
			(lengthSol :: Sol) <- peekSolLengths inputPtr

			-- Now we know the lengths, so we can calculate the size of the data we require.
			let neededSize = sizeOfExistingSol lengthSol
			let actualSize = inputSize
			if' (actualSize < neededSize) (return . Left $ printf "Error: parseLevelFile: we parsed the lengths, but the data is too small to parse the file: input ‘%s’ has size %d <= %d" inputName inputSize neededSize) $ do

			-- Now parse the sol now that we validated the size.
			sol <- peekSol inputPtr
			return $ Right sol
	where
		lengthSolSize :: Int
		lengthSolSize = sizeOfEmptySol emptySol
		inputSize = BS.length inputContents
