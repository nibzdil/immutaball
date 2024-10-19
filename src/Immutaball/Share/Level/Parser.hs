{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Parser.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE ScopedTypeVariables, NondecreasingIndentation, ExistentialQuantification #-}

module Immutaball.Share.Level.Parser
	(
		-- * parsing
		parseLevelFile,

		-- * exceptions
		LevelIBParseException(..),
		levelIBParseExceptionToException,
		levelIBParseExceptionFromException,
		UndersizedLevelIBParseException(..)
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Exception
import Data.Typeable
import Foreign.Ptr
import Text.Printf

import qualified Data.ByteString as BS
--import qualified Data.ByteString.Lazy as BL

import Immutaball.Share.Level.Base
import Immutaball.Share.Utils

-- Low-level imports.
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Unsafe as UB

import Debug.Trace as D-------------------------------- TODO

-- * parsing

-- | Uses low-level memory management.
parseLevelFile :: String -> BS.ByteString -> Either LevelIBParseException LevelIB
parseLevelFile inputName inputContents
	| inputSize < lengthSolSize =
		Left . err $ "Error: parseLevelFile: the data string does not have enough data to read lengths!"
	| otherwise = unsafePerformIO $
		UB.unsafeUseAsCString inputContents $ \inputPtr_ -> do
			let (inputPtr :: Ptr Sol) = castPtr inputPtr_
			(lengthSol :: Sol) <- peekSolLengths inputPtr

			-- Now we know the lengths, so we can calculate the size of the data we require.
			let neededSize = sizeOfExistingSol lengthSol
			let actualSize = inputSize
			flip D.trace (return ()) $ (printf "DEBUG0: %s" (show (lengthSol)))
			if' (actualSize < neededSize) (return . Left . err $ printf "Error: parseLevelFile: we parsed the lengths, but the data is too small to parse the file: input ‘%s’ has size %d <= %d" inputName inputSize neededSize) $ do

			-- Now parse the sol now that we validated the size.
			sol <- peekSol inputPtr
			return $ Right sol
	where
		lengthSolSize :: Int
		lengthSolSize = sizeOfEmptySol emptySol
		inputSize = BS.length inputContents
		err = LevelIBParseException . UndersizedLevelIBParseException

-- * exceptions

data LevelIBParseException = forall e. Exception e => LevelIBParseException e
instance Show LevelIBParseException where
	show (LevelIBParseException e) = show e
instance Exception LevelIBParseException
levelIBParseExceptionToException :: Exception e => e -> SomeException
levelIBParseExceptionToException = toException . LevelIBParseException
levelIBParseExceptionFromException :: Exception e => SomeException -> Maybe e
levelIBParseExceptionFromException x = do
	LevelIBParseException a <- fromException x
	cast a

-- | The input data was not big enough.
data UndersizedLevelIBParseException = UndersizedLevelIBParseException String
instance Exception UndersizedLevelIBParseException where
	toException = levelIBParseExceptionToException
	fromException = levelIBParseExceptionFromException
instance Show UndersizedLevelIBParseException where
	show (UndersizedLevelIBParseException msg) = msg
