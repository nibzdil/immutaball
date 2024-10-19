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
		levelFileParser,

		-- * optional low-level parsing
		unsafeParseLevelFileRaw,

		-- * exceptions
		LevelIBParseException(..),
		levelIBParseExceptionToException,
		levelIBParseExceptionFromException,
		UndersizedLevelIBParseException(..),
		MissingSOLMagicIBParseException(..),
		UnsupportedSOLVersionIBParseException(..),
		OversizedLevelIBParseException(..),
		ParseErrorIBParseException(..)
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Exception
import Control.Lens
import Data.Typeable
import Foreign.Ptr
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Text.Parsec

import Immutaball.Share.Level.Base
import Immutaball.Share.Utils

-- Low-level imports.
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Unsafe as UB

import Debug.Trace as D-------------------------------- TODO

-- * optional low-level parsing

-- | Uses low-level memory management.
--
-- It does _some_ length checking but still does some memory reading without
-- checking for length of the input data (and only afterward reading).
unsafeParseLevelFileRaw :: String -> BS.ByteString -> Either LevelIBParseException LevelIB
unsafeParseLevelFileRaw inputName inputContents
	| inputSize < lengthSolSize =
		Left . errSize $ "Error: parseLevelFile: the data string does not have enough data to read the header with lengths!"
	| otherwise = unsafePerformIO $
		UB.unsafeUseAsCString inputContents $ \inputPtr_ -> do
			let (inputPtr :: Ptr Sol) = castPtr inputPtr_
			(lengthSol :: Sol) <- peekSolLengths inputPtr

			-- Check magic.
			if' ((lengthSol^.solMagic)   /= solMagicConstant) (return . Left . errMagic   $ printf "Error: parseLevelFile: input ‘%s’ is missing the 4 SOL magic bytes.  Is it a SOL file?  0x%04X /= 0x%04X." inputName (lengthSol^.solMagic) solMagicConstant) $ do

			-- Check version.
			if' ((lengthSol^.solVersion) /= solVersionCurr)   (return . Left . errVersion $ printf "Error: parseLevelFile: SOL file input ‘%s’ has an unsupported version.  Its code %d /= %d." inputName (lengthSol^.solVersion) solVersionCurr) $ do

			-- Now we know the lengths, so we can calculate the size of the data we require.
			let neededSizeMin = sizeOfExistingSolMin lengthSol
			let neededSizeMax = sizeOfExistingSolMax lengthSol
			let actualSize = inputSize
			flip D.trace (return ()) $ (printf "DEBUG0: %s" (show (lengthSol)))
			if' (actualSize < neededSizeMin) (return . Left . errSize $ printf "Error: parseLevelFile: we parsed the lengths, but the data is too small to parse the file: input ‘%s’ has size %d < %d" inputName inputSize neededSizeMin) $ do

			-- Also check for oversize.
			if' (actualSize > neededSizeMax) (return . Left . errBigSize $ printf "Error: parseLevelFile: we parsed the lengths, but the data is larger than expected: input ‘%s’ has size %d > %d; recommend aborting in case data is corruted" inputName inputSize neededSizeMax) $ do

			-- Now parse the sol now that we validated the size.
			-- This is unsafe since we don't know if we have enough data, but we at least know we're somewhat within the bounds.
			sol <- peekSol inputPtr

			-- Now check the exact length.
			let neededSize = sizeOfExistingSol lengthSol
			if' (actualSize < neededSize) (return . Left . errSize $ printf "Error: parseLevelFile: we parsed the lengths, but the data is too small to parse the file (exact sizE): input ‘%s’ has size %d < %d" inputName inputSize neededSize) $ do
			if' (actualSize > neededSize) (return . Left . errBigSize $ printf "Error: parseLevelFile: we parsed the lengths, but the data is larger than expected (exact sizE): input ‘%s’ has size %d > %d; recommend aborting in case data is corruted" inputName inputSize neededSize) $ do

			-- Return our parsed Sol.
			return $ Right sol
	where
		lengthSolSize :: Int
		lengthSolSize = sizeOfEmptySol emptySol
		inputSize = BS.length inputContents
		errSize = LevelIBParseException . UndersizedLevelIBParseException
		errBigSize = LevelIBParseException . OversizedLevelIBParseException
		errMagic = LevelIBParseException . MissingSOLMagicIBParseException
		errVersion = LevelIBParseException . UnsupportedSOLVersionIBParseException

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

-- | The file does not start with the 4 SOL magic bytes.
data MissingSOLMagicIBParseException = MissingSOLMagicIBParseException String
instance Exception MissingSOLMagicIBParseException where
	toException = levelIBParseExceptionToException
	fromException = levelIBParseExceptionFromException
instance Show MissingSOLMagicIBParseException where
	show (MissingSOLMagicIBParseException msg) = msg

-- | The SOL version is not supported.
data UnsupportedSOLVersionIBParseException = UnsupportedSOLVersionIBParseException String
instance Exception UnsupportedSOLVersionIBParseException where
	toException = levelIBParseExceptionToException
	fromException = levelIBParseExceptionFromException
instance Show UnsupportedSOLVersionIBParseException where
	show (UnsupportedSOLVersionIBParseException msg) = msg

-- | The input data was bigger than expected.
data OversizedLevelIBParseException = OversizedLevelIBParseException String
instance Exception OversizedLevelIBParseException where
	toException = levelIBParseExceptionToException
	fromException = levelIBParseExceptionFromException
instance Show OversizedLevelIBParseException where
	show (OversizedLevelIBParseException msg) = msg

-- | The input data was bigger than expected.
data ParseErrorLevelIBParseException = ParseErrorLevelIBParseException ParseError
instance Exception ParseErrorLevelIBParseException where
	toException = levelIBParseExceptionToException
	fromException = levelIBParseExceptionFromException
instance Show ParseErrorLevelIBParseException where
	show (ParseErrorLevelIBParseException parseError) = show parseError

-- * parsing

parseLevelFile :: String -> BL.ByteString -> Either LevelIBParseException LevelIB
parseLevelFile inputName inputContents = LevelIBParseException . ParseErrorLevelIBParseException +++ id $ parse levelFileParser inputName inputContents

levelFileParser :: Parsec BL.ByteString () LevelIB
levelFileParser = _
