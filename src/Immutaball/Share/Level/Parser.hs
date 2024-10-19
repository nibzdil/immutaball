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
		parsei32Native,
		parsei32BE,
		parsei32LE,
		parsef32dLE,
		parsen,
		parseCString,

		-- * optional low-level parsing
		unsafeParseLevelFileRaw,
		w32ToFloat,

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
import Data.Bits
import Data.Coerce
import Data.Function hiding (id, (.))
import Data.Int
import Data.Typeable
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

import Data.Array
import Data.Array.IArray as IA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified SDL.Raw.Enum as Raw
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

w32ToFloat :: Word32 -> Float
w32ToFloat w32 = unsafePerformIO $ do
	-- GHC doesn't support coerce between Word32 and Float.
	-- Just malloc a new cfloat.
	{-
	let (f32 :: Float)  = coerce w32
	-}
	(cfloat :: ForeignPtr CFloat) <- mallocForeignPtr
	(cf32 :: CFloat) <- withForeignPtr cfloat $ \cfloatPtr -> poke (castPtr cfloatPtr) w32 >> peek cfloatPtr
	let (f32 :: Float) = coerce cf32
	return f32

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

parseByte :: Parsec BL.ByteString () Word8
parseByte = truncateAsciiChar <$> anyChar
	where
		truncateAsciiChar :: Char -> Word8
		truncateAsciiChar = toEnum . fromEnum

parsei32Native :: Parsec BL.ByteString () Int32
parsei32Native = do
	byte0_ <- parseByte
	byte1_ <- parseByte
	byte2_ <- parseByte
	byte3_ <- parseByte
	let (byte0, byte1, byte2, byte3) = if' (Raw.SDL_BYTEORDER == Raw.SDL_BIG_ENDIAN) (byte0_, byte1_, byte2_, byte3_) (byte3_, byte2_, byte1_, byte0_)
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	return $ i32

parsei32BE :: Parsec BL.ByteString () Int32
parsei32BE = do
	byte0 <- parseByte
	byte1 <- parseByte
	byte2 <- parseByte
	byte3 <- parseByte
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	return $ i32

parsei32LE :: Parsec BL.ByteString () Int32
parsei32LE = do
	byte3 <- parseByte
	byte2 <- parseByte
	byte1 <- parseByte
	byte0 <- parseByte
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	return $ i32

parsef32dLE :: Parsec BL.ByteString () Double
parsef32dLE = do
	byte3 <- parseByte
	byte2 <- parseByte
	byte1 <- parseByte
	byte0 <- parseByte
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (fl  :: Float)  = w32ToFloat w32
	let (d   :: Double) = realToFrac fl
	return $ d

parsen :: forall a. Parsec BL.ByteString () a -> Int32 -> Parsec BL.ByteString () (Array Int32 a)
parsen parseElem n
	| n <= 0 = return emptyArray
	| otherwise = do
		as <- flip fix 0 $ \me idx -> do
			if' (idx >= n) (return []) $ do
			a <- parseElem
			(a:) <$> me (idx+1)
		return $ IA.listArray (0, n-1) as
	where
		emptyArray :: Array Int32 a
		emptyArray = IA.listArray (0, -1) []

parseCString :: Int -> Parsec BL.ByteString () String
parseCString n
	| n <= 0 = return []
	| otherwise = do
		cs <- (\me -> me 0 False) . fix $ \me idx isTerminated -> do
			if' (idx >= n) (return []) $ do
			b <- parseByte
			let c = asciiChar b
			if' (isTerminated || b == 0) (me (idx+1) True) $ do
			(c:) <$> me (idx+1) isTerminated
		let str = cs
		return $ str
	where
		asciiChar :: Word8 -> Char
		asciiChar = toEnum . fromEnum

levelFileParser :: Parsec BL.ByteString () LevelIB
levelFileParser = _
