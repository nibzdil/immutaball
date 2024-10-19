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
		levelFileParser',
		parseByte,
		parsei32Native,
		parsei32BE,
		parsei32LE,
		parsef32dLE,
		parsen,
		parseCString,
		parseVec2fd,
		parseVec3fd,
		parseVec4fd,

		parseDict,
		parseMtrl,
		parseVert,
		parseEdge,
		parseSide,
		parseTexc,
		parseOffs,
		parseGeom,
		parseLump,
		parseNode,
		parsePath,
		parseBody,
		parseItem,
		parseGoal,
		parseJump,
		parseSwch,
		parseBill,
		parseBall,
		parseView,

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
		ParseErrorLevelIBParseException(..)
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad
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
import Text.Parsec as P

import Immutaball.Share.Level.Base
import Immutaball.Share.Math
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
--parseLevelFile inputName inputContents = LevelIBParseException . ParseErrorLevelIBParseException +++ id $ parse levelFileParser inputName inputContents
parseLevelFile inputName inputContents = (\r -> flip D.trace r $ printf "DEBUG parseLevelFile: %s" (show r)) . LevelIBParseException . ParseErrorLevelIBParseException +++ id $ parse levelFileParser inputName inputContents

parseByte :: Parsec BL.ByteString () Word8
parseByte = truncateAsciiChar <$> anyChar
	& P.try <?> "parseByte expected a w8"
	where
		truncateAsciiChar :: Char -> Word8
		truncateAsciiChar = toEnum . fromEnum

parsei32Native :: Parsec BL.ByteString () Int32
parsei32Native = (<?> "parsei32Native expected an i32") . P.try $ do
	byte0_ <- parseByte
	byte1_ <- parseByte
	byte2_ <- parseByte
	byte3_ <- parseByte
	let (byte0, byte1, byte2, byte3) = if' (Raw.SDL_BYTEORDER == Raw.SDL_BIG_ENDIAN) (byte0_, byte1_, byte2_, byte3_) (byte3_, byte2_, byte1_, byte0_)
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	return $ i32

parsei32BE :: Parsec BL.ByteString () Int32
parsei32BE = (<?> "parsei32BE expected an i32") . P.try $ do
	byte0 <- parseByte
	byte1 <- parseByte
	byte2 <- parseByte
	byte3 <- parseByte
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	return $ i32

parsei32LE :: Parsec BL.ByteString () Int32
parsei32LE = (<?> "parsei32LE expected an i32") . P.try $ do
	byte3 <- parseByte
	byte2 <- parseByte
	byte1 <- parseByte
	byte0 <- parseByte
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	return $ i32

parsef32dLE :: Parsec BL.ByteString () Double
parsef32dLE = (<?> "parsef32dLE expected an f32") . P.try $ do
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
	| otherwise = (<?> (printf "parsen _ %d expected an array" n)) . P.try $ do
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
	| otherwise = (<?> (printf "parseCString %d expected a CString" n)) . P.try $ do
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

parseVec2fd :: Parsec BL.ByteString () (Vec2 Double)
parseVec2fd = Vec2 <$> parsef32dLE <*> parsef32dLE
	& P.try <?> "parseVec2fd expected a Vec2f"

parseVec3fd :: Parsec BL.ByteString () (Vec3 Double)
parseVec3fd = Vec3 <$> parsef32dLE <*> parsef32dLE <*> parsef32dLE
	& P.try <?> "parseVec3fd expected a Vec3f"

parseVec4fd :: Parsec BL.ByteString () (Vec4 Double)
parseVec4fd = Vec4 <$> parsef32dLE <*> parsef32dLE <*> parsef32dLE <*> parsef32dLE
	& P.try <?> "parseVec4fd expected a Vec4f"

levelFileParser :: Parsec BL.ByteString () LevelIB
levelFileParser = levelFileParser' True

levelFileParser' :: Bool -> Parsec BL.ByteString () LevelIB
levelFileParser' isEof = (<?> "levelFileParser expected a sol") . P.try $ do
	magic   <- parsei32LE & P.try <?> "levelFileParser expected magic"
	when (magic   /= solMagicConstant) . unexpected $ printf "Error: levelFileParser: expected the 4 SOL starter bytes; found 0x%04X, /= 0x%04X" magic solMagicConstant
	version <- parsei32LE & P.try <?> "levelFileParser expected version"
	when (version /= solVersionCurr  ) . unexpected $ printf "Error: levelFileParser: unsupported SOL version; found %d, /= %d" version solVersionCurr

	ac <- parsei32LE & P.try <?> "levelFileParser expected ac"
	dc <- parsei32LE & P.try <?> "levelFileParser expected dc"
	mc <- parsei32LE & P.try <?> "levelFileParser expected mc"
	vc <- parsei32LE & P.try <?> "levelFileParser expected vc"
	ec <- parsei32LE & P.try <?> "levelFileParser expected ec"
	sc <- parsei32LE & P.try <?> "levelFileParser expected sc"
	tc <- parsei32LE & P.try <?> "levelFileParser expected tc"
	oc <- parsei32LE & P.try <?> "levelFileParser expected oc"
	gc <- parsei32LE & P.try <?> "levelFileParser expected gc"
	lc <- parsei32LE & P.try <?> "levelFileParser expected lc"
	nc <- parsei32LE & P.try <?> "levelFileParser expected nc"
	pc <- parsei32LE & P.try <?> "levelFileParser expected pc"
	bc <- parsei32LE & P.try <?> "levelFileParser expected bc"
	hc <- parsei32LE & P.try <?> "levelFileParser expected hc"
	zc <- parsei32LE & P.try <?> "levelFileParser expected zc"
	jc <- parsei32LE & P.try <?> "levelFileParser expected jc"
	xc <- parsei32LE & P.try <?> "levelFileParser expected xc"
	rc <- parsei32LE & P.try <?> "levelFileParser expected rc"
	uc <- parsei32LE & P.try <?> "levelFileParser expected uc"
	wc <- parsei32LE & P.try <?> "levelFileParser expected wc"
	ic <- parsei32LE & P.try <?> "levelFileParser expected ic"

	av <- parsen parseByte  ac & (fmap (coerce . (fromIntegral :: Word8 -> Int8)) <$>) & P.try <?> "levelFileParser expected av"
	dv <- parsen parseDict  dc & P.try <?> "levelFileParser expected dv"
	mv <- parsen parseMtrl  mc & P.try <?> "levelFileParser expected mv"
	vv <- parsen parseVert  vc & P.try <?> "levelFileParser expected vv"
	ev <- parsen parseEdge  ec & P.try <?> "levelFileParser expected ev"
	sv <- parsen parseSide  sc & P.try <?> "levelFileParser expected sv"
	tv <- parsen parseTexc  tc & P.try <?> "levelFileParser expected tv"
	ov <- parsen parseOffs  oc & P.try <?> "levelFileParser expected ov"
	gv <- parsen parseGeom  gc & P.try <?> "levelFileParser expected gv"
	lv <- parsen parseLump  lc & P.try <?> "levelFileParser expected lv"
	nv <- parsen parseNode  nc & P.try <?> "levelFileParser expected nv"
	pv <- parsen parsePath  pc & P.try <?> "levelFileParser expected pv"
	bv <- parsen parseBody  bc & P.try <?> "levelFileParser expected bv"
	hv <- parsen parseItem  hc & P.try <?> "levelFileParser expected hv"
	zv <- parsen parseGoal  zc & P.try <?> "levelFileParser expected zv"
	jv <- parsen parseJump  jc & P.try <?> "levelFileParser expected jv"
	xv <- parsen parseSwch  xc & P.try <?> "levelFileParser expected xv"
	rv <- parsen parseBill  rc & P.try <?> "levelFileParser expected rv"
	uv <- parsen parseBall  uc & P.try <?> "levelFileParser expected uv"
	wv <- parsen parseView  wc & P.try <?> "levelFileParser expected wv"
	--parserTrace $ printf "DEBUG5: %s" (show (ac, dc, mc, mc, vc, ec, (sc, tc, oc, gc, lc, nc, pc, (bc, hc, zc, jc, xc, rc, uc, wc, ic))))
	parserTrace $ printf "DEBUG5: %s" (show (mv))
	iv <- parsen parsei32LE ic & P.try <?> "levelFileParser expected iv"
	--parserTrace "DEBUG6"

	if' isEof eof (pure ()) & P.try <?> "levelFileParser expected end of input."

	return $ Sol {
		_solMagic   = magic,
		_solVersion = version,

		_solAc = ac,
		_solDc = dc,
		_solMc = mc,
		_solVc = vc,
		_solEc = ec,
		_solSc = sc,
		_solTc = tc,
		_solOc = oc,
		_solGc = gc,
		_solLc = lc,
		_solNc = nc,
		_solPc = pc,
		_solBc = bc,
		_solHc = hc,
		_solZc = zc,
		_solJc = jc,
		_solXc = xc,
		_solRc = rc,
		_solUc = uc,
		_solWc = wc,
		_solIc = ic,

		_solAv = av,
		_solDv = dv,
		_solMv = mv,
		_solVv = vv,
		_solEv = ev,
		_solSv = sv,
		_solTv = tv,
		_solOv = ov,
		_solGv = gv,
		_solLv = lv,
		_solNv = nv,
		_solPv = pv,
		_solBv = bv,
		_solHv = hv,
		_solZv = zv,
		_solJv = jv,
		_solXv = xv,
		_solRv = rv,
		_solUv = uv,
		_solWv = wv,
		_solIv = iv
	}

parseDict :: Parsec BL.ByteString () Dict
parseDict = Dict <$> parsei32LE <*> parsei32LE
	& P.try <?> "parseDict expected a Dict"

parseMtrl :: Parsec BL.ByteString () Mtrl
parseMtrl = (<?> "parsePath expected a Path") . P.try $ do
	d <- parseVec4fd
	a <- parseVec4fd
	s <- parseVec4fd
	e <- parseVec4fd
	h <- parsef32dLE

	let angle = 0.0

	fl <- parsei32LE

	f <- parseCString solPathMax

	alphaFunc <- if' ((fl .&. mtrlFlagAlphaTest) /= 0) parsei32LE  (pure 0  )
	alphaRef  <- if' ((fl .&. mtrlFlagAlphaTest) /= 0) parsef32dLE (pure 0.0)

	return $ Mtrl {
		_mtrlD = d,
		_mtrlA = a,
		_mtrlS = s,
		_mtrlE = e,
		_mtrlH = h,

		_mtrlAngle = angle,

		_mtrlFl = fl,
		_mtrlF  = f,

		_mtrlAlphaFunc = alphaFunc,
		_mtrlAlphaRef  = alphaRef
	}

parseVert :: Parsec BL.ByteString () Vert
parseVert = Vert <$> parseVec3fd
	& P.try <?> "parseVert expected a Vert"

parseEdge :: Parsec BL.ByteString () Edge
parseEdge = Edge <$> parsei32LE <*> parsei32LE
	& P.try <?> "parseEdge expected an Edge"

parseSide :: Parsec BL.ByteString () Side
parseSide = Side <$> parseVec3fd <*> parsef32dLE
	& P.try <?> "parseSide expected a Side"

parseTexc :: Parsec BL.ByteString () Texc
parseTexc = Texc <$> parseVec2fd
	& P.try <?> "parseTexc expected a Texc"

parseOffs :: Parsec BL.ByteString () Offs
parseOffs = Offs <$> parsei32LE <*> parsei32LE <*> parsei32LE
	& P.try <?> "parseOffs expected an Offs"

parseGeom :: Parsec BL.ByteString () Geom
parseGeom = Geom <$> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE
	& P.try <?> "parseGeom expected a Geom"

parseLump :: Parsec BL.ByteString () Lump
parseLump = Lump <$> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE
	& P.try <?> "parseLump expected a Lump"

parseNode :: Parsec BL.ByteString () Node
parseNode = Node <$> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE
	& P.try <?> "parseNode expected a Node"

-- Irregular encoding.
parsePath :: Parsec BL.ByteString () Path
parsePath = (<?> "parsePath expected a Path") . P.try $ do
	p   <- parseVec3fd
	t   <- parsef32dLE
	pi_ <- parsei32LE
	f   <- parsei32LE
	s   <- parsei32LE
	fl  <- parsei32LE

	e        <- if' ((fl .&. pathFlagOriented) /= 0) parseVec4fd (return $ Vec4 0.0 0.0 0.0 0.0)
	(p0, p1) <- if' ((fl .&. pathFlagParented) /= 0) ((,) <$> parsei32LE <*> parsei32LE) (return $ (0, 0))

	let tm = round $ (1000.0*t)

	let p1' = if' (p1 < 0) p0 p1

	return $ Path {
		_pathP  = p,
		_pathE  = e,
		_pathT  = t,
		_pathTm = tm,

		_pathPi = pi_,
		_pathF  = f,
		_pathS  = s,

		_pathFl = fl,

		_pathP0 = p0,
		_pathP1 = p1'
	}

parseBody :: Parsec BL.ByteString () Body
parseBody = Body <$> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE
	& P.try <?> "parseBody expected a Body"

parseItem :: Parsec BL.ByteString () Item
parseItem = (((\item -> item & (itemP1 .~ (if' (item^.itemP1 < 0) (item^.itemP0) (item^.itemP1)))) <$>) $ Item <$>
	parseVec3fd <*> parsei32LE <*> parsei32LE <*> parsei32LE <*> parsei32LE)
	& P.try <?> "parseItem expected an Item"

parseGoal :: Parsec BL.ByteString () Goal
parseGoal = (((\goal -> goal & (goalP1 .~ (if' (goal^.goalP1 < 0) (goal^.goalP0) (goal^.goalP1)))) <$>) $ Goal <$>
	parseVec3fd <*> parsef32dLE <*> parsei32LE <*> parsei32LE)
	& P.try <?> "parseGoal expected a Goal"

parseJump :: Parsec BL.ByteString () Jump
parseJump = (((\jump -> jump & (jumpP1 .~ (if' (jump^.jumpP1 < 0) (jump^.jumpP0) (jump^.jumpP1)))) <$>) $ Jump <$>
	parseVec3fd <*> parseVec3fd <*> parsef32dLE <*> parsei32LE <*> parsei32LE)
	& P.try <?> "parseJump expected a Jump"

-- Irregular encoding.
parseSwch :: Parsec BL.ByteString () Swch
parseSwch = (<?> "parseSwch expected a Swch") . P.try $ do
	p   <- parseVec3fd
	r   <- parsef32dLE
	pi_ <- parsei32LE

	t      <- parsef32dLE
	_skip0 <- parsef32dLE
	let tm = round $ 1000.0*t
	f      <- parsei32LE
	_skip1 <- parsei32LE
	i      <- parsei32LE

	p0 <- parsei32LE
	p1 <- parsei32LE

	let p1' = if' (p1 < 0) p0 p1

	return $ Swch {
		_swchP  = p,
		_swchR  = r,
		_swchPi = pi_,

		_swchT  = t,
		_swchTm = tm,
		_swchF  = f,
		_swchI  = i,

		_swchP0 = p0,
		_swchP1 = p1'
	}

parseBill :: Parsec BL.ByteString () Bill
parseBill = (((\bill -> bill & (billP1 .~ (if' (bill^.billP1 < 0) (bill^.billP0) (bill^.billP1)))) <$>) $ Bill <$>
	parsei32LE <*> parsei32LE <*> parsef32dLE <*> parsef32dLE <*>
	parseVec3fd <*> parseVec3fd <*>
	parseVec3fd <*> parseVec3fd <*> parseVec3fd <*>
	parseVec3fd <*>
	parsei32LE <*> parsei32LE)
	& P.try <?> "parseBill expected a Bill"

parseBall :: Parsec BL.ByteString () Ball
parseBall = Ball <$> parseVec3fd <*> parsef32dLE
	& P.try <?> "parseBall expected a Ball"

parseView :: Parsec BL.ByteString () View
parseView = View <$> parseVec3fd <*> parseVec3fd
	& P.try <?> "parseView expected a View"
