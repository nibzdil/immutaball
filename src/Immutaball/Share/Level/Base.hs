{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Base.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, NondecreasingIndentation #-}

module Immutaball.Share.Level.Base
	(
		solPathMax,
		Mtrl(..), mtrlD, mtrlA, mtrlS, mtrlE, mtrlH, mtrlAngle, mtrlFl, mtrlF,
			mtrlAlphaFunc, mtrlAlphaRef,
		Vert(..), vertP,
		Edge(..), edgeVi, edgeVj,
		Side(..), edgeN, edgeD,
		Texc(..), texcU,
		Offs(..), offsTi, offsSi, offsVi,
		Geom(..), geomMi, geomOi, geomOj, geomOk,
		Lump(..), lumpFl, lumpV0, lumpVc, lumpE0, lumpEc, lumpG0, lumpGc,
			lumpS0, lumpSc,
		Sol(..), solAc, solMc, solVc, solEc, solSc, solTc, solOc, solGc, solLc,
			solNc, solPc, solBc, solHc, solZc, solJc, solXc, solRc, solUc,
			solWc, solDc, solIc, solAv, solMv, solVv, solEv, solSv, solTv,
			solOv, solGv, solLv, solNv, solPv, solBv, solHv, solZv, solJv,
			solXv, solRv, solUv, solWv, solDv, solIv,
		LevelIB,
		peeki32Native,
		peeki32BE,
		peeki32LE,
		peekf32dLE,
		peekn,
		peekCString,
		pokei32Native,
		pokei32BE,
		pokei32LE,
		pokef32dLE,
		poken,
		pokeCString,
		asType
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad
import Control.Monad.Fix
import Data.Bits
import Data.Coerce
--import Data.Function hiding (id, (.))
import Data.Int
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import Control.Lens
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Array
import Data.Array.IO
import Data.Array.IArray as IA
--import Data.Array.MArray as MA
--import Data.Array.Unboxed

import Immutaball.Share.Math
import Immutaball.Share.Utils

solPathMax :: Int
solPathMax = 64

data Mtrl = Mtrl {
	-- | Diffuse color.
	_mtrlD :: Vec4 Double,
	-- | Ambient color.
	_mtrlA :: Vec4 Double,
	-- | Specular color.
	_mtrlS :: Vec4 Double,
	-- | Emmission color.
	_mtrlE :: Vec4 Double,
	-- | Specular exponent.
	_mtrlH :: Double,

	_mtrlAngle :: Double,

	-- | Material flags.
	_mtrlFl :: Int32,

	-- | Texture file name.
	_mtrlF :: String,

	-- | Comparison function.
	_mtrlAlphaFunc :: Int32,
	_mtrlAlphaRef :: Double
}
makeLenses ''Mtrl

data Vert = Vert {
	-- | Vertex position.
	_vertP :: Vec3 Double
}
makeLenses ''Vert

data Edge = Edge {
	_edgeVi :: Int32,
	_edgeVj :: Int32
}
makeLenses ''Edge

data Side = Side {
	-- | Plane normal vector.
	_edgeN :: Vec3 Double,
	-- | Distance from origin.
	_edgeD :: Double
}
makeLenses ''Side

data Texc = Texc {
	-- | Texture coordinates.
	_texcU :: Vec2 Double
}
makeLenses ''Texc

data Offs = Offs {
	_offsTi :: Int32,
	_offsSi :: Int32,
	_offsVi :: Int32
}
makeLenses ''Offs

data Geom = Geom {
	_geomMi :: Int32,
	_geomOi :: Int32,
	_geomOj :: Int32,
	_geomOk :: Int32
}
makeLenses ''Geom

data Lump = Lump {
	-- | Lump flags.
	_lumpFl :: Int32,
	_lumpV0 :: Int32,
	_lumpVc :: Int32,
	_lumpE0 :: Int32,
	_lumpEc :: Int32,
	_lumpG0 :: Int32,
	_lumpGc :: Int32,
	_lumpS0 :: Int32,
	_lumpSc :: Int32
}
makeLenses ''Lump

-- TODO:
type Node = Int32
type Path = Int32
type Body = Int32
type Item = Int32
type Goal = Int32
type Jump = Int32
type Swch = Int32
type Bill = Int32
type Ball = Int32
type View = Int32
type Dict = Int32

-- | The level format: .sol.
--
-- Uses little-endian format.
--
-- Also encoded as floats, but we read into doubles.
data Sol = Sol {
	_solAc :: Int32,
	_solMc :: Int32,
	_solVc :: Int32,
	_solEc :: Int32,
	_solSc :: Int32,
	_solTc :: Int32,
	_solOc :: Int32,
	_solGc :: Int32,
	_solLc :: Int32,
	_solNc :: Int32,
	_solPc :: Int32,
	_solBc :: Int32,
	_solHc :: Int32,
	_solZc :: Int32,
	_solJc :: Int32,
	_solXc :: Int32,
	_solRc :: Int32,
	_solUc :: Int32,
	_solWc :: Int32,
	_solDc :: Int32,
	_solIc :: Int32,

	_solAv :: Array Int32 CChar,
	_solMv :: Array Int32 Mtrl,
	_solVv :: Array Int32 Vert,
	_solEv :: Array Int32 Edge,
	_solSv :: Array Int32 Side,
	_solTv :: Array Int32 Texc,
	_solOv :: Array Int32 Offs,
	_solGv :: Array Int32 Geom,
	_solLv :: Array Int32 Lump,
	_solNv :: Array Int32 Node,
	_solPv :: Array Int32 Path,
	_solBv :: Array Int32 Body,
	_solHv :: Array Int32 Item,
	_solZv :: Array Int32 Goal,
	_solJv :: Array Int32 Jump,
	_solXv :: Array Int32 Swch,
	_solRv :: Array Int32 Bill,
	_solUv :: Array Int32 Ball,
	_solWv :: Array Int32 View,
	_solDv :: Array Int32 Dict,
	_solIv :: Array Int32 Int32
}
makeLenses ''Sol

type LevelIB = Sol

instance Storable Sol where
	sizeOf
		(Sol
			ac mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc dc ic
			--av mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv dv iv
			_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
		) = sum $
			[
				sizeOf ac,
				sizeOf mc,
				sizeOf vc,
				sizeOf ec,
				sizeOf sc,
				sizeOf tc,
				sizeOf oc,
				sizeOf gc,
				sizeOf lc,
				sizeOf nc,
				sizeOf pc,
				sizeOf bc,
				sizeOf hc,
				sizeOf zc,
				sizeOf jc,
				sizeOf xc,
				sizeOf rc,
				sizeOf uc,
				sizeOf wc,
				sizeOf dc,
				sizeOf ic,

				fromIntegral ac * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: CChar),
				fromIntegral mc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Mtrl ),
				fromIntegral vc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Vert ),
				fromIntegral ec * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Edge ),
				fromIntegral sc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Side ),
				fromIntegral tc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Texc ),
				fromIntegral oc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Offs ),
				fromIntegral gc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Geom ),
				fromIntegral lc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Lump ),
				fromIntegral nc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Node ),
				fromIntegral pc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Path ),
				fromIntegral bc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Body ),
				fromIntegral hc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Item ),
				fromIntegral zc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Goal ),
				fromIntegral jc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Jump ),
				fromIntegral xc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Swch ),
				fromIntegral rc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Bill ),
				fromIntegral uc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Ball ),
				fromIntegral wc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: View ),
				fromIntegral dc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Dict ),
				fromIntegral ic * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Int32)
			]

	alignment
		(Sol
			ac mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc dc ic
			--av mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv dv iv
			_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
		) = max 1 . maximum $
			[
				alignment ac,
				alignment mc,
				alignment vc,
				alignment ec,
				alignment sc,
				alignment tc,
				alignment oc,
				alignment gc,
				alignment lc,
				alignment nc,
				alignment pc,
				alignment bc,
				alignment hc,
				alignment zc,
				alignment jc,
				alignment xc,
				alignment rc,
				alignment uc,
				alignment wc,
				alignment dc,
				alignment ic,

				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: CChar),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Mtrl ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Vert ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Edge ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Side ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Texc ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Offs ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Geom ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Lump ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Node ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Path ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Body ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Item ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Goal ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Jump ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Swch ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Bill ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Ball ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: View ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Dict ),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Int32)
			]

	peek ptr = mfix $ \sol -> flip evalStateT 0 $ Sol <$>
		peeki32LE ptr' <*>  -- ac
		peeki32LE ptr' <*>  -- mc
		peeki32LE ptr' <*>  -- vc
		peeki32LE ptr' <*>  -- ec
		peeki32LE ptr' <*>  -- sc
		peeki32LE ptr' <*>  -- tc
		peeki32LE ptr' <*>  -- oc
		peeki32LE ptr' <*>  -- gc
		peeki32LE ptr' <*>  -- lc
		peeki32LE ptr' <*>  -- nc
		peeki32LE ptr' <*>  -- pc
		peeki32LE ptr' <*>  -- bc
		peeki32LE ptr' <*>  -- hc
		peeki32LE ptr' <*>  -- zc
		peeki32LE ptr' <*>  -- jc
		peeki32LE ptr' <*>  -- xc
		peeki32LE ptr' <*>  -- rc
		peeki32LE ptr' <*>  -- uc
		peeki32LE ptr' <*>  -- wc
		peeki32LE ptr' <*>  -- dc
		peeki32LE ptr' <*>  -- ic

		peekn ptr' (sol^.solAc) <*>  -- av
		peekn ptr' (sol^.solMc) <*>  -- mv
		peekn ptr' (sol^.solVc) <*>  -- vv
		peekn ptr' (sol^.solEc) <*>  -- ev
		peekn ptr' (sol^.solSc) <*>  -- sv
		peekn ptr' (sol^.solTc) <*>  -- tv
		peekn ptr' (sol^.solOc) <*>  -- ov
		peekn ptr' (sol^.solGc) <*>  -- gv
		peekn ptr' (sol^.solLc) <*>  -- lv
		peekn ptr' (sol^.solNc) <*>  -- nv
		peekn ptr' (sol^.solPc) <*>  -- pv
		peekn ptr' (sol^.solBc) <*>  -- bv
		peekn ptr' (sol^.solHc) <*>  -- hv
		peekn ptr' (sol^.solZc) <*>  -- zv
		peekn ptr' (sol^.solJc) <*>  -- jv
		peekn ptr' (sol^.solXc) <*>  -- xv
		peekn ptr' (sol^.solRc) <*>  -- rv
		peekn ptr' (sol^.solUc) <*>  -- uv
		peekn ptr' (sol^.solWc) <*>  -- wv
		peekn ptr' (sol^.solDc) <*>  -- dv
		peekn ptr' (sol^.solIc)      -- iv

		where ptr' = castPtr ptr

	poke ptr
		(Sol
			ac mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc dc ic
			av mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv dv iv
		) = flip evalStateT 0 $ do
			pokei32LE ptr' ac
			pokei32LE ptr' mc
			pokei32LE ptr' vc
			pokei32LE ptr' ec
			pokei32LE ptr' sc
			pokei32LE ptr' tc
			pokei32LE ptr' oc
			pokei32LE ptr' gc
			pokei32LE ptr' lc
			pokei32LE ptr' nc
			pokei32LE ptr' pc
			pokei32LE ptr' bc
			pokei32LE ptr' hc
			pokei32LE ptr' zc
			pokei32LE ptr' jc
			pokei32LE ptr' xc
			pokei32LE ptr' rc
			pokei32LE ptr' uc
			pokei32LE ptr' wc
			pokei32LE ptr' dc
			pokei32LE ptr' ic

			poken ptr' ac av
			poken ptr' mc mv
			poken ptr' vc vv
			poken ptr' ec ev
			poken ptr' sc sv
			poken ptr' tc tv
			poken ptr' oc ov
			poken ptr' gc gv
			poken ptr' lc lv
			poken ptr' nc nv
			poken ptr' pc pv
			poken ptr' bc bv
			poken ptr' hc hv
			poken ptr' zc zv
			poken ptr' jc jv
			poken ptr' xc xv
			poken ptr' rc rv
			poken ptr' uc uv
			poken ptr' wc wv
			poken ptr' dc dv
			poken ptr' ic iv

		where ptr' = castPtr ptr

peeki32Native :: Ptr () -> StateT Int IO Int32
peeki32Native ptr = do
	offset <- get
	val    <- lift $ peek (castPtr ptr `plusPtr` offset)
	put $ offset + sizeOf val
	return val

-- | Read a big-endian encoded int.
peeki32BE :: Ptr () -> StateT Int IO Int32
peeki32BE ptr = do
	offset <- get
	(byte0 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 0))
	(byte1 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 1))
	(byte2 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 2))
	(byte3 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 3))
	let (w32 :: Word32) = ((fromIntegral byte3 `shiftL` 24) .|. (fromIntegral byte2 `shiftL` 16) .|. (fromIntegral byte1 `shiftL` 8) .|. (fromIntegral byte0 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	let val = i32
	put $ offset + sizeOf w32
	return val

-- | Read a little-endian encoded int, which has backwards byte order (least
-- significant first).
peeki32LE :: Ptr () -> StateT Int IO Int32
peeki32LE ptr = do
	offset <- get
	(byte3 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 0))
	(byte2 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 1))
	(byte1 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 2))
	(byte0 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 3))
	let (w32 :: Word32) = ((fromIntegral byte3 `shiftL` 24) .|. (fromIntegral byte2 `shiftL` 16) .|. (fromIntegral byte1 `shiftL` 8) .|. (fromIntegral byte0 `shiftL` 0))
	let (i32 :: Int32)  = fromIntegral w32
	let val = i32
	put $ offset + sizeOf w32
	return val

-- | Read a little-endian encoded float as a double.
peekf32dLE :: Ptr () -> StateT Int IO Double
peekf32dLE ptr = do
	offset <- get
	(byte3 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 0))
	(byte2 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 1))
	(byte1 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 2))
	(byte0 :: Word8) <- lift $ peek (castPtr ptr `plusPtr` (offset + 3))
	let (w32 :: Word32) = ((fromIntegral byte3 `shiftL` 24) .|. (fromIntegral byte2 `shiftL` 16) .|. (fromIntegral byte1 `shiftL` 8) .|. (fromIntegral byte0 `shiftL` 0))

	-- GHC doesn't support coerce between Word32 and Float.
	-- Just malloc a new cfloat.
	{-
	let (f32 :: Float)  = coerce w32
	-}
	(cfloat :: ForeignPtr CFloat) <- lift $ mallocForeignPtr
	(cf32 :: CFloat) <- lift . withForeignPtr cfloat $ \cfloatPtr -> poke (castPtr cfloatPtr) w32 >> peek cfloatPtr
	let (f32 :: Float)  = coerce cf32

	let (d   :: Double) = realToFrac f32
	let val = d
	put $ offset + sizeOf w32
	return val

peekn :: forall a. (Storable a) => Ptr () -> Int32 -> StateT Int IO (Array Int32 a)
peekn ptr n
	| n <= 0    = lift $ newArray_ (0, (-1)) >>= freeze'
	| otherwise = do
		offset <- get
		let elemSizeof = error "Internal error: peekn: sizeOf accessed its element!"  :: a
		let sizeofElem = sizeOf elemSizeof
		array_ <- lift . (>>= freeze') . newGenArray (0, n-1) $ \idx ->
			peek (castPtr ptr `plusPtr` (offset + fromIntegral idx * sizeofElem))
		put $ offset + fromIntegral n * sizeofElem
		let _ = asType elemSizeof (array_ IA.! 0)
		return array_
	where
		freeze' :: IOArray Int32 a -> IO (Array Int32 a)
		freeze' = freeze

peekCString :: Ptr () -> Int -> StateT Int IO String
peekCString ptr' bufSize = do
	offset <- get
	str <- flip fix 0 $ \withOffset relOffset -> do
		if' (offset >= bufSize) (return []) $ do
		(c :: Word8) <- lift $ peek (castPtr ptr' `plusPtr` (offset + relOffset))
		if' (c == 0) (return []) $ do
		(asciiChar c:) <$> withOffset (relOffset+1)
	put $ offset + bufSize
	return str
	where
		asciiChar :: Word8 -> Char
		asciiChar = toEnum . fromEnum

asType :: a -> a -> ()
asType _ _ = ()

pokei32Native :: Ptr () -> Int32 -> StateT Int IO ()
pokei32Native ptr val = do
	offset <- get
	lift $ poke (castPtr ptr `plusPtr` offset) val
	put $ offset + sizeOf val

pokei32BE :: Ptr () -> Int32 -> StateT Int IO ()
pokei32BE ptr val = do
	offset <- get
	let (w32 :: Word32) = fromIntegral val
	let (byte0 :: Word8) = fromIntegral $ (w32 .&. 0xFF000000) `shiftR` 24
	let (byte1 :: Word8) = fromIntegral $ (w32 .&. 0x00FF0000) `shiftR` 16
	let (byte2 :: Word8) = fromIntegral $ (w32 .&. 0x0000FF00) `shiftR`  8
	let (byte3 :: Word8) = fromIntegral $ (w32 .&. 0x000000FF) `shiftR`  0
	lift $ poke (castPtr ptr `plusPtr` (offset + 0)) byte0
	lift $ poke (castPtr ptr `plusPtr` (offset + 1)) byte1
	lift $ poke (castPtr ptr `plusPtr` (offset + 2)) byte2
	lift $ poke (castPtr ptr `plusPtr` (offset + 3)) byte3
	put $ offset + sizeOf w32

pokei32LE :: Ptr () -> Int32 -> StateT Int IO ()
pokei32LE ptr val = do
	offset <- get
	let (w32 :: Word32) = fromIntegral val
	let (byte3 :: Word8) = fromIntegral $ (w32 .&. 0xFF000000) `shiftR` 24
	let (byte2 :: Word8) = fromIntegral $ (w32 .&. 0x00FF0000) `shiftR` 16
	let (byte1 :: Word8) = fromIntegral $ (w32 .&. 0x0000FF00) `shiftR`  8
	let (byte0 :: Word8) = fromIntegral $ (w32 .&. 0x000000FF) `shiftR`  0
	lift $ poke (castPtr ptr `plusPtr` (offset + 0)) byte0
	lift $ poke (castPtr ptr `plusPtr` (offset + 1)) byte1
	lift $ poke (castPtr ptr `plusPtr` (offset + 2)) byte2
	lift $ poke (castPtr ptr `plusPtr` (offset + 3)) byte3
	put $ offset + sizeOf w32

pokef32dLE :: Ptr () -> Double -> StateT Int IO ()
pokef32dLE ptr val = do
	offset <- get
	let (fl :: Float) = realToFrac val

	-- GHC doesn't support coerce between Float and Word32.
	-- Just malloc a new word32.
	{-
	let (w32 :: Word32) = coerce f
	-}
	(w32Ptr :: ForeignPtr Word32) <- lift $ mallocForeignPtr
	(cw32 :: Word32) <- lift . withForeignPtr w32Ptr $ \ptr_ -> poke (castPtr ptr_) fl >> peek ptr_
	let (w32 :: Word32) = coerce cw32

	let (byte3 :: Word8) = fromIntegral $ (w32 .&. 0xFF000000) `shiftR` 24
	let (byte2 :: Word8) = fromIntegral $ (w32 .&. 0x00FF0000) `shiftR` 16
	let (byte1 :: Word8) = fromIntegral $ (w32 .&. 0x0000FF00) `shiftR`  8
	let (byte0 :: Word8) = fromIntegral $ (w32 .&. 0x000000FF) `shiftR`  0
	lift $ poke (castPtr ptr `plusPtr` (offset + 0)) byte0
	lift $ poke (castPtr ptr `plusPtr` (offset + 1)) byte1
	lift $ poke (castPtr ptr `plusPtr` (offset + 2)) byte2
	lift $ poke (castPtr ptr `plusPtr` (offset + 3)) byte3
	put $ offset + sizeOf w32

poken :: forall a. (Storable a) => Ptr () -> Int32 -> Array Int32 a -> StateT Int IO ()
poken ptr n array_
	| n <= 0    = return ()
	| otherwise = do
		let elemSizeof = error "Internal error: poken: sizeOf accessed its element!"  :: a
		let sizeofElem = sizeOf elemSizeof
		forM_ array_ $ \elem_ -> do
			offset <- get
			lift $ poke (castPtr ptr `plusPtr` offset) elem_
			put $ offset + sizeofElem

pokeCString :: Ptr () -> Int -> String -> StateT Int IO ()
pokeCString ptr bufSize str
	| bufSize <= 0 = return ()
	| otherwise = do
		offset <- get
		let strLen = max (bufSize - 1) $ length str
		forM_ (zip [0..] (take strLen $ str)) $ \(idx, c) -> do
			lift $ poke (castPtr ptr `plusPtr` (offset + idx)) $ truncateChar c
		lift $ poke (castPtr ptr `plusPtr` (offset + (strLen - 1))) (0x00 :: Word8)
		put $ offset + bufSize
	where
		truncateChar :: Char -> Word8
		truncateChar = toEnum . fromEnum

instance Storable Mtrl where
	sizeOf
		(Mtrl
			d a s e h angle fl _f alphaFunc alphaRef
		) = sum $
			[
				sizeOf (d^._1) * 4,
				sizeOf (a^._1) * 4,
				sizeOf (s^._1) * 4,
				sizeOf (e^._1) * 4,
				sizeOf h,
				sizeOf angle,
				sizeOf fl,
				solPathMax,
				sizeOf alphaFunc,
				sizeOf alphaRef
			]

	alignment
		(Mtrl
			d a s e h angle fl _f alphaFunc alphaRef
		) = max 1 . maximum $
			[
				alignment (d^._1),
				alignment (a^._1),
				alignment (s^._1),
				alignment (e^._1),
				alignment h,
				alignment angle,
				alignment fl,
				1,
				alignment alphaFunc,
				alignment alphaRef
			]

	peek ptr = flip evalStateT 0 $ Mtrl <$>
		(Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- mtrlD
		(Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- mtrlA
		(Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- mtrlS
		(Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- mtrlE
		peekf32dLE ptr' <*>  -- mtrlH

		peekf32dLE ptr' <*>  -- mtrlAngle

		peeki32LE ptr' <*>  -- mtrlFl

		peekCString ptr' solPathMax <*>  -- mtrlF

		peeki32LE ptr' <*>  -- mtrlAlphaFunc
		peekf32dLE ptr'  -- mtrlAlphaRef

		where ptr' = castPtr ptr

	poke ptr
		(Mtrl
			d a s e h angle fl f alphaFunc alphaRef
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' (d^.x4) >> pokef32dLE ptr' (d^.y4) >> pokef32dLE ptr' (d^.z4) >> pokef32dLE ptr' (d^.w4)
			pokef32dLE ptr' (a^.x4) >> pokef32dLE ptr' (a^.y4) >> pokef32dLE ptr' (a^.z4) >> pokef32dLE ptr' (a^.w4)
			pokef32dLE ptr' (s^.x4) >> pokef32dLE ptr' (s^.y4) >> pokef32dLE ptr' (s^.z4) >> pokef32dLE ptr' (s^.w4)
			pokef32dLE ptr' (e^.x4) >> pokef32dLE ptr' (e^.y4) >> pokef32dLE ptr' (e^.z4) >> pokef32dLE ptr' (e^.w4)
			pokef32dLE ptr' h

			pokef32dLE ptr' angle

			pokei32LE ptr' fl

			pokeCString ptr' solPathMax f

			pokei32LE ptr' alphaFunc
			pokef32dLE ptr' alphaRef

		where ptr' = castPtr ptr

instance Storable Vert where
	sizeOf    (Vert (Vec3 _ _ _)) = sum [3 * sizeOf x']
		where x' = error "Internal error: sizeOf Vert: sizeOf accessed its argument!" :: Float
	alignment (Vert (Vec3 _ _ _)) = max 1 $ maximum [alignment x']
		where x' = error "Internal error: alignment Vert: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Vert <$> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr')
		where ptr' = castPtr ptr
	poke ptr (Vert (Vec3 x y z)) = flip evalStateT 0 $ pokef32dLE ptr' x >> pokef32dLE ptr' y >> pokef32dLE ptr' z
		where ptr' = castPtr ptr

instance Storable Edge where
	sizeOf    (Edge vi vj) = sum [sizeOf vi, sizeOf vj]
	alignment (Edge vi vj) = max 1 $ maximum [alignment vi, alignment vj]
	peek ptr = flip evalStateT 0 $ Edge <$> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Edge vi vj) = flip evalStateT 0 $ pokei32LE ptr' vi >> pokei32LE ptr' vj
		where ptr' = castPtr ptr

instance Storable Side where
	sizeOf    (Side (Vec3 _nx _ny _nz) _d) = sum [3 * sizeOf x', sizeOf x']
		where x' = error "Internal error: sizeOf Side: sizeOf accessed its argument!" :: Float
	alignment (Side (Vec3 _nx _ny _nz) _d) = max 1 $ maximum [alignment x', alignment x']
		where x' = error "Internal error: alignment Side: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Side <$> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*> peekf32dLE ptr'
		where ptr' = castPtr ptr
	poke ptr (Side (Vec3 nx ny nz) d) = flip evalStateT 0 $ pokef32dLE ptr' nx >> pokef32dLE ptr' ny >> pokef32dLE ptr' nz >> pokef32dLE ptr' d
		where ptr' = castPtr ptr

instance Storable Texc where
	sizeOf    (Texc (Vec2 _tx _ty)) = sum [3 * sizeOf x', sizeOf x']
		where x' = error "Internal error: sizeOf Texc: sizeOf accessed its argument!" :: Float
	alignment (Texc (Vec2 _tx _ty)) = max 1 $ maximum [alignment x', alignment x']
		where x' = error "Internal error: alignment Texc: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Texc <$> (Vec2 <$> peekf32dLE ptr' <*> peekf32dLE ptr')
		where ptr' = castPtr ptr
	poke ptr (Texc (Vec2 tx ty)) = flip evalStateT 0 $ pokef32dLE ptr' tx >> pokef32dLE ptr' ty
		where ptr' = castPtr ptr

instance Storable Offs where
	sizeOf    (Offs ti si vi) = sum [sizeOf ti, sizeOf si, sizeOf vi]
	alignment (Offs ti si vi) = max 1 $ maximum [alignment ti, alignment si, alignment vi]
	peek ptr = flip evalStateT 0 $ Offs <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Offs ti si vi) = flip evalStateT 0 $ pokei32LE ptr' ti >> pokei32LE ptr' si >> pokei32Native ptr' vi
		where ptr' = castPtr ptr

instance Storable Geom where
	sizeOf    (Geom mi oi oj ok) = sum [sizeOf mi, sizeOf oi, sizeOf oj, sizeOf ok]
	alignment (Geom mi oi oj ok) = max 1 $ maximum [alignment mi, alignment oi, alignment oj, alignment ok]
	peek ptr = flip evalStateT 0 $ Geom <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Geom mi oi oj ok) = flip evalStateT 0 $ pokei32LE ptr' mi >> pokei32LE ptr' oi >> pokei32Native ptr' oj >> pokei32LE ptr' ok
		where ptr' = castPtr ptr

instance Storable Lump where
	sizeOf    (Lump fl v0 vc e0 ec g0 gc s0 sc) = sum [sizeOf fl, sizeOf v0, sizeOf vc, sizeOf e0, sizeOf ec, sizeOf g0, sizeOf gc, sizeOf s0, sizeOf sc]
	alignment (Lump fl v0 vc e0 ec g0 gc s0 sc) = max 1 $ maximum [alignment fl, alignment v0, alignment vc, alignment e0, alignment ec, alignment g0, alignment gc, alignment s0, alignment sc]
	peek ptr = flip evalStateT 0 $ Lump <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Lump fl v0 vc e0 ec g0 gc s0 sc) = flip evalStateT 0 $ pokei32LE ptr' fl >> pokei32LE ptr' v0 >> pokei32LE ptr' vc >> pokei32LE ptr' e0 >> pokei32LE ptr' ec >> pokei32LE ptr' g0 >> pokei32LE ptr' gc >> pokei32LE ptr' s0 >> pokei32LE ptr' sc
		where ptr' = castPtr ptr
