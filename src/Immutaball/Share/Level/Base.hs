{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Base.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, NondecreasingIndentation #-}

module Immutaball.Share.Level.Base
	(
		-- * sol
		solPathMax,
		solMagicConstant,
		solVersionCurr,
		solVersion2024_04,
		pathFlagOriented,
		pathFlagParented,
		mtrlFlagAlphaTest,
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
		Node(..), nodeSi, nodeNi, nodeNj, nodeL0, nodeLc,
		Path(..), pathP, pathE, pathT, pathTm, pathPi, pathF, pathS, pathFl,
			pathP0, pathP1,
		Body(..), bodyP0, bodyP1, bodyNi, bodyL0, bodyLc, bodyG0, bodyGc,
		Item(..), itemP, itemT, itemN, itemP0, itemP1,
		Goal(..), goalP, goalR, goalP0, goalP1,
		Jump(..), jumpP, jumpQ, jumpR, jumpP0, jumpP1,
		Swch(..), swchP, swchR, swchPi, swchT, swchTm, swchF, swchI, swchP0, swchP1,
		Bill(..), billFl, billMi, billT, billD, billW, billH, billRx, billRy,
			billRz, billP, billP0, billP1,
		Ball(..), ballP, ballR,
		View(..), viewP, viewQ,
		Dict(..), dictAi, dictAj,
		Sol(..), solMagic, solVersion, solAc, solDc, solMc, solVc, solEc,
			solSc, solTc, solOc, solGc, solLc, solNc, solPc, solBc, solHc,
			solZc, solJc, solXc, solRc, solUc, solWc, solIc, solAv, solDv,
			solMv, solVv, solEv, solSv, solTv, solOv, solGv, solLv, solNv,
			solPv, solBv, solHv, solZv, solJv, solXv, solRv, solUv, solWv,
			solIv,
		LevelIB,
		emptySol,

		-- * Optional low-level storable provisions.
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
		asType,
		sizeOfMtrl,
		sizeOfMtrlMin,
		sizeOfMtrlMax,
		sizeOfExistingMtrl,
		sizeOfPath,
		sizeOfPathMin,
		sizeOfPathMax,
		sizeOfExistingPath,
		sizeOfEmptySol,
		sizeOfExistingSolMin,
		sizeOfExistingSolMax,
		sizeOfExistingSol,
		peekSol,
		peekSolLengths,
		pokeSol
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

-- * sol

solPathMax :: Int
solPathMax = 64

solMagicConstant :: Int32
solMagicConstant = 0x4C4F53AF  -- LE encoding of AF 'S' 'O' 'L'.

solVersionCurr :: Int32
solVersionCurr = solVersion2024_04

solVersion2024_04 :: Int32
solVersion2024_04 = 9

pathFlagOriented :: Int32
pathFlagOriented = 1

pathFlagParented :: Int32
pathFlagParented = 2

mtrlFlagAlphaTest :: Int32
mtrlFlagAlphaTest = 1 `shiftL` 9

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
	deriving (Eq, Ord, Show)
makeLenses ''Mtrl

data Vert = Vert {
	-- | Vertex position.
	_vertP :: Vec3 Double
}
	deriving (Eq, Ord, Show)
makeLenses ''Vert

data Edge = Edge {
	_edgeVi :: Int32,
	_edgeVj :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Edge

data Side = Side {
	-- | Plane normal vector.
	_edgeN :: Vec3 Double,
	-- | Distance from origin.
	_edgeD :: Double
}
	deriving (Eq, Ord, Show)
makeLenses ''Side

data Texc = Texc {
	-- | Texture coordinates.
	_texcU :: Vec2 Double
}
	deriving (Eq, Ord, Show)
makeLenses ''Texc

data Offs = Offs {
	_offsTi :: Int32,
	_offsSi :: Int32,
	_offsVi :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Offs

data Geom = Geom {
	_geomMi :: Int32,
	_geomOi :: Int32,
	_geomOj :: Int32,
	_geomOk :: Int32
}
	deriving (Eq, Ord, Show)
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
	deriving (Eq, Ord, Show)
makeLenses ''Lump

data Node = Node {
	_nodeSi :: Int32,
	_nodeNi :: Int32,
	_nodeNj :: Int32,
	_nodeL0 :: Int32,
	_nodeLc :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Node

data Path = Path {
	-- | Starting position.
	_pathP :: Vec3 Double,
	-- | Orientation (quaternion).
	_pathE :: Vec4 Double,
	-- | Travel time.
	_pathT :: Double,
	-- | Milliseconds.
	_pathTm :: Int32,

	-- | Next path.
	_pathPi :: Int32,
	-- | Enable flag.
	_pathF :: Int32,
	-- | Smooth flag.
	_pathS :: Int32,

	-- | Flags.
	_pathFl :: Int32,

	_pathP0 :: Int32,
	_pathP1 :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Path

data Body = Body {
	-- | Translation path.
	_bodyP0 :: Int32,
	-- | Rotation path.
	_bodyP1 :: Int32,

	_bodyNi :: Int32,
	_bodyL0 :: Int32,
	_bodyLc :: Int32,
	_bodyG0 :: Int32,
	_bodyGc :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Body

data Item = Item {
	-- | Position.
	_itemP :: Vec3 Double,
	-- | Type.
	_itemT :: Int32,
	-- | Value.
	_itemN :: Int32,

	-- | Translation path.
	_itemP0 :: Int32,
	-- | Rotation path.
	_itemP1 :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Item

data Goal = Goal {
	-- | Position.
	_goalP :: Vec3 Double,
	-- | Radius.
	_goalR :: Double,

	-- | Translation path.
	_goalP0 :: Int32,
	-- | Rotation path.
	_goalP1 :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Goal

data Jump = Jump {
	-- | Position.
	_jumpP :: Vec3 Double,
	-- | Target position.
	_jumpQ :: Vec3 Double,
	-- | Radius.
	_jumpR :: Double,

	-- | Translation path.
	_jumpP0 :: Int32,
	-- | Rotation path.
	_jumpP1 :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Jump

data Swch = Swch {
	-- | Position.
	_swchP :: Vec3 Double,
	-- | Radius.
	_swchR :: Double,
	-- | The activated path.
	_swchPi :: Int32,

	-- | Default timer.
	_swchT :: Double,
	-- | Milliseconds.
	_swchTm :: Int32,
	-- | Default state.
	_swchF :: Int32,
	-- | Is invisible?
	_swchI :: Int32,

	-- | Translation path.
	_swchP0 :: Int32,
	-- | Rotation path.
	_swchP1 :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Swch

data Bill = Bill {
	_billFl :: Int32,
	_billMi :: Int32,
	-- | Repeat time interval.
	_billT :: Double,
	-- | Distance.
	_billD :: Double,

	-- | Width coefficients.
	_billW :: Vec3 Double,
	-- | Height coefficients.
	_billH :: Vec3 Double,

	-- | X rotation coefficients.
	_billRx :: Vec3 Double,
	-- | Y rotation coefficients.
	_billRy :: Vec3 Double,
	-- | Z rotation coefficients.
	_billRz :: Vec3 Double,

	_billP :: Vec3 Double,

	-- | Translation path.
	_billP0 :: Int32,
	-- | Rotation path.
	_billP1 :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Bill

data Ball = Ball {
	_ballP :: Vec3 Double,
	_ballR :: Double
}
	deriving (Eq, Ord, Show)
makeLenses ''Ball

data View = View {
	_viewP :: Vec3 Double,
	_viewQ :: Vec3 Double
}
	deriving (Eq, Ord, Show)
makeLenses ''View

data Dict = Dict {
	_dictAi :: Int32,
	_dictAj :: Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Dict

-- | The level format: .sol.
--
-- Uses little-endian format.
--
-- Also encoded as floats, but we read into doubles.
--
-- The Storable instance does not follow the sizeOf laws of not accessing its
-- argument, but is still useful for reading and writing when combined with
-- validation.  peekSol and pokeSol offer more pure implementations.
--
-- Note that Mtrl, Path, and Swch have irregular serializers, and Mtrl and Path
-- is variable-width.  The sizes by default are the conservative max sizes.
data Sol = Sol {
	_solMagic   :: Int32,
	_solVersion :: Int32,

	_solAc :: Int32,
	_solDc :: Int32,
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
	_solIc :: Int32,

	_solAv :: Array Int32 CChar,
	_solDv :: Array Int32 Dict,
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
	_solIv :: Array Int32 Int32
}
	deriving (Eq, Ord, Show)
makeLenses ''Sol

type LevelIB = Sol

emptySol :: Sol
emptySol =
	(Sol
		solMagicConstant solVersionCurr
		0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
		a a a a a a a a a a a a a a a a a a a a a
	)
	where
		a :: (Storable a) => Array Int32 a
		a = IA.listArray (0, -1) []

-- * Optional low-level storable provisions.

instance Storable Sol where
	sizeOf = sizeOfEmptySol

	alignment
		~(Sol
			magic version
			ac dc mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc ic
			--av dv mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv iv
			_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
		) = max 1 . maximum $
			[
				alignment magic,
				alignment version,

				alignment ac,
				alignment dc,
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
				alignment ic,

				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: CChar),
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Dict ),
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
				alignment (error "Internal error: alignment Sol: alignment accessed its argument!"  :: Int32)
			]

	peek = peekSol
	poke = pokeSol

sizeOfMtrl :: Mtrl -> Int
sizeOfMtrl = sizeOfMtrlMax

sizeOfMtrlMin :: Mtrl -> Int
sizeOfMtrlMin
	~(Mtrl
		_d _a _s _e _h _angle fl _f _alphaFunc _alphaRef
	) = sum $
		[
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x',
			--sizeOf angle,
			sizeOf fl,
			solPathMax
			--sizeOf alphaFunc,
			--sizeOf x'
		]
	where x' = error "Internal error: sizeOf Mtrl: sizeOf accessed its argument!" :: Float

sizeOfMtrlMax :: Mtrl -> Int
sizeOfMtrlMax
	~(Mtrl
		_d _a _s _e _h _angle fl _f alphaFunc _alphaRef
	) = sum $
		[
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x',
			--sizeOf angle,
			sizeOf fl,
			solPathMax,
			sizeOf alphaFunc,
			sizeOf x'
		]
	where x' = error "Internal error: sizeOf Mtrl: sizeOf accessed its argument!" :: Float

sizeOfExistingMtrl :: Mtrl -> Int
sizeOfExistingMtrl
	~(Mtrl
		_d _a _s _e _h _angle fl _f alphaFunc _alphaRef
	) = sum $
		[
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x' * 4,
			sizeOf x',
			--sizeOf angle,
			sizeOf fl,
			solPathMax,
			if' ((fl .&. mtrlFlagAlphaTest) /= 0) 0 $ sizeOf alphaFunc,
			if' ((fl .&. mtrlFlagAlphaTest) /= 0) 0 $ sizeOf x'
		]
	where x' = error "Internal error: sizeOf Mtrl: sizeOf accessed its argument!" :: Float

sizeOfPath :: Path -> Int
sizeOfPath = sizeOfPathMax

sizeOfPathMin :: Path -> Int
sizeOfPathMin
	~(Path
		_p _e _t _tm pi_ f s fl _p0 _p1
	) = sum $
		[
			3 * sizeOf x',
			--4 * sizeOf x',
			sizeOf x',
			--sizeOf tm,

			sizeOf pi_,
			sizeOf f,
			sizeOf s,

			sizeOf fl
			--sizeOf p0,
			--sizeOf p1
		]
	where x' = error "Internal error: sizeOf Path: sizeOf accessed its argument!" :: Float

sizeOfPathMax :: Path -> Int
sizeOfPathMax
	~(Path
		_p _e _t _tm pi_ f s fl p0 p1
	) = sum $
		[
			3 * sizeOf x',
			4 * sizeOf x',
			sizeOf x',
			--sizeOf tm,

			sizeOf pi_,
			sizeOf f,
			sizeOf s,

			sizeOf fl,
			sizeOf p0,
			sizeOf p1
		]
	where x' = error "Internal error: sizeOf Path: sizeOf accessed its argument!" :: Float

sizeOfExistingPath :: Path -> Int
sizeOfExistingPath
	(Path
		_p _e _t _tm pi_ f s fl p0 p1
	) = sum $
		[
			3 * sizeOf x',
			if' ((fl .&. pathFlagOriented) /= 0) 0 $ 4 * sizeOf x',
			sizeOf x',
			--sizeOf tm,

			sizeOf pi_,
			sizeOf f,
			sizeOf s,

			sizeOf fl,
			if' ((fl .&. pathFlagParented) /= 0) 0 $ sizeOf p0,
			if' ((fl .&. pathFlagParented) /= 0) 0 $ sizeOf p1
		]
	where x' = error "Internal error: sizeOf Path: sizeOf accessed its argument!" :: Float

sizeOfEmptySol :: Sol -> Int
sizeOfEmptySol
	~(Sol
		magic version
		ac dc mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc ic
		--av dv mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv iv
		_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
	) = sum $
		[
			sizeOf magic,
			sizeOf version,

			sizeOf ac,
			sizeOf dc,
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
			sizeOf ic,

			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: CChar),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Dict ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Mtrl ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Vert ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Edge ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Side ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Texc ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Offs ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Geom ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Lump ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Node ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Path ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Body ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Item ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Goal ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Jump ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Swch ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Bill ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Ball ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: View ),
			0 * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Int32)
		]

-- | Accesses its argument!
--
-- Disregards the actual array elements, making this suitable to parse lengths
-- and then judge needed total storage size.
--
-- Get a lower bound by the minimum size of variant width elements, but don't
-- precisely calculate the exact size.
sizeOfExistingSolMin :: Sol -> Int
sizeOfExistingSolMin
	(Sol
		magic version
		ac dc mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc ic
		--av dv mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv iv
		_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
	) = sum $
		[
			sizeOf magic,
			sizeOf version,

			sizeOf ac,
			sizeOf dc,
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
			sizeOf ic,

			fromIntegral ac * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: CChar),
			fromIntegral dc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Dict ),
			fromIntegral mc * sizeOfMtrlMin (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Mtrl ),
			fromIntegral vc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Vert ),
			fromIntegral ec * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Edge ),
			fromIntegral sc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Side ),
			fromIntegral tc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Texc ),
			fromIntegral oc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Offs ),
			fromIntegral gc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Geom ),
			fromIntegral lc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Lump ),
			fromIntegral nc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Node ),
			fromIntegral pc * sizeOfPathMin (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Path ),
			fromIntegral bc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Body ),
			fromIntegral hc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Item ),
			fromIntegral zc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Goal ),
			fromIntegral jc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Jump ),
			fromIntegral xc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Swch ),
			fromIntegral rc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Bill ),
			fromIntegral uc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Ball ),
			fromIntegral wc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: View ),
			fromIntegral ic * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Int32)
		]

-- | Accesses its argument!
--
-- Disregards the actual array elements, making this suitable to parse lengths
-- and then judge needed total storage size.
--
-- Get a lower bound by the maximum size of variant width elements, but don't
-- precisely calculate the exact size.
sizeOfExistingSolMax :: Sol -> Int
sizeOfExistingSolMax
	(Sol
		magic version
		ac dc mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc ic
		--av dv mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv iv
		_  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _  _
	) = sum $
		[
			sizeOf magic,
			sizeOf version,

			sizeOf ac,
			sizeOf dc,
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
			sizeOf ic,

			fromIntegral ac * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: CChar),
			fromIntegral dc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Dict ),
			fromIntegral mc * sizeOfMtrlMax (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Mtrl ),
			fromIntegral vc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Vert ),
			fromIntegral ec * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Edge ),
			fromIntegral sc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Side ),
			fromIntegral tc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Texc ),
			fromIntegral oc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Offs ),
			fromIntegral gc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Geom ),
			fromIntegral lc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Lump ),
			fromIntegral nc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Node ),
			fromIntegral pc * sizeOfPathMax (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Path ),
			fromIntegral bc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Body ),
			fromIntegral hc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Item ),
			fromIntegral zc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Goal ),
			fromIntegral jc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Jump ),
			fromIntegral xc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Swch ),
			fromIntegral rc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Bill ),
			fromIntegral uc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Ball ),
			fromIntegral wc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: View ),
			fromIntegral ic * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Int32)
		]

-- | Accesses its argument!
--
-- Disregards the actual array elements, making this suitable to parse lengths
-- and then judge needed total storage size.
sizeOfExistingSol :: Sol -> Int
sizeOfExistingSol
	(Sol
		magic version
		ac dc mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc ic
		--av dv mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv iv
		_  _  mv _  _  _  _  _  _  _  _  pv _  _  _  _  _  _  _  _  _
	) = sum $
		[
			sizeOf magic,
			sizeOf version,

			sizeOf ac,
			sizeOf dc,
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
			sizeOf ic,

			fromIntegral ac * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: CChar),
			fromIntegral dc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Dict ),
			sum . flip map (IA.elems mv) $ \mtrl -> sizeOfExistingMtrl mtrl,
			fromIntegral vc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Vert ),
			fromIntegral ec * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Edge ),
			fromIntegral sc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Side ),
			fromIntegral tc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Texc ),
			fromIntegral oc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Offs ),
			fromIntegral gc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Geom ),
			fromIntegral lc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Lump ),
			fromIntegral nc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Node ),
			sum . flip map (IA.elems pv) $ \path -> sizeOfExistingPath path,
			fromIntegral bc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Body ),
			fromIntegral hc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Item ),
			fromIntegral zc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Goal ),
			fromIntegral jc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Jump ),
			fromIntegral xc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Swch ),
			fromIntegral rc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Bill ),
			fromIntegral uc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Ball ),
			fromIntegral wc * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: View ),
			fromIntegral ic * sizeOf (error "Internal error: sizeOf Sol: sizeOf accessed its argument!"  :: Int32)
		]

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
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
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
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))
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
	let (w32 :: Word32) = ((fromIntegral byte0 `shiftL` 24) .|. (fromIntegral byte1 `shiftL` 16) .|. (fromIntegral byte2 `shiftL` 8) .|. (fromIntegral byte3 `shiftL` 0))

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
		if' (relOffset >= bufSize) (return []) $ do
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

-- mfix doesn't seem quite up to the task; it throws a blocked indefinitely in
-- mvar exception.  Just do it manually.
{-
peekSol :: Ptr Sol -> IO Sol
peekSol ptr = mfix $ \sol -> flip evalStateT 0 $ Sol <$>
	peeki32LE ptr' <*>  -- magic
	peeki32LE ptr' <*>  -- version

	peeki32LE ptr' <*>  -- ac
	peeki32LE ptr' <*>  -- dc
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
	peeki32LE ptr' <*>  -- ic

	peekn ptr' (sol^.solAc) <*>  -- av
	peekn ptr' (sol^.solDc) <*>  -- dv
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
	peekn ptr' (sol^.solIc)      -- iv

	where ptr' = castPtr ptr
-}
peekSol :: Ptr Sol -> IO Sol
peekSol ptr = flip evalStateT 0 $ do
	magic   <- peeki32LE ptr'
	version <- peeki32LE ptr'

	ac <- peeki32LE ptr'
	dc <- peeki32LE ptr'
	mc <- peeki32LE ptr'
	vc <- peeki32LE ptr'
	ec <- peeki32LE ptr'
	sc <- peeki32LE ptr'
	tc <- peeki32LE ptr'
	oc <- peeki32LE ptr'
	gc <- peeki32LE ptr'
	lc <- peeki32LE ptr'
	nc <- peeki32LE ptr'
	pc <- peeki32LE ptr'
	bc <- peeki32LE ptr'
	hc <- peeki32LE ptr'
	zc <- peeki32LE ptr'
	jc <- peeki32LE ptr'
	xc <- peeki32LE ptr'
	rc <- peeki32LE ptr'
	uc <- peeki32LE ptr'
	wc <- peeki32LE ptr'
	ic <- peeki32LE ptr'

	av <- peekn ptr' ac
	dv <- peekn ptr' dc
	mv <- peekn ptr' mc
	vv <- peekn ptr' vc
	ev <- peekn ptr' ec
	sv <- peekn ptr' sc
	tv <- peekn ptr' tc
	ov <- peekn ptr' oc
	gv <- peekn ptr' gc
	lv <- peekn ptr' lc
	nv <- peekn ptr' nc
	pv <- peekn ptr' pc
	bv <- peekn ptr' bc
	hv <- peekn ptr' hc
	zv <- peekn ptr' zc
	jv <- peekn ptr' jc
	xv <- peekn ptr' xc
	rv <- peekn ptr' rc
	uv <- peekn ptr' uc
	wv <- peekn ptr' wc
	iv <- peekn ptr' ic

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

	where ptr' = castPtr ptr

-- | Parse only the lengths, leaving empty arrays.
--
-- This can be used to safely read in a whole SOL file with arbitrary input.
peekSolLengths :: Ptr Sol -> IO Sol
peekSolLengths ptr = flip evalStateT 0 $ Sol <$>
	peeki32LE ptr' <*>  -- magic
	peeki32LE ptr' <*>  -- version

	peeki32LE ptr' <*>  -- ac
	peeki32LE ptr' <*>  -- dc
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
	peeki32LE ptr' <*>  -- ic

	pure emptyArray <*>  -- av
	pure emptyArray <*>  -- dv
	pure emptyArray <*>  -- mv
	pure emptyArray <*>  -- vv
	pure emptyArray <*>  -- ev
	pure emptyArray <*>  -- sv
	pure emptyArray <*>  -- tv
	pure emptyArray <*>  -- ov
	pure emptyArray <*>  -- gv
	pure emptyArray <*>  -- lv
	pure emptyArray <*>  -- nv
	pure emptyArray <*>  -- pv
	pure emptyArray <*>  -- bv
	pure emptyArray <*>  -- hv
	pure emptyArray <*>  -- zv
	pure emptyArray <*>  -- jv
	pure emptyArray <*>  -- xv
	pure emptyArray <*>  -- rv
	pure emptyArray <*>  -- uv
	pure emptyArray <*>  -- wv
	pure emptyArray      -- iv

	where
		ptr' = castPtr ptr
		a :: (Storable a) => Array Int32 a
		a = IA.listArray (0, -1) []
		emptyArray :: (Storable a) => Array Int32 a
		emptyArray = a

pokeSol :: Ptr Sol -> Sol -> IO ()
pokeSol ptr
	(Sol
		magic version
		ac dc mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc ic
		av dv mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv iv
	) = flip evalStateT 0 $ do
		pokei32LE ptr' magic
		pokei32LE ptr' version

		pokei32LE ptr' ac
		pokei32LE ptr' dc
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
		pokei32LE ptr' ic

		poken ptr' ac av
		poken ptr' mc dv
		poken ptr' vc mv
		poken ptr' ec vv
		poken ptr' sc ev
		poken ptr' tc sv
		poken ptr' oc tv
		poken ptr' gc ov
		poken ptr' lc gv
		poken ptr' nc lv
		poken ptr' pc nv
		poken ptr' bc pv
		poken ptr' hc bv
		poken ptr' zc hv
		poken ptr' jc zv
		poken ptr' xc jv
		poken ptr' rc xv
		poken ptr' uc rv
		poken ptr' wc uv
		poken ptr' dc wv
		poken ptr' ic iv

	where ptr' = castPtr ptr

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
	sizeOf = sizeOfMtrlMax
	alignment
		~(Mtrl
			_d _a _s _e h _angle fl _f alphaFunc alphaRef
		) = max 1 . maximum $
			[
				alignment x',
				alignment x',
				alignment x',
				alignment x',
				alignment h,
				--alignment angle,
				alignment fl,
				1,
				alignment alphaFunc,
				alignment alphaRef
			]
		where x' = error "Internal error: alignment Mtrl: alignment accessed its argument!" :: Float

	-- Irregular encoding; replace straightforward.
	{-
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
	-}

	peek ptr = flip evalStateT 0 $ do
		d <- Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr'
		a <- Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr'
		s <- Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr'
		e <- Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr'
		h <- peekf32dLE ptr'

		let angle = 0.0

		fl <- peeki32LE ptr'

		f <- peekCString ptr' solPathMax

		alphaFunc <- if' ((fl .&. mtrlFlagAlphaTest) /= 0) (peeki32LE  ptr') (pure 0  )
		alphaRef  <- if' ((fl .&. mtrlFlagAlphaTest) /= 0) (peekf32dLE ptr') (pure 0.0)

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

		where ptr' = castPtr ptr

	poke ptr
		(Mtrl
			d a s e h _angle fl f alphaFunc alphaRef
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' (d^.x4) >> pokef32dLE ptr' (d^.y4) >> pokef32dLE ptr' (d^.z4) >> pokef32dLE ptr' (d^.w4)
			pokef32dLE ptr' (a^.x4) >> pokef32dLE ptr' (a^.y4) >> pokef32dLE ptr' (a^.z4) >> pokef32dLE ptr' (a^.w4)
			pokef32dLE ptr' (s^.x4) >> pokef32dLE ptr' (s^.y4) >> pokef32dLE ptr' (s^.z4) >> pokef32dLE ptr' (s^.w4)
			pokef32dLE ptr' (e^.x4) >> pokef32dLE ptr' (e^.y4) >> pokef32dLE ptr' (e^.z4) >> pokef32dLE ptr' (e^.w4)
			pokef32dLE ptr' h

			--pokef32dLE ptr' angle

			pokei32LE ptr' fl

			pokeCString ptr' solPathMax f

			when ((fl .&. mtrlFlagAlphaTest) /= 0) $ pokei32LE  ptr' alphaFunc
			when ((fl .&. mtrlFlagAlphaTest) /= 0) $ pokef32dLE ptr' alphaRef

		where ptr' = castPtr ptr

instance Storable Vert where
	sizeOf    ~(Vert (Vec3 _ _ _)) = sum [3 * sizeOf x']
		where x' = error "Internal error: sizeOf Vert: sizeOf accessed its argument!" :: Float
	alignment ~(Vert (Vec3 _ _ _)) = max 1 $ maximum [alignment x']
		where x' = error "Internal error: alignment Vert: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Vert <$> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr')
		where ptr' = castPtr ptr
	poke ptr (Vert (Vec3 x y z)) = flip evalStateT 0 $ pokef32dLE ptr' x >> pokef32dLE ptr' y >> pokef32dLE ptr' z
		where ptr' = castPtr ptr

instance Storable Edge where
	sizeOf    ~(Edge vi vj) = sum [sizeOf vi, sizeOf vj]
	alignment ~(Edge vi vj) = max 1 $ maximum [alignment vi, alignment vj]
	peek ptr = flip evalStateT 0 $ Edge <$> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Edge vi vj) = flip evalStateT 0 $ pokei32LE ptr' vi >> pokei32LE ptr' vj
		where ptr' = castPtr ptr

instance Storable Side where
	sizeOf    ~(Side (Vec3 _nx _ny _nz) _d) = sum [3 * sizeOf x', sizeOf x']
		where x' = error "Internal error: sizeOf Side: sizeOf accessed its argument!" :: Float
	alignment ~(Side (Vec3 _nx _ny _nz) _d) = max 1 $ maximum [alignment x', alignment x']
		where x' = error "Internal error: alignment Side: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Side <$> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*> peekf32dLE ptr'
		where ptr' = castPtr ptr
	poke ptr (Side (Vec3 nx ny nz) d) = flip evalStateT 0 $ pokef32dLE ptr' nx >> pokef32dLE ptr' ny >> pokef32dLE ptr' nz >> pokef32dLE ptr' d
		where ptr' = castPtr ptr

instance Storable Texc where
	sizeOf    ~(Texc (Vec2 _tx _ty)) = sum [2 * sizeOf x']
		where x' = error "Internal error: sizeOf Texc: sizeOf accessed its argument!" :: Float
	alignment ~(Texc (Vec2 _tx _ty)) = max 1 $ maximum [alignment x', alignment x']
		where x' = error "Internal error: alignment Texc: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Texc <$> (Vec2 <$> peekf32dLE ptr' <*> peekf32dLE ptr')
		where ptr' = castPtr ptr
	poke ptr (Texc (Vec2 tx ty)) = flip evalStateT 0 $ pokef32dLE ptr' tx >> pokef32dLE ptr' ty
		where ptr' = castPtr ptr

instance Storable Offs where
	sizeOf    ~(Offs ti si vi) = sum [sizeOf ti, sizeOf si, sizeOf vi]
	alignment ~(Offs ti si vi) = max 1 $ maximum [alignment ti, alignment si, alignment vi]
	peek ptr = flip evalStateT 0 $ Offs <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Offs ti si vi) = flip evalStateT 0 $ pokei32LE ptr' ti >> pokei32LE ptr' si >> pokei32Native ptr' vi
		where ptr' = castPtr ptr

instance Storable Geom where
	sizeOf    ~(Geom mi oi oj ok) = sum [sizeOf mi, sizeOf oi, sizeOf oj, sizeOf ok]
	alignment ~(Geom mi oi oj ok) = max 1 $ maximum [alignment mi, alignment oi, alignment oj, alignment ok]
	peek ptr = flip evalStateT 0 $ Geom <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Geom mi oi oj ok) = flip evalStateT 0 $ pokei32LE ptr' mi >> pokei32LE ptr' oi >> pokei32Native ptr' oj >> pokei32LE ptr' ok
		where ptr' = castPtr ptr

instance Storable Lump where
	sizeOf    ~(Lump fl v0 vc e0 ec g0 gc s0 sc) = sum [sizeOf fl, sizeOf v0, sizeOf vc, sizeOf e0, sizeOf ec, sizeOf g0, sizeOf gc, sizeOf s0, sizeOf sc]
	alignment ~(Lump fl v0 vc e0 ec g0 gc s0 sc) = max 1 $ maximum [alignment fl, alignment v0, alignment vc, alignment e0, alignment ec, alignment g0, alignment gc, alignment s0, alignment sc]
	peek ptr = flip evalStateT 0 $ Lump <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Lump fl v0 vc e0 ec g0 gc s0 sc) = flip evalStateT 0 $ pokei32LE ptr' fl >> pokei32LE ptr' v0 >> pokei32LE ptr' vc >> pokei32LE ptr' e0 >> pokei32LE ptr' ec >> pokei32LE ptr' g0 >> pokei32LE ptr' gc >> pokei32LE ptr' s0 >> pokei32LE ptr' sc
		where ptr' = castPtr ptr

instance Storable Node where
	sizeOf    ~(Node si ni nj l0 lc) = sum [sizeOf si, sizeOf ni, sizeOf nj, sizeOf l0, sizeOf lc]
	alignment ~(Node si ni nj l0 lc) = max 1 $ maximum [alignment si, alignment ni, alignment nj, alignment l0, alignment lc]
	peek ptr = flip evalStateT 0 $ Node <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Node si ni nj l0 lc) = flip evalStateT 0 $ pokei32LE ptr' si >> pokei32LE ptr' ni >> pokei32LE ptr' nj >> pokei32LE ptr' l0 >> pokei32LE ptr' lc
		where ptr' = castPtr ptr

instance Storable Path where
	sizeOf = sizeOfPathMax
	alignment
		~(Path
			_p _e _t _tm pi_ f s fl p0 p1
		) = max 1 . maximum $
			[
				alignment x',
				alignment x',
				alignment x',
				--alignment tm,

				alignment pi_,
				alignment f,
				alignment s,

				alignment fl,
				alignment p0,
				alignment p1
			]
		where x' = error "Internal error: alignment Path: alignment accessed its argument!" :: Float

	{-
	peek ptr = flip evalStateT 0 $ Path <$>
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- p
		(Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- e
		peekf32dLE ptr' <*>  -- t
		peeki32LE ptr' <*>  -- tm

		peeki32LE ptr' <*>  -- pi
		peeki32LE ptr' <*>  -- f
		peeki32LE ptr' <*>  -- s

		peeki32LE ptr' <*>  -- fl

		peeki32LE ptr' <*>  -- p0
		peeki32LE ptr'  -- p1

		where ptr' = castPtr ptr

	poke ptr
		(Path
			(Vec3 px py pz) (Vec4 ex ey ez ew) t tm pi_ f s fl p0 p1
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz
			pokef32dLE ptr' ex >> pokef32dLE ptr' ey >> pokef32dLE ptr' ez >> pokef32dLE ptr' ew
			pokef32dLE ptr' t
			pokei32LE ptr' tm

			pokei32LE ptr' pi_
			pokei32LE ptr' f
			pokei32LE ptr' s

			pokei32LE ptr' fl

			pokei32LE ptr' p0
			pokei32LE ptr' p1

		where ptr' = castPtr ptr
	-}
	-- Actually, Paths are serialized in a different order, and are also
	-- variable width.  So we'll need to provide special implementations
	-- carefully.
	peek ptr = flip evalStateT 0 $ do
		p   <- Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr'
		t   <- peekf32dLE ptr'
		pi_ <- peeki32LE ptr'
		f   <- peeki32LE ptr'
		s   <- peeki32LE ptr'
		fl  <- peeki32LE ptr'

		e        <- if' ((fl .&. pathFlagOriented) /= 0) (Vec4 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') (return $ Vec4 0.0 0.0 0.0 0.0)
		(p0, p1) <- if' ((fl .&. pathFlagParented) /= 0) ((,)  <$> peeki32LE  ptr' <*> peeki32LE  ptr'                                        ) (return $ (0, 0))

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

		where ptr' = castPtr ptr
	poke ptr
		(Path
			(Vec3 px py pz) (Vec4 ex ey ez ew) t _tm pi_ f s fl p0 p1
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz
			pokef32dLE ptr' t
			pokei32LE  ptr' pi_
			pokei32LE  ptr' f
			pokei32LE  ptr' s
			pokei32LE  ptr' fl

			when ((fl .&. pathFlagOriented) /= 0) (pokef32dLE ptr' ex >> pokef32dLE ptr' ey >> pokef32dLE ptr' ez >> pokef32dLE ptr' ew)
			when ((fl .&. pathFlagParented) /= 0) (pokei32LE ptr' p0 >> pokei32LE ptr' p1)

		where ptr' = castPtr ptr

instance Storable Body where
	sizeOf    ~(Body p0 p1 ni l0 lc g0 gc) = sum [sizeOf p0, sizeOf p1, sizeOf ni, sizeOf l0, sizeOf lc, sizeOf g0, sizeOf gc]
	alignment ~(Body p0 p1 ni l0 lc g0 gc) = max 1 $ maximum [alignment p0, alignment p1, alignment ni, alignment l0, alignment lc, alignment g0, alignment gc]
	peek ptr = flip evalStateT 0 $ Body <$> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Body p0 p1 ni l0 lc g0 gc) = flip evalStateT 0 $ pokei32LE ptr' p0 >> pokei32LE ptr' p1 >> pokei32Native ptr' ni >> pokei32LE ptr' l0 >> pokei32LE ptr' lc >> pokei32Native ptr' g0 >> pokei32LE ptr' gc
		where ptr' = castPtr ptr

instance Storable Item where
	sizeOf
		~(Item
			_p t n p0 p1
		) = sum $
			[
				3 * sizeOf x',
				sizeOf t,
				sizeOf n,

				sizeOf p0,
				sizeOf p1
			]
		where x' = error "Internal error: sizeOf Item: sizeOf accessed its argument!" :: Float
	alignment
		~(Item
			_p t n p0 p1
		) = max 1 . maximum $
			[
				alignment x',
				alignment t,
				alignment n,

				alignment p0,
				alignment p1
			]
		where x' = error "Internal error: alignment Item: alignment accessed its argument!" :: Float

	peek ptr = ((\item -> item & (itemP1 .~ (if' (item^.itemP1 < 0) (item^.itemP0) (item^.itemP1)))) <$>) . flip evalStateT 0 $ Item <$>
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- p
		peeki32LE ptr' <*>  -- t
		peeki32LE ptr' <*>  -- n

		peeki32LE ptr' <*>  -- p0
		peeki32LE ptr'      -- p1

		where ptr' = castPtr ptr

	poke ptr
		(Item
			(Vec3 px py pz) t n p0 p1
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz
			pokei32LE ptr' t
			pokei32LE ptr' n

			pokei32LE ptr' p0
			pokei32LE ptr' p1

		where ptr' = castPtr ptr

instance Storable Goal where
	sizeOf
		~(Goal
			_p _r p0 p1
		) = sum $
			[
				3 * sizeOf x',
				sizeOf x',

				sizeOf p0,
				sizeOf p1
			]
		where x' = error "Internal error: sizeOf Item: sizeOf accessed its argument!" :: Float
	alignment
		~(Goal
			_p _r p0 p1
		) = max 1 . maximum $
			[
				alignment x',
				alignment x',

				alignment p0,
				alignment p1
			]
		where x' = error "Internal error: alignment Item: alignment accessed its argument!" :: Float

	peek ptr = ((\goal -> goal & (goalP1 .~ (if' (goal^.goalP1 < 0) (goal^.goalP0) (goal^.goalP1)))) <$>) . flip evalStateT 0 $ Goal <$>
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- p
		peekf32dLE ptr' <*>  -- r

		peeki32LE ptr' <*>  -- p0
		peeki32LE ptr'      -- p1

		where ptr' = castPtr ptr

	poke ptr
		(Goal
			(Vec3 px py pz) r p0 p1
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz
			pokef32dLE ptr' r

			pokei32LE ptr' p0
			pokei32LE ptr' p1

		where ptr' = castPtr ptr

instance Storable Jump where
	sizeOf
		~(Jump
			_p _q _r p0 p1
		) = sum $
			[
				3 * sizeOf x',
				3 * sizeOf x',
				sizeOf x',

				sizeOf p0,
				sizeOf p1
			]
		where x' = error "Internal error: sizeOf Jump: sizeOf accessed its argument!" :: Float
	alignment
		~(Jump
			_p _q _r p0 p1
		) = max 1 . maximum $
			[
				alignment x',
				alignment x',
				alignment x',

				alignment p0,
				alignment p1
			]
		where x' = error "Internal error: alignment Jump: alignment accessed its argument!" :: Float

	peek ptr = ((\jump -> jump & (jumpP1 .~ (if' (jump^.jumpP1 < 0) (jump^.jumpP0) (jump^.jumpP1)))) <$>) . flip evalStateT 0 $ Jump <$>
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- p
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- q
		peekf32dLE ptr' <*>  -- r

		peeki32LE ptr' <*>  -- p0
		peeki32LE ptr'      -- p1

		where ptr' = castPtr ptr

	poke ptr
		(Jump
			(Vec3 px py pz) (Vec3 qx qy qz) r p0 p1
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz
			pokef32dLE ptr' qx >> pokef32dLE ptr' qy >> pokef32dLE ptr' qz
			pokef32dLE ptr' r

			pokei32LE ptr' p0
			pokei32LE ptr' p1

		where ptr' = castPtr ptr

instance Storable Swch where
	sizeOf
		~(Swch
			_p _r pi_ _t _tm f i p0 p1
		) = sum $
			[
				3 * sizeOf x',
				sizeOf x',
				sizeOf pi_,
				sizeOf x',
				--sizeOf tm,
				sizeOf f,
				sizeOf i,

				sizeOf p0,
				sizeOf p1
			]
		where x' = error "Internal error: sizeOf Swch: sizeOf accessed its argument!" :: Float
	alignment
		~(Swch
			_p _r pi_ _t _tm f i p0 p1
		) = max 1 . maximum $
			[
				alignment x',
				alignment x',
				alignment pi_,
				alignment x',
				--alignment tm,
				alignment f,
				alignment i,

				alignment p0,
				alignment p1
			]
		where x' = error "Internal error: alignment Swch: alignment accessed its argument!" :: Float

	peek ptr =
		((\swch -> swch & (swchP1 .~ (if' (swch^.swchP1 < 0) (swch^.swchP0) (swch^.swchP1)))) <$>) .
		((\swch -> swch & (swchTm .~ (round $ 1000.0*(swch^.swchT)))) <$>) . flip evalStateT 0 $ Swch <$>
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- p
		peekf32dLE ptr' <*>  -- r
		peeki32LE ptr' <*>  -- pi

		(peekf32dLE ptr' <* peekf32dLE ptr') <*>  -- t (skip f32)
		pure 0 <*>  -- tm
		(peeki32LE ptr' <* peeki32LE ptr') <*>  -- f (skip i32)
		peeki32LE ptr' <*>  -- i

		peeki32LE ptr' <*>  -- p0
		peeki32LE ptr'      -- p1

		where ptr' = castPtr ptr

	poke ptr
		(Swch
			(Vec3 px py pz) r pi_ t _tm f i p0 p1
		) = flip evalStateT 0 $ do
			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz
			pokef32dLE ptr' r
			pokei32LE ptr' pi_

			pokef32dLE ptr' t >> pokef32dLE ptr' t
			--pokei32LE ptr' tm
			pokei32LE ptr' f >> pokei32LE ptr' f
			pokei32LE ptr' i

			pokei32LE ptr' p0
			pokei32LE ptr' p1

		where ptr' = castPtr ptr

instance Storable Bill where
	sizeOf
		~(Bill
			fl mi _t _d _w _h _rx _ry _rz _p p0 p1
		) = sum $
			[
				sizeOf fl,
				sizeOf mi,
				sizeOf x',
				sizeOf x',

				3 * sizeOf x',
				3 * sizeOf x',

				3 * sizeOf x',
				3 * sizeOf x',
				3 * sizeOf x',

				3 * sizeOf x',

				sizeOf p0,
				sizeOf p1
			]
		where x' = error "Internal error: sizeOf Bill: sizeOf accessed its argument!" :: Float
	alignment
		~(Bill
			fl mi _t _d _w _h _rx _ry _rz _p p0 p1
		) = max 1 . maximum $
			[
				alignment fl,
				alignment mi,
				alignment x',
				alignment x',

				alignment x',
				alignment x',

				alignment x',
				alignment x',
				alignment x',

				alignment x',

				alignment p0,
				alignment p1
			]
		where x' = error "Internal error: alignment Bill: alignment accessed its argument!" :: Float

	peek ptr = ((\bill -> bill & (billP1 .~ (if' (bill^.billP1 < 0) (bill^.billP0) (bill^.billP1)))) <$>) . flip evalStateT 0 $ Bill <$>
		peeki32LE ptr' <*>  -- fl
		peeki32LE ptr' <*>  -- mi
		peekf32dLE ptr' <*>  -- t
		peekf32dLE ptr' <*>  -- d

		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- w
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- h

		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- rx
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- ry
		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- rz

		(Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*>  -- p

		peeki32LE ptr' <*>  -- p0
		peeki32LE ptr'      -- p1

		where ptr' = castPtr ptr

	poke ptr
		(Bill
			fl mi t d (Vec3 wx wy wz) (Vec3 hx hy hz) (Vec3 rxx rxy rxz) (Vec3 ryx ryy ryz) (Vec3 rzx rzy rzz) (Vec3 px py pz) p0 p1
		) = flip evalStateT 0 $ do
			pokei32LE ptr' fl
			pokei32LE ptr' mi
			pokef32dLE ptr' t
			pokef32dLE ptr' d

			pokef32dLE ptr' wx >> pokef32dLE ptr' wy >> pokef32dLE ptr' wz
			pokef32dLE ptr' hx >> pokef32dLE ptr' hy >> pokef32dLE ptr' hz

			pokef32dLE ptr' rxx >> pokef32dLE ptr' rxy >> pokef32dLE ptr' rxz
			pokef32dLE ptr' ryx >> pokef32dLE ptr' ryy >> pokef32dLE ptr' ryz
			pokef32dLE ptr' rzx >> pokef32dLE ptr' rzy >> pokef32dLE ptr' rzz

			pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz

			pokei32LE ptr' p0
			pokei32LE ptr' p1

		where ptr' = castPtr ptr

instance Storable Ball where
	sizeOf    ~(Ball (Vec3 _px _py _pz) _r) = sum [3 * sizeOf x', sizeOf x']
		where x' = error "Internal error: sizeOf Ball: sizeOf accessed its argument!" :: Float
	alignment ~(Ball (Vec3 _px _py _pz) _r) = max 1 $ maximum [alignment x', alignment x']
		where x' = error "Internal error: alignment Ball: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ Ball <$> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*> peekf32dLE ptr'
		where ptr' = castPtr ptr
	poke ptr (Ball (Vec3 px py pz) r) = flip evalStateT 0 $ pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz >> pokef32dLE ptr' r
		where ptr' = castPtr ptr

instance Storable View where
	sizeOf    ~(View (Vec3 _px _py _pz) (Vec3 _qx _qy _qz)) = sum [3 * sizeOf x', 3 * sizeOf x']
		where x' = error "Internal error: sizeOf View: sizeOf accessed its argument!" :: Float
	alignment ~(View (Vec3 _px _py _pz) (Vec3 _qx _qy _qz)) = max 1 $ maximum [alignment x', alignment x']
		where x' = error "Internal error: alignment View: alignment accessed its argument!" :: Float
	peek ptr = flip evalStateT 0 $ View <$> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr') <*> (Vec3 <$> peekf32dLE ptr' <*> peekf32dLE ptr' <*> peekf32dLE ptr')
		where ptr' = castPtr ptr
	poke ptr (View (Vec3 px py pz) (Vec3 qx qy qz)) = flip evalStateT 0 $ pokef32dLE ptr' px >> pokef32dLE ptr' py >> pokef32dLE ptr' pz >> pokef32dLE ptr' qx >> pokef32dLE ptr' qy >> pokef32dLE ptr' qz
		where ptr' = castPtr ptr

instance Storable Dict where
	sizeOf    ~(Dict ai aj) = sum [sizeOf ai, sizeOf aj]
	alignment ~(Dict ai aj) = max 1 $ maximum [alignment ai, alignment aj]
	peek ptr = flip evalStateT 0 $ Dict <$> peeki32LE ptr' <*> peeki32LE ptr'
		where ptr' = castPtr ptr
	poke ptr (Dict ai aj) = flip evalStateT 0 $ pokei32LE ptr' ai >> pokei32LE ptr' aj
		where ptr' = castPtr ptr
