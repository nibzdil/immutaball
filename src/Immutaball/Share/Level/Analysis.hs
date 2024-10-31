{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RankNTypes #-}

-- | Optionally, we can add our own extra information about a level file.
module Immutaball.Share.Level.Analysis
	(
		SolAnalysis(..), saRenderAnalysis, saPhysicsAnalysis,
		sar, sap,
		SolRenderAnalysis(..), sraVertexData, sraVertexDataGPU, sraGeomData,
			sraGeomDataGPU, sraLumpData, sraLumpDataGPU, sraPathDoublesData,
			sraPathDoublesDataGPU, sraPathInt32sData, sraPathInt32sDataGPU,
			sraBodyData, sraBodyDataGPU,
		SolPhysicsAnalysis(..),
		mkSolAnalysis,
		mkSolRenderAnalysis,
		mkSolPhysicsAnalysis
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Function hiding (id, (.))
import Data.Int

import Control.Lens
import Data.Array.IArray

import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.Level.Analysis.LowLevel
import Immutaball.Share.Level.Base
import Immutaball.Share.Math

data SolAnalysis = SolAnalysis {
	-- | Extra analysis of the sol useful for rendering.
	_saRenderAnalysis :: SolRenderAnalysis,

	-- | Extra analysis of the sol useful for physics.
	_saPhysicsAnalysis :: SolPhysicsAnalysis
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolAnalysis

-- sar, sap

-- | Extra data of the sol useful for rendering.
--
-- We reorder the data a bit.
data SolRenderAnalysis = SolRenderAnalysis {
	-- | The basis of 'sraVertexData'.
	--
	-- It's an array of the concatenation of x y and z.  *3 index gets the start index, to x.
	_sraVertexData    :: Array Int32 Double,
	-- | You can use an SSBO to upload the vertex array as a GLData
	-- (sized bytestring), and then use that SSBO in the shader.
	--
	-- The shaders can usefully use the current array of vertices.
	_sraVertexDataGPU :: GLData,

	-- | Get all triangles of the SOL.
	-- Represented as vi, vj, vk, ti, tj, tk, si, sj, sk.
	-- (Note we don't include mtrl (base texture) data here; the renderer can
	-- handle that with passes of up to 16 textures at once, recovering the
	-- geom index.)
	_sraGeomData    :: Array Int32 Int32,
	_sraGeomDataGPU :: GLData,

	-- | Each lump is range of geoms: g0, gc.
	--
	-- (Each geom is a set of textured triangles.)
	_sraLumpData    :: Array Int32 Int32,
	_sraLumpDataGPU :: GLData,

	-- | Path doubles: start position x y and z (3 doubles); path ints: next path, smooth flag (2 ints).
	_sraPathDoublesData    :: Array Int32 Double,
	_sraPathDoublesDataGPU :: GLData,
	_sraPathInt32sData     :: Array Int32 Int32,
	_sraPathInt32sDataGPU  :: GLData,

	-- | Body data: initial translation path and l0 and lc (lumps), 3 ints.
	--
	-- When rendering, the renderer can use a uniform to tell the GPU what path
	-- the body is currently on, and the linear interpolation (0 to 1) for that
	-- path.  The GPU can also use the body's initial path to determine the
	-- relative positioning of the path when performing interpolation.
	--
	-- Bodies are sets of lumps that follow the same path.
	_sraBodyData     :: Array Int32 Int32,
	_sraBodyDataGPU  :: GLData
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolRenderAnalysis

-- | Extra data o the sol useful for rendering.
data SolPhysicsAnalysis = SolPhysicsAnalysis {
}
	deriving (Eq, Ord, Show)
makeLenses ''SolAnalysis
makeLenses ''SolRenderAnalysis
makeLenses ''SolPhysicsAnalysis

sar :: Lens' SolAnalysis SolRenderAnalysis
sar = saRenderAnalysis

sap :: Lens' SolAnalysis SolPhysicsAnalysis
sap = saPhysicsAnalysis

mkSolAnalysis :: Sol -> SolAnalysis
mkSolAnalysis sol = fix $ \_sa -> SolAnalysis {
	_saRenderAnalysis  = mkSolRenderAnalysis  sol,
	_saPhysicsAnalysis = mkSolPhysicsAnalysis sol
}

mkSolRenderAnalysis :: Sol -> SolRenderAnalysis
mkSolRenderAnalysis sol = fix $ \sra -> SolRenderAnalysis {
	_sraVertexData    = genArray (0, 3 * (sol^.solVc) - 1) $ \idx -> divMod idx 3 & \(vi, coord) -> ((sol^.solVv) ! vi)^.(vertP.lcoord3 coord),
	_sraVertexDataGPU = gpuEncodeArray (sra^.sraVertexData),

	_sraGeomData    = genArray (0, 9 * (sol^.solGc) - 1) $ \idx -> divMod idx 9 & \(gi, ridx) -> geomRelIdx gi ridx,
	_sraGeomDataGPU = gpuEncodeArray (sra^.sraGeomData),

	_sraLumpData    = genArray (0, 2 * (sol^.solLc) - 1) $ \idx -> divMod idx 2 & \(li, ridx) -> lumpRelIdx li ridx,
	_sraLumpDataGPU = gpuEncodeArray (sra^.sraLumpData),

	_sraPathDoublesData    = genArray (0, 3 * (sol^.solPc) - 1) $ \idx -> divMod idx 3 & \(pi_, ridx) -> pathDoubleRelIdx pi_ ridx,
	_sraPathDoublesDataGPU = gpuEncodeArray (sra^.sraPathDoublesData),

	_sraPathInt32sData    = genArray (0, 2 * (sol^.solPc) - 1) $ \idx -> divMod idx 2 & \(pi_, ridx) -> pathInt32RelIdx pi_ ridx,
	_sraPathInt32sDataGPU = gpuEncodeArray (sra^.sraPathInt32sData),

	_sraBodyData    = genArray (0, 3 * (sol^.solBc) - 1) $ \idx -> divMod idx 3 & \(bi, ridx) -> bodyRelIdx bi ridx,
	_sraBodyDataGPU = gpuEncodeArray (sra^.sraBodyData)
}
	where
		lcoord3 :: (Integral i, Show i) => i -> Lens' (Vec3 a) a
		lcoord3 0 = x3
		lcoord3 1 = y3
		lcoord3 2 = z3
		lcoord3 x = error $ "Internal error: mkSolRenderAnalysis^.lcoord: unrecognized coord number " ++ show x ++ "."

		geomRelIdx :: Int32 -> Int32 -> Int32
		geomRelIdx gi 0    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOi)^.offsVi)  -- v1 (first vertex of the triangle)
		geomRelIdx gi 1    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOj)^.offsVi)  -- v2 (second vertex of the triangle)
		geomRelIdx gi 2    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOk)^.offsVi)  -- v3 (third vertex of the triangle)
		geomRelIdx gi 3    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOi)^.offsTi)  -- t1 (texture translation)
		geomRelIdx gi 4    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOj)^.offsTi)  -- t2 (texture translation)
		geomRelIdx gi 5    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOk)^.offsTi)  -- t3 (texture translation)
		geomRelIdx gi 6    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOi)^.offsSi)  -- s1 (texture scale)
		geomRelIdx gi 7    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOj)^.offsSi)  -- s2
		geomRelIdx gi 8    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOk)^.offsSi)  -- s3
		geomRelIdx gi ridx = error $ "Internal error: mkSolRenderAnalysis^.geomRelIdx: unrecognized ridx " ++ show ridx ++ " (gi " ++ show gi ++ ")."

		lumpRelIdx :: Int32 -> Int32 -> Int32
		lumpRelIdx li 0    = ((sol^.solLv) ! li)^.lumpG0  -- g0
		lumpRelIdx li 1    = ((sol^.solLv) ! li)^.lumpGc  -- gc
		lumpRelIdx li ridx = error $ "Internal error: mkSolRenderAnalysis^.lumpRelIdx: unrecognized ridx " ++ show ridx ++ " (li " ++ show li ++ ")."

		pathDoubleRelIdx :: Int32 -> Int32 -> Double
		pathDoubleRelIdx pi_ 0    = ((sol^.solPv) ! pi_)^.pathP.x3  -- pathP.x3
		pathDoubleRelIdx pi_ 1    = ((sol^.solPv) ! pi_)^.pathP.y3  -- pathP.y3
		pathDoubleRelIdx pi_ 2    = ((sol^.solPv) ! pi_)^.pathP.z3  -- pathP.z3
		pathDoubleRelIdx pi_ ridx = error $ "Internal error: mkSolRenderAnalysis^.pathDoubleRelIdx: unrecognized ridx " ++ show ridx ++ " (pi " ++ show pi_ ++ ")."

		pathInt32RelIdx :: Int32 -> Int32 -> Int32
		pathInt32RelIdx pi_ 0    = ((sol^.solPv) ! pi_)^.pathPi  -- pathPi
		pathInt32RelIdx pi_ 1    = ((sol^.solPv) ! pi_)^.pathS   -- pathS
		pathInt32RelIdx pi_ ridx = error $ "Internal error: mkSolRenderAnalysis^.pathInt32RelIdx: unrecognized ridx " ++ show ridx ++ " (pi " ++ show pi_ ++ ")."

		bodyRelIdx :: Int32 -> Int32 -> Int32
		bodyRelIdx bi 0    = ((sol^.solBv) ! bi)^.bodyP0  -- bodyP0
		bodyRelIdx bi 1    = ((sol^.solBv) ! bi)^.bodyG0  -- bodyG0
		bodyRelIdx bi 2    = ((sol^.solBv) ! bi)^.bodyGc  -- bodyGc
		bodyRelIdx bi ridx = error $ "Internal error: mkSolRenderAnalysis^.bodyRelIdx: unrecognized ridx " ++ show ridx ++ " (bi " ++ show bi ++ ")."

mkSolPhysicsAnalysis :: Sol -> SolPhysicsAnalysis
mkSolPhysicsAnalysis _sol = fix $ \_spa -> SolPhysicsAnalysis {
}
