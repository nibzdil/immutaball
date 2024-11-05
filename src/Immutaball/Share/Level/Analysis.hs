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
			sraBodyData, sraBodyDataGPU, sraOpaqueGeoms, sraTransparentGeoms,
		GeomPass(..), gpBi, gpMv, gpTextures, gpTexturesGPU, gpGis, gpGisGPU,
		SolPhysicsAnalysis(..),
		mkSolAnalysis,
		mkSolRenderAnalysis,
		mkSolPhysicsAnalysis
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Monad
import Data.Bits
import Data.Function hiding (id, (.))
import Data.Int
import Data.Maybe

import Control.Lens
import Control.Monad.Trans.State.Lazy
import Data.Array.IArray
import qualified Data.Map.Lazy as M

import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO.GLIO
import Immutaball.Share.Level.Analysis.LowLevel
import Immutaball.Share.Level.Base
import Immutaball.Share.Math
import Immutaball.Share.Utils

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
--
-- We have 5 arrays of data for the GPU that can be uploaded and accesses as 5
-- SSBOs.
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
	_sraBodyDataGPU  :: GLData,

	-- | A grouping of all opaque geoms by 16 textures at a time.
	_sraOpaqueGeoms      :: [GeomPass],
	_sraTransparentGeoms :: [GeomPass]
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolRenderAnalysis

-- | A subset of geometry that can be rendered in a single pass.
data GeomPass = GeomPass {
	-- | Body.
	_gpBi :: Int32,

	-- | Materials used in this pass; up to array of 16.
	_gpMv :: Array Int32 Int32,

	-- | For each geom, provide an index 0-15 of the 'gpMv' array.
	-- This array is of equal size with 'gpGis'.
	_gpTextures    :: Array Int32 Int32,
	_gpTexturesGPU :: GLData,

	-- | The textured triangles to draw.
	-- Array of geom indices.
	_gpGis    :: Array Int32 Int32,
	_gpGisGPU :: GLData
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolRenderAnalysis

-- | Extra data of the sol useful for physics.
data SolPhysicsAnalysis = SolPhysicsAnalysis {
}
	deriving (Eq, Ord, Show)
makeLenses ''SolAnalysis
makeLenses ''SolRenderAnalysis
makeLenses ''GeomPass
makeLenses ''SolPhysicsAnalysis

sar :: Lens' SolAnalysis SolRenderAnalysis
sar = saRenderAnalysis

sap :: Lens' SolAnalysis SolPhysicsAnalysis
sap = saPhysicsAnalysis

mkSolAnalysis :: IBContext' a -> Sol -> SolAnalysis
mkSolAnalysis cxt sol = fix $ \_sa -> SolAnalysis {
	_saRenderAnalysis  = mkSolRenderAnalysis  cxt sol,
	_saPhysicsAnalysis = mkSolPhysicsAnalysis cxt sol
}

mkSolRenderAnalysis :: IBContext' a -> Sol -> SolRenderAnalysis
mkSolRenderAnalysis cxt sol = fix $ \sra -> SolRenderAnalysis {
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
	_sraBodyDataGPU = gpuEncodeArray (sra^.sraBodyData),

	_sraOpaqueGeoms      = concat . filter (not . null) . map (passGeom (cxt^.ibStaticConfig.x'cfgMaxPassTextures) False) . zip [0..] $ elems (sol^.solBv),
	_sraTransparentGeoms = concat . filter (not . null) . map (passGeom (cxt^.ibStaticConfig.x'cfgMaxPassTextures) True ) . zip [0..] $ elems (sol^.solBv)
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

		-- | It'd be nice to be able to render the whole scene in a single
		-- pass, but since our shader only handles 16 textures at once, split
		-- up mtrl (mv) textures into chunks of up to 16, and then for each chunk
		-- of mtrls, take the portion of all of the scene we render that uses a
		-- texture material in that chunk.  It partitions the scene (i.e. it
		-- partitions the geom array gv) into partitions each of which only use
		-- up to 16 textures.
		--
		-- That's the purpose of this function.  Partition into partitions
		-- (each partition is a GeomPass).  Each geom is a textured triangle to
		-- draw, BTW.  Each GeomPass is a partition as we described.
		--
		-- Finally, the caller first takes all partitions on all
		-- non-transparent geometry, then take all partitions on all
		-- transparent geometry (i.e. that supports an alpha test (technically
		-- could still be opaque but just has alpha / transparency enabled)).
		passGeom :: Integer -> Bool -> (Int32, Body) -> [GeomPass]
		passGeom maxTextures transparent (bi, b) = geomPasses
			where
				-- First make a single GeomPass - a single array structure that we will later split up by 16.
				wholeGpGis = [b^.bodyG0 .. b^.bodyG0 + b^.bodyGc - 1]
				wholeGpGisTransparent = flip filter wholeGpGis $ \gi ->
					let g = (sol^.solGv) ! gi in let mi = g^.geomMi in let mtrl = (sol^.solMv) ! mi in
					(((mtrl^.mtrlFl) .&. mtrlFlagAlphaTest) /= 0) == transparent
				(wholeGpMvTextures, miToTexture) = flip runState M.empty . fmap concatFirst . forM wholeGpGisTransparent $ \gi -> let g = (sol^.solGv) ! gi in let mi = g^.geomMi in
					gets (M.lookup mi) >>= \midx -> case midx of
						Just idx -> return ([], idx)
						Nothing -> do
							idxs <- get
							let nextIdx = fromIntegral $ M.size idxs
							put $ M.insert mi nextIdx idxs
							return ([mi], nextIdx)
				(wholeGpMv, _wholeGpTextures) = split wholeGpMvTextures

				-- (We could pre-process the whole textures list, but it's
				-- convenient to process the index list at the same time that we
				-- chunk it up.)
				--wholeGpTextures' = map (`mod` maxTextures) wholeGpTextures

				-- Split up unique materials mv into pass chunks of 16, and
				-- split up gv and mv indices where the gv/mv pair points to a
				-- mtrl represented in the current pass.  (gv/mv pairings
				-- should be preserved, not end up between different partitions,
				-- since they make up an aggregate structure.)
				gpMvPasses          = chunksOf maxTextures wholeGpMv
				gpGisTexturesPasses = flip map gpMvPasses $ \mvPass ->
					let gisPass = flip filter wholeGpGisTransparent $ \gi -> (((sol^.solGv) ! gi)^.geomMi) `elem` mvPass in
					let err mi = error $ "Internal error: mkSolRenderAnalysis^.geomPasses: we thought we were tracking mtrl texture indices, but we couldn't find a texture index for mtrl i " ++ show mi ++ "." in
					flip map gisPass $ \gi -> (gi, let g = (sol^.solGv) ! gi in let mi = g^.geomMi in fi $ (fromMaybe (err mi) $ M.lookup mi miToTexture) `mod` maxTextures)
				(fi :: Integer -> Int32) = fromIntegral
				(gpGisPasses, gpTexturesPasses) = split . map split $ gpGisTexturesPasses

				listArray' xs = listArray (0, fromIntegral (length xs) - 1) xs

				gpGisPasses'      = map listArray' gpGisPasses
				gpMvPasses'       = map listArray' gpMvPasses
				gpTexturesPasses' = map listArray' gpTexturesPasses

				geomPasses = flip map (zip3 gpGisPasses' gpMvPasses' gpTexturesPasses') $
					\(gpGisPass, gpMvPass, gpTexturesPass) -> fix $ \gp -> GeomPass {
						_gpBi = bi,

						_gpMv = gpMvPass,

						_gpTextures    = fromIntegral <$> gpTexturesPass,
						_gpTexturesGPU = gpuEncodeArray (gp^.gpTextures),

						_gpGis    = gpGisPass,
						_gpGisGPU = gpuEncodeArray (gp^.gpGis)
					}

mkSolPhysicsAnalysis :: IBContext' a -> Sol -> SolPhysicsAnalysis
mkSolPhysicsAnalysis _cxt _sol = fix $ \_spa -> SolPhysicsAnalysis {
}
