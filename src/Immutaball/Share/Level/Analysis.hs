{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, RankNTypes #-}

-- | Optionally, we can add our own extra information about a level file.
--
-- TODO: add a type variable for LevelSet in SolMeta to to remove ball
-- (Ball.LevelSets) dependency from Shared.  Use ‘SolMeta'’, and in Ball have
-- ‘type SolMeta = SolMeta' LevelSet’.
module Immutaball.Share.Level.Analysis
	(
		SolWithAnalysis(..), swaSol, swaSa, swaMeta,
		SolAnalysis(..), saRenderAnalysis, saPhysicsAnalysis,
		SolMeta(..), smPath, smLevelSet,
		sar, sap,
		SolRenderAnalysis(..), sraVertexData, sraVertexDataGPU, sraGeomData,
			sraGeomDataGPU, sraLumpData, sraLumpDataGPU, sraPathDoublesData,
			sraPathDoublesDataGPU, sraPathInt32sData, sraPathInt32sDataGPU,
			sraBodyData, sraBodyDataGPU, sraOpaqueGeoms, sraTransparentGeoms,
			sraGcArray, sraGcArrayGPU, sraNumOpaqueGeomPasses,
			sraNumTransparentGeomPasses, sraNumGeomPasses, sraAllGeomPassMv,
			sraAllGeomPassTextures, sraAllGeomPassGis, sraAllGeomPassMvGPU,
			sraAllGeomPassTexturesGPU, sraAllGeomPassGisGPU,
			sraGeomPassMvRanges, sraGeomPassTexturesRanges,
			sraGeomPassGisRanges, sraGeomPassMvRangesGPU,
			sraGeomPassTexturesRangesGPU, sraGeomPassGisRangesGPU,
			sraGeomPassBis, sraGeomPassBisGPU, sraTexcoordsDoubleData,
			sraTexcoordsDoubleDataGPU,
		GeomPass(..), gpBi, gpMv, gpTextures, gpTexturesGPU, gpGis, gpGisGPU,
		SolPhysicsAnalysis(..), spaLumpOutwardsSides,
			spaLumpOutwardsSidesNumNegatedNormals,
			spaLumpOutwardsSidesNumNotNegatedNormals, spaLumpAverageVertex,
		mkSolAnalysis,
		mkSolRenderAnalysis,
		mkSolPhysicsAnalysis
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Function hiding (id, (.))
import Data.Int
import Data.Maybe

import Control.Lens
import Control.Monad.Trans.State.Lazy
import Data.Array.IArray
import Data.List
import qualified Data.Map.Lazy as M

import Immutaball.Ball.LevelSets
import Immutaball.Share.Config
import Immutaball.Share.Context
import Immutaball.Share.ImmutaballIO.GLIO
--import Immutaball.Share.Level.Analysis.LowLevel
import Immutaball.Share.Level.Base
import Immutaball.Share.Math
import Immutaball.Share.Utils
import Immutaball.Share.Video

data SolWithAnalysis = SolWithAnalysis {
	_swaSol  :: Sol,
	_swaSa   :: SolAnalysis,
	_swaMeta :: SolMeta
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolWithAnalysis

data SolAnalysis = SolAnalysis {
	-- | Extra analysis of the sol useful for rendering.
	_saRenderAnalysis :: SolRenderAnalysis,

	-- | Extra analysis of the sol useful for physics.
	_saPhysicsAnalysis :: SolPhysicsAnalysis
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolAnalysis

data SolMeta = SolMeta {
	-- | The path to the sol.
	_smPath :: String,

	-- | Whether the sol is as a level set containing the sol.
	_smLevelSet :: Maybe LevelSet
}
	deriving (Eq, Ord, Show)
--makeLenses ''SolMeta

-- sar, sap

-- | Extra data of the sol useful for rendering.
--
-- We reorder the data a bit.
--
-- We have 14 arrays of data for the GPU that can be uploaded and accesses as 5
-- SSBOs.
data SolRenderAnalysis = SolRenderAnalysis {
	-- | The basis of 'sraVertexData'.
	--
	-- It's an array of the concatenation of x y and z.  *3 index gets the start index, to x.
	_sraVertexData    :: Array Int32 ShaderDoubleType,
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
	_sraPathDoublesData    :: Array Int32 ShaderDoubleType,
	_sraPathDoublesDataGPU :: GLData,
	_sraPathInt32sData     :: Array Int32 Int32,
	_sraPathInt32sDataGPU  :: GLData,

	-- | Body data: initial translation path and g0 and gc (lumps), 3 ints.
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
	_sraTransparentGeoms :: [GeomPass],

	-- | A convenience identity array of length 3 * num all geom indices.
	--
	-- This is convenient since it allows a convenient rendering pass to be
	-- performed by calling glDrawArrays with an index and count into this
	-- array, and the shaders can do the rest of the work.
	_sraGcArray    :: Array Int32 Int32,
	_sraGcArrayGPU :: GLData,

	-- Now aggregate the GeomPass lists, so they can be conveniently uploaded
	-- to the GPU in whole.
	_sraNumOpaqueGeomPasses      :: Integer,
	_sraNumTransparentGeomPasses :: Integer,
	_sraNumGeomPasses            :: Integer,

	-- The aggregate GeomPass mv, and gi&texture list arrays.

	-- | Concatenation of all geom pass gpMvs.
	_sraAllGeomPassMv       :: Array Int32 Int32,
	-- | Concatenation of all geom pass gpTextures.
	_sraAllGeomPassTextures :: Array Int32 Int32,
	-- | Concatenation of all geom pass gpGis.
	_sraAllGeomPassGis      :: Array Int32 Int32,

	-- | The GPU encoded data.
	_sraAllGeomPassMvGPU       :: GLData,
	-- | The GPU encoded data.
	_sraAllGeomPassTexturesGPU :: GLData,
	-- | The GPU encoded data.
	_sraAllGeomPassGisGPU      :: GLData,

	-- | For each geompass we have an elem pair that represents a range of the
	-- aggregate sraAllGeomPassMv array.  We can tell the GPU what geom pass we
	-- are on, and it can have access to gpMv for that GeomPass.  For each
	-- geompass, we add 2 elems: the starting index, and the count.
	_sraGeomPassMvRanges       :: Array Int32 Int32,
	-- | Same but for sraAllGeomPassTextures.
	_sraGeomPassTexturesRanges :: Array Int32 Int32,
	_sraGeomPassGisRanges      :: Array Int32 Int32,

	-- | The GPU encoded data.
	_sraGeomPassMvRangesGPU       :: GLData,
	_sraGeomPassTexturesRangesGPU :: GLData,
	_sraGeomPassGisRangesGPU      :: GLData,

	-- | Array of body indices for each geompass.
	_sraGeomPassBis    :: Array Int32 Int32,
	_sraGeomPassBisGPU :: GLData,

	-- | Array of texcoords concatenating, e.g. s0 t0 s1 t1 s2 t2 … sn tn
	-- The shader can use the vertex data to look up the ti, and then double it
	-- to get the base index for the 2 tex coord doubles (x and y for textures
	-- are often conventionally called s and t).
	_sraTexcoordsDoubleData    :: Array Int32 ShaderDoubleType,
	_sraTexcoordsDoubleDataGPU :: GLData
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
	-- | The sides of a lump by lump index (li), ensured to be pointing
	-- outwards.
	_spaLumpOutwardsSides :: M.Map Int32 [Plane3 Double],
	_spaLumpOutwardsSidesNumNegatedNormals :: M.Map Int32 Integer,
	_spaLumpOutwardsSidesNumNotNegatedNormals :: M.Map Int32 Integer,

	-- | Find the mean vertex of a lump.
	_spaLumpAverageVertex :: M.Map Int32 (Vec3 Double)
}
	deriving (Eq, Ord, Show)
makeLenses ''SolWithAnalysis
makeLenses ''SolAnalysis
makeLenses ''SolMeta
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
	_sraVertexData    = genArray (0, 3 * (sol^.solVc) - 1) $ \idx -> toShaderDoubleType $ divMod idx 3 & \(vi, coord) -> ((sol^.solVv) ! vi)^.(vertP.lcoord3 coord),
	_sraVertexDataGPU = gpuEncodeArray (sra^.sraVertexData),

	_sraGeomData    = genArray (0, 9 * (sol^.solGc) - 1) $ \idx -> divMod idx 9 & \(gi, ridx) -> geomRelIdx gi ridx,
	_sraGeomDataGPU = gpuEncodeArray (sra^.sraGeomData),

	_sraLumpData    = genArray (0, 2 * (sol^.solLc) - 1) $ \idx -> divMod idx 2 & \(li, ridx) -> lumpRelIdx li ridx,
	_sraLumpDataGPU = gpuEncodeArray (sra^.sraLumpData),

	_sraPathDoublesData    = genArray (0, 3 * (sol^.solPc) - 1) $ \idx -> toShaderDoubleType $ divMod idx 3 & \(pi_, ridx) -> pathDoubleRelIdx pi_ ridx,
	_sraPathDoublesDataGPU = gpuEncodeArray (sra^.sraPathDoublesData),

	_sraPathInt32sData    = genArray (0, 2 * (sol^.solPc) - 1) $ \idx -> divMod idx 2 & \(pi_, ridx) -> pathInt32RelIdx pi_ ridx,
	_sraPathInt32sDataGPU = gpuEncodeArray (sra^.sraPathInt32sData),

	_sraBodyData    = genArray (0, 3 * (sol^.solBc) - 1) $ \idx -> divMod idx 3 & \(bi, ridx) -> bodyRelIdx bi ridx,
	_sraBodyDataGPU = gpuEncodeArray (sra^.sraBodyData),

	_sraOpaqueGeoms      = concat . filter (not . null) . map (passGeom (cxt^.ibStaticConfig.x'cfgMaxPassTextures) False) . zip [0..] $ elems (sol^.solBv),
	_sraTransparentGeoms = concat . filter (not . null) . map (passGeom (cxt^.ibStaticConfig.x'cfgMaxPassTextures) True ) . zip [0..] $ elems (sol^.solBv),

	_sraGcArray    = genArray (0, 3 * (sol^.solGc) - 1) $ \idx -> idx,
	_sraGcArrayGPU = gpuEncodeArray (sra^.sraGcArray),

	_sraNumOpaqueGeomPasses      = genericLength (sra^.sraOpaqueGeoms),
	_sraNumTransparentGeomPasses = genericLength (sra^.sraTransparentGeoms),
	_sraNumGeomPasses            = (sra^.sraNumOpaqueGeomPasses) + (sra^.sraNumTransparentGeomPasses),

	_sraAllGeomPassMv       = listArray'_ $ [mi      | geomPasses <- [sra^.sraOpaqueGeoms, sra^.sraTransparentGeoms], geomPass <- geomPasses, mi      <- elems (geomPass^.gpMv)      ],
	_sraAllGeomPassTextures = listArray'_ $ [texture | geomPasses <- [sra^.sraOpaqueGeoms, sra^.sraTransparentGeoms], geomPass <- geomPasses, texture <- elems (geomPass^.gpTextures)],
	_sraAllGeomPassGis      = listArray'_ $ [gi      | geomPasses <- [sra^.sraOpaqueGeoms, sra^.sraTransparentGeoms], geomPass <- geomPasses, gi      <- elems (geomPass^.gpGis)     ],

	_sraAllGeomPassMvGPU       = gpuEncodeArray (sra^.sraAllGeomPassMv),
	_sraAllGeomPassTexturesGPU = gpuEncodeArray (sra^.sraAllGeomPassTextures),
	_sraAllGeomPassGisGPU      = gpuEncodeArray (sra^.sraAllGeomPassGis),

	_sraGeomPassMvRanges       = listArray (0, 2 * fromIntegral (sra^.sraNumGeomPasses) - 1) $
		let
			(opaqueMv, opaqueAccumLength) =
				(flip fix ((sra^.sraOpaqueGeoms), 0) $ \me (opaqueGeomPassesRemaining, accumLength) ->
					case opaqueGeomPassesRemaining of
						[] -> ([], accumLength)
						(opaqueGeomPass:remaining) ->
							let arrayLen = fromIntegral . rangeSize . bounds in
							let passLen = arrayLen (opaqueGeomPass^.gpMv) in
							first (\xs -> accumLength : passLen : xs) $ me (remaining, accumLength + passLen))
			(transparentMv, _transparentAccumLength) =
				(flip fix ((sra^.sraTransparentGeoms), opaqueAccumLength) $ \me (transparentGeomPassesRemaining, accumLength) ->
					case transparentGeomPassesRemaining of
						[] -> ([], accumLength)
						(transparentGeomPass:remaining) ->
							let arrayLen = fromIntegral . rangeSize . bounds in
							let passLen = arrayLen (transparentGeomPass^.gpMv) in
							first (\xs -> accumLength : passLen : xs) $ me (remaining, accumLength + passLen))
		in
			opaqueMv ++ transparentMv,
	_sraGeomPassTexturesRanges = listArray (0, 2 * fromIntegral (sra^.sraNumGeomPasses) - 1) $
		let
			(opaqueTextures, opaqueAccumLength) =
				(flip fix ((sra^.sraOpaqueGeoms), 0) $ \me (opaqueGeomPassesRemaining, accumLength) ->
					case opaqueGeomPassesRemaining of
						[] -> ([], accumLength)
						(opaqueGeomPass:remaining) ->
							let arrayLen = fromIntegral . rangeSize . bounds in
							let passLen = arrayLen (opaqueGeomPass^.gpTextures) in
							first (\xs -> accumLength : passLen : xs) $ me (remaining, accumLength + passLen))
			(transparentTextures, _transparentAccumLength) =
				(flip fix ((sra^.sraTransparentGeoms), opaqueAccumLength) $ \me (transparentGeomPassesRemaining, accumLength) ->
					case transparentGeomPassesRemaining of
						[] -> ([], accumLength)
						(transparentGeomPass:remaining) ->
							let arrayLen = fromIntegral . rangeSize . bounds in
							let passLen = arrayLen (transparentGeomPass^.gpTextures) in
							first (\xs -> accumLength : passLen : xs) $ me (remaining, accumLength + passLen))
		in
			opaqueTextures ++ transparentTextures,
	_sraGeomPassGisRanges      = listArray (0, 2 * fromIntegral (sra^.sraNumGeomPasses) - 1) $
		let
			(opaqueGis, opaqueAccumLength) =
				(flip fix ((sra^.sraOpaqueGeoms), 0) $ \me (opaqueGeomPassesRemaining, accumLength) ->
					case opaqueGeomPassesRemaining of
						[] -> ([], accumLength)
						(opaqueGeomPass:remaining) ->
							let arrayLen = fromIntegral . rangeSize . bounds in
							let passLen = arrayLen (opaqueGeomPass^.gpGis) in
							first (\xs -> accumLength : passLen : xs) $ me (remaining, accumLength + passLen))
			(transparentGis, _transparentAccumLength) =
				(flip fix ((sra^.sraTransparentGeoms), opaqueAccumLength) $ \me (transparentGeomPassesRemaining, accumLength) ->
					case transparentGeomPassesRemaining of
						[] -> ([], accumLength)
						(transparentGeomPass:remaining) ->
							let arrayLen = fromIntegral . rangeSize . bounds in
							let passLen = arrayLen (transparentGeomPass^.gpGis) in
							first (\xs -> accumLength : passLen : xs) $ me (remaining, accumLength + passLen))
		in
			opaqueGis ++ transparentGis,

	-- | The GPU encoded data.
	_sraGeomPassMvRangesGPU       = gpuEncodeArray (sra^.sraGeomPassMvRanges),
	_sraGeomPassTexturesRangesGPU = gpuEncodeArray (sra^.sraGeomPassTexturesRanges),
	_sraGeomPassGisRangesGPU      = gpuEncodeArray (sra^.sraGeomPassGisRanges),

	_sraGeomPassBis    = listArray'_ $ [bi | geomPasses <- [sra^.sraOpaqueGeoms, sra^.sraTransparentGeoms], geomPass <- geomPasses, bi <- return (geomPass^.gpBi)],
	_sraGeomPassBisGPU = gpuEncodeArray (sra^.sraGeomPassBis),

	_sraTexcoordsDoubleData    = genArray (0, 2 * (sol^.solTc) - 1) $ \idx -> toShaderDoubleType $ divMod idx 2 & \(ti, ridx) -> texcRelIdx ti ridx,
	_sraTexcoordsDoubleDataGPU = gpuEncodeArray (sra^.sraTexcoordsDoubleData)
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
		geomRelIdx gi 3    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOi)^.offsTi)  -- t1 (tex coords index)
		geomRelIdx gi 4    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOj)^.offsTi)  -- t2
		geomRelIdx gi 5    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOk)^.offsTi)  -- t3
		geomRelIdx gi 6    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOi)^.offsSi)  -- s1 (side (plane) index)
		geomRelIdx gi 7    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOj)^.offsSi)  -- s2
		geomRelIdx gi 8    = ((sol^.solOv) ! (((sol^.solGv) ! gi)^.geomOk)^.offsSi)  -- s3
		geomRelIdx gi ridx = error $ "Internal error: mkSolRenderAnalysis^.geomRelIdx: unrecognized ridx " ++ show ridx ++ " (gi " ++ show gi ++ ")."

		texcRelIdx :: Int32 -> Int32 -> Double
		texcRelIdx ti 0    = (((sol^.solTv) ! ti)^.texcU.x2)  -- s (texture x coord)
		texcRelIdx ti 1    = (((sol^.solTv) ! ti)^.texcU.y2)  -- t (texture y coord)
		texcRelIdx ti ridx = error $ "Internal error: mkSolRenderAnalysis^.texcRelIdx: unrecognized ridx " ++ show ridx ++ " (ti " ++ show ti ++ ")."

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
				indirection :: Int32 -> Int32
				indirection idx = (sol^.solIv) ! idx

				wholeGpGisDirect = map indirection [b^.bodyG0 .. b^.bodyG0 + b^.bodyGc - 1]
				wholeGpGisLumps = map indirection [r | li <- [b^.bodyL0 .. b^.bodyL0 + b^.bodyLc - 1], l <- return $ ((sol^.solLv) ! li), gi <- [l^.lumpG0 .. l^.lumpG0 + l^.lumpGc - 1], r <- return gi]

				-- First make a single GeomPass - a single array structure that we will later split up by 16.
				wholeGpGis = wholeGpGisDirect ++ wholeGpGisLumps
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

		listArray'_ xs = listArray (0, genericLength xs - 1) xs

mkSolPhysicsAnalysis :: IBContext' a -> Sol -> SolPhysicsAnalysis
mkSolPhysicsAnalysis _cxt sol = fix $ \spa -> SolPhysicsAnalysis {
	_spaLumpOutwardsSides                     = (^._1) <$> lumpSidesData spa,
	_spaLumpOutwardsSidesNumNegatedNormals    = (^._2) <$> lumpSidesData spa,
	_spaLumpOutwardsSidesNumNotNegatedNormals = (^._3) <$> lumpSidesData spa,

	_spaLumpAverageVertex = lumpAverageVertex spa
}
	where
		indirection :: Int32 -> Int32
		indirection idx = (sol^.solIv) ! idx

		lumpAverageVertex :: SolPhysicsAnalysis -> M.Map Int32 (Vec3 Double)
		lumpAverageVertex _spa = M.fromList . flip map [0..sol^.solLc - 1] $ \li ->
			let lump = (sol^.solLv) ! li in
			let vis = indirection <$> [lump^.lumpV0..lump^.lumpV0 + lump^.lumpVc - 1] in
			let vs = ((sol^.solVv) !) <$> vis in
			let vsum = sum ((^.vertP) <$> vs) in
			let vmean = (fromIntegral $ lump^.lumpVc) `sv3` vsum in
			(li, vmean)

		-- | Simply look at an arbitrary point in the convex lump to see if a
		-- given normal should be negated to make the point fall behind the
		-- plane.
		lumpSidesData :: SolPhysicsAnalysis -> M.Map Int32 ([Plane3 Double], Integer, Integer)
		lumpSidesData spa = M.fromList . flip map [0..sol^.solLc - 1] $ \li ->
			let averageVertexErrMsg = error $ "Internal error: mkSolPhysicsAnalysis: finding lump sides data for lump without average vertex for li " ++ (show li) ++ "." in
			let averageVertex = flip M.lookup (spa^.spaLumpAverageVertex) li `morElse` error averageVertexErrMsg in
			let lump = (sol^.solLv) ! li in
			let sis = indirection <$> [lump^.lumpS0..lump^.lumpS0 + lump^.lumpSc - 1] in
			let sides = ((sol^.solSv) !) <$> sis in
			let planes = flip map sides $ \side -> normalPlane3 (side^.sideN) (side^.sideD) in
			let backwardsPlanes = flip map planes $ \p -> not $ plane3PointDistance p averageVertex <= 0 in
			let numNeg = genericLength . filter id  $ backwardsPlanes in
			let numId  = genericLength . filter not $ backwardsPlanes in
			let planes' = flip map (zip planes backwardsPlanes) . uncurry $ \p n -> if' (not n) (p) (negatePlaneOrientation p) in
			(li, (planes', numNeg, numId))
