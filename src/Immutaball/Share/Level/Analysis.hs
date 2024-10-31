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
		SolRenderAnalysis(..), sraVertexData, sraVertexDataGPU, --sraGeomData,
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

-- | Extra data o the sol useful for rendering.
data SolRenderAnalysis = SolRenderAnalysis {
	-- | The basis of 'sraVertexData'.
	--
	-- It's an array of the concatenation of x y and z.  *3 index gets the start index, to x.
	_sraVertexData :: Array Int32 Double,

	-- | You can use an SSBO to upload the vertex array as a GLData
	-- (sized bytestring), and then use that SSBO in the shader.
	--
	-- The shaders can usefully use the current array of vertices.
	_sraVertexDataGPU :: GLData

	-- | Get all triangles of the SOL.
	--_sraGeomData :: GLData
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
	_sraVertexData    = genArray (0, 3 * (sol^.solVc)) $ \idx -> divMod idx 3 & \(vi, coord) -> ((sol^.solVv) ! vi)^.(vertP.lcoord3 coord),
	_sraVertexDataGPU = gpuEncodeArray (sra^.sraVertexData)
}
	where
		lcoord3 :: (Integral i, Show i) => i -> Lens' (Vec3 a) a
		lcoord3 0 = x3
		lcoord3 1 = y3
		lcoord3 2 = z3
		lcoord3 x = error $ "Internal error: mkSolRenderAnalysis^.lcoord: unrecognized coord number " ++ show x ++ "."

mkSolPhysicsAnalysis :: Sol -> SolPhysicsAnalysis
mkSolPhysicsAnalysis _sol = fix $ \_spa -> SolPhysicsAnalysis {
}
