{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Level/Base.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module Immutaball.Share.Level.Base
	(
		Sol(..), solAc, solMc, solVc, solEc, solSc, solTc, solOc, solGc, solLc,
			solNc, solPc, solBc, solHc, solZc, solJc, solXc, solRc, solUc,
			solWc, solDc, solIc, solAv, solMv, solVv, solEv, solSv, solTv,
			solOv, solGv, solLv, solNv, solPv, solBv, solHv, solZv, solJv,
			solXv, solRv, solUv, solWv, solDv, solIv,
		LevelIB
	) where

import Prelude ()
--import Immutaball.Prelude

import Data.Int
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import Control.Lens
import Control.Monad.Trans.State
import Data.Array.Unboxed

-- | The level format: .sol.
--
-- Uses little-endian format.
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

	_solAv :: UArray Int32 CChar,
	_solMv :: UArray Int32 Mtrl,
	_solVv :: UArray Int32 Vert,
	_solEv :: UArray Int32 Edge,
	_solSv :: UArray Int32 Side,
	_solTv :: UArray Int32 Texc,
	_solOv :: UArray Int32 Offs,
	_solGv :: UArray Int32 Geom,
	_solLv :: UArray Int32 Lump,
	_solNv :: UArray Int32 Node,
	_solPv :: UArray Int32 Path,
	_solBv :: UArray Int32 Body,
	_solHv :: UArray Int32 Item,
	_solZv :: UArray Int32 Goal,
	_solJv :: UArray Int32 Jump,
	_solXv :: UArray Int32 Swch,
	_solRv :: UArray Int32 Bill,
	_solUv :: UArray Int32 Ball,
	_solWv :: UArray Int32 View,
	_solDv :: UArray Int32 Dict,
	_solIv :: UArray Int32 Int32
}
makeLenses ''Sol

type LevelIB = Sol

instance Storable Sol where
	sizeOf
		(Sol
			ac mc vc ec sc tc oc gc lc nc pc bc hc zc jc xc rc uc wc dc ic
			av mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv dv iv
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
			av mv vv ev sv tv ov gv lv nv pv bv hv zv jv xv rv uv wv dv iv
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

	peek ptr = flip evalStateT 0 $ Sol <$>
		peek' <*>  -- ac
		peek' <*>  -- mc
		peek' <*>  -- vc
		peek' <*>  -- ec
		peek' <*>  -- sc
		peek' <*>  -- tc
		peek' <*>  -- oc
		peek' <*>  -- gc
		peek' <*>  -- lc
		peek' <*>  -- nc
		peek' <*>  -- pc
		peek' <*>  -- bc
		peek' <*>  -- hc
		peek' <*>  -- zc
		peek' <*>  -- jc
		peek' <*>  -- xc
		peek' <*>  -- rc
		peek' <*>  -- uc
		peek' <*>  -- wc
		peek' <*>  -- dc
		peek' <*>  -- ic

		peekn ac <*>  -- av
		peekn mc <*>  -- mv
		peekn vc <*>  -- vv
		peekn ec <*>  -- ev
		peekn sc <*>  -- sv
		peekn tc <*>  -- tv
		peekn oc <*>  -- ov
		peekn gc <*>  -- gv
		peekn lc <*>  -- lv
		peekn nc <*>  -- nv
		peekn pc <*>  -- pv
		peekn bc <*>  -- bv
		peekn hc <*>  -- hv
		peekn zc <*>  -- zv
		peekn jc <*>  -- jv
		peekn xc <*>  -- xv
		peekn rc <*>  -- rv
		peekn uc <*>  -- uv
		peekn wc <*>  -- wv
		peekn dc <*>  -- dv
		peekn ic      -- iv

		where
			peek' = do
				offset <- get
				val    <- lift $ peek (ptr `plusPtr` offset)
				put $ offset + sizeOf val
				return val

			peekn n
				| n <= 0    = lift $ newArray_ (0, (-1))
				| otherwise = do
					offset <- get
					let elemSizeof = error "Internal error: peekn: sizeOf accessed its element!"
					let sizeofElem = sizeOf elemSizeof
					array_ <- lift . freeze . newGenArray (0, n-1) $ \idx ->
						peek (offset + idx * sizeofElem)
					put $ offset + n * sizeofElem
					let () = asType elemSizeof (readArray array_ 0)
					return array_
			asType :: a -> a -> ()
			asType _ _ = ()
