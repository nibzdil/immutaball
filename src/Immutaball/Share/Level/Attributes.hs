{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- Game.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module Immutaball.Share.Level.Attributes
	(
		SolAttributes(..), saAttributes, saMessage, saSong, saShot, saGoal,
			saTime, saTimeHs, saFastUnlockHs, saCoinHs, saVersionStr,
			saVersionNum, saAuthor, saBonus,
		mkSolAttributes
	) where

import Prelude ()
import Immutaball.Prelude

import Data.Maybe
import Data.Function hiding (id, (.))
import Data.Word
import Foreign.C.Types

import Control.Lens
import Data.Array.IArray
import qualified Data.Map as M

import Immutaball.Share.Level.Base
import Immutaball.Share.Utils

data SolAttributes = SolAttributes {
	-- | Level attributes.
	_saAttributes :: M.Map String String,

	-- | Level message.
	_saMessage      :: String,
	-- | Level song / track.
	_saSong         :: String,
	-- | Screenshot of the level.
	_saShot         :: String,
	-- | Coins needed to unlock the goal.
	_saGoal         :: Integer,
	-- | Time available in this level.
	_saTime         :: Integer,
	-- | Hard, easy, medium.  Best times.
	_saTimeHs       :: (Integer, Integer, Integer),
	-- | Hard, easy, medium.  Fast unlock (best times after unlocking goal).
	_saFastUnlockHs :: (Integer, Integer, Integer),
	-- | Hard, easy, medium.  Most coins.
	_saCoinHs       :: (Integer, Integer, Integer),

	_saVersionStr :: String,
	_saVersionNum :: Integer,
	_saAuthor     :: String,
	-- | Is this a bonus level?
	_saBonus      :: Bool
}
	deriving (Eq, Ord, Show)
makeLenses ''SolAttributes

mkSolAttributes :: Sol -> SolAttributes
mkSolAttributes sol = fix $ \sa -> SolAttributes {
	_saAttributes = M.fromList $ do
		d <- elems (sol^.solDv)
		(ki, vi) <- return (d^.dictAi, d^.dictAj)
		let k = (\f -> fix f 0 "") $ \me idx reversed_ ->
			let (cc :: CChar) = (sol^.solAv)!(ki+idx) in
			let (cb :: Word8) = fromIntegral cc in
			let (c :: Char) = asciiChar8 cb in
			if' (cc == 0) (reverse reversed_) $
			me (idx+1) (c:reversed_)
		let v = (\f -> fix f 0 "") $ \me idx reversed_ ->
			let (cc :: CChar) = (sol^.solAv)!(vi+idx) in
			let (cb :: Word8) = fromIntegral cc in
			let (c :: Char) = asciiChar8 cb in
			if' (cc == 0) (reverse reversed_) $
			me (idx+1) (c:reversed_)
		let r = (k, v)
		return r,

	_saMessage = M.lookup "message" (sa^.saAttributes) & fromMaybe "",
	_saSong    = M.lookup "song"    (sa^.saAttributes) & fromMaybe "",
	_saShot    = M.lookup "shot"    (sa^.saAttributes) & fromMaybe "",
	_saGoal    = M.lookup "goal"    (sa^.saAttributes) >>= parseInteger & fromMaybe 0,
	_saTime    = M.lookup "time"    (sa^.saAttributes) >>= parseInteger & fromMaybe 0,
	_saTimeHs  =
		let (hard,    afterHard)   = M.lookup "time_hs" (sa^.saAttributes) >>= parseIntegerMore & fromMaybe (sa^.saTime, "") in
		let (medium,  afterMedium) = parseIntegerMore afterHard                                 & fromMaybe (sa^.saTime, "") in
		let (easy,   _afterEasy)   = parseIntegerMore afterMedium                               & fromMaybe (sa^.saTime, "") in
		(hard, medium, easy),
	_saFastUnlockHs  =
		let (hard,    afterHard)   = M.lookup "goal_hs" (sa^.saAttributes) >>= parseIntegerMore & fromMaybe (sa^.saTime, "") in
		let (medium,  afterMedium) = parseIntegerMore afterHard                                 & fromMaybe (sa^.saTime, "") in
		let (easy,   _afterEasy)   = parseIntegerMore afterMedium                               & fromMaybe (sa^.saTime, "") in
		(hard, medium, easy),
	_saCoinHs  =
		let (hard,    afterHard)   = M.lookup "coin_hs" (sa^.saAttributes) >>= parseIntegerMore & fromMaybe (sa^.saGoal, "") in
		let (medium,  afterMedium) = parseIntegerMore afterHard                                 & fromMaybe (sa^.saGoal, "") in
		let (easy,   _afterEasy)   = parseIntegerMore afterMedium                               & fromMaybe (sa^.saGoal, "") in
		(hard, medium, easy),

	_saVersionStr = M.lookup "version" (sa^.saAttributes) & fromMaybe "",
	_saVersionNum = let (versionNum, _afterVersionNum) = parseIntegerMore (sa^.saVersionStr) & fromMaybe (0, "") in versionNum,
	_saAuthor     = M.lookup "author" (sa^.saAttributes) & fromMaybe "",
	_saBonus      = (M.lookup "bonus" (sa^.saAttributes) >>= parseInteger & fromMaybe 0) /= 0
}
	where
		asciiChar8 :: Word8 -> Char
		asciiChar8 = toEnum . fromEnum

		parseInteger :: String -> Maybe Integer
		parseInteger str = case (reads str :: [(Integer, String)]) of
			([(val, "")]) -> Just val
			_             -> Nothing

		parseIntegerMore :: String -> Maybe (Integer, String)
		parseIntegerMore str = case (reads str :: [(Integer, String)]) of
			([(val, rest)]) -> Just (val, rest)
			_               -> Nothing
