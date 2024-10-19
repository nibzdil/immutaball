{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- LevelSelect.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE Arrows, TemplateHaskell, ScopedTypeVariables #-}

module Immutaball.Ball.State.LevelSelect
	(
		mkLevelSelectState,
		LevelSelectWidget(..), AsLevelSelectWidget(..),
		levelSelectGui,
		levelSelectBaseGui,
		levelSelectButtons
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
import Control.Exception
import Control.Lens
import Control.Monad
--import Data.Functor.Identity

--import Control.Lens
--import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
--import qualified Data.Map as M
import qualified SDL.Raw.Enum as Raw
import System.FilePath

import Immutaball.Ball.LevelSets
import qualified Immutaball.Ball.State.Play as Play
import Immutaball.Share.Context
import Immutaball.Share.Context.Config
import Immutaball.Share.Level.Base
import Immutaball.Share.Level.Parser
import Immutaball.Share.GUI
import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- (Placed at beginning to avoid Template Haskell errors.)
data LevelSelectWidget =
	  LevelSelectRoot
	| BackButton
	| LevelsVstack
	| LevelButton String
	| Anonymous Integer
	deriving (Eq, Ord, Show)
makeClassyPrisms ''LevelSelectWidget

mkLevelSelectState :: LevelSet -> (Either IBContext IBStateContext -> Immutaball) -> Either IBContext IBStateContext -> Immutaball
mkLevelSelectState levelSet mkBack baseCxt0 = closeSecondI . switch . fromImmutaballSingleWith Nothing . openSecondI $ proc (Identity request) -> do
	rec
		cxtLast <- delay cxt0 -< cxt
		cxtn <- requireBasics -< (cxtLast, request)

		--let (levelSet :: LevelSet) = levelSet

		--(guiResponse, cxtnp1) <- mkGUI $ levelSelectGui levelSet -< (GUIDrive request, cxtn)
		(guiResponse, cxtnp1) <- withM (\gui -> second (mkGUI gui) >>> arr snd) (return . fst) -< (levelSelectGui levelSet, (GUIDrive request, cxtn))
		let response = ContinueResponse

		let isEsc  = (const False ||| (== (fromIntegral Raw.SDLK_ESCAPE, True))) . matching _Keybd $ request
		let isBack = isEsc || guiResponse == WidgetAction BackButton

		() <- finishFrame -< (request, cxtnp1)
		cxt <- returnA -< cxtnp1

	-- If a level was selected, parse it.
	let (toLevelPath :: Maybe String) = (const Nothing ||| Just) . matching (_WidgetAction . _LevelButton) $ guiResponse
	let (toLevelPath' :: Maybe String) = ((cxt^.ibContext.ibDirs.ibStaticDataDir) </>) <$> toLevelPath
	(mtoLevelContents :: Maybe (Either IOException BL.ByteString)) <- monadic -< maybe (pure Nothing) (\path -> (Just <$>) . liftIBIO . BasicIBIOF $ ReadBytesSync path id) $ toLevelPath'
	(toLevelContents :: Maybe BL.ByteString) <- monadic -< maybe (pure Nothing) (\mcontents -> liftIBIO . ThrowIO ||| pure . Just $ mcontents) $ mtoLevelContents
	let (toLevelParse :: Maybe (Either LevelIBParseException LevelIB)) = parseLevelFile' <$> toLevelPath <*> toLevelContents
	(toLevel :: Maybe LevelIB) <- monadic -< maybe (pure Nothing) (liftIBIO . ThrowIO ||| pure . Just) $ toLevelParse

	-- Switch to a level.
	let switchTo0 = flip fmap toLevel $ \level -> openSecondI $ Play.mkPlayState levelSet level (mkLevelSelectState levelSet mkBack) (Right cxt)
	{-
	let switchTo0 = if' (isJust toLevel) Nothing   . Just . openSecondI $ mkPlayState levelSet level (mkLevelSelectState levelSet mkBack) (Right cxt)
	-}
	-- Switch on Back button.
	let switchTo  = if' (not    isBack ) switchTo0 . Just . openSecondI $ mkBack (Right cxt)
	returnA -< (Identity response, switchTo)

	where
		cxt0 = either initialStateCxt id baseCxt0
		useUnsafeVersion = False
		parseLevelFile' = if' useUnsafeVersion (\p -> unsafeParseLevelFileRaw p . BL.toStrict) parseLevelFile

-- TODO: make a better UI.  For now we just have a simple list of levels.

levelSelectGui :: LevelSet -> [Widget LevelSelectWidget]
levelSelectGui levelSet =
	levelSelectBaseGui ++
	levelSelectButtons levelSet

levelSelectBaseGui :: [Widget LevelSelectWidget]
levelSelectBaseGui =
	[
		RootWidget   $ Root   { _rootWid   = LevelSelectRoot                                },
		ButtonWidget $ Button { _buttonWid = BackButton, _buttonWparent = LevelSelectRoot,
			_buttonText = "Back", _buttonRect = Just $ Rect (Vec2 (-0.800) (0.720)) (Vec2 (-0.700) (0.800)) },
		VstackWidget $ Vstack { _vstackWid = LevelsVstack, _vstackWparent = LevelSelectRoot }
	]

levelSelectButtons :: LevelSet -> [Widget LevelSelectWidget]
levelSelectButtons levelSet = flip map (zip [0..] (levelSet^.lsLevels)) $ \((idx :: Integer), path) ->
	let (r, c) = join (***) fromIntegral $ idx `divMod` 5 in
	ButtonWidget $ Button { _buttonWid = LevelButton path, _buttonWparent = LevelsVstack,
		_buttonText = "Level", _buttonRect = Just $ Rect (Vec2 (-0.165 + c*0.070) (0.160 - r*0.100)) (Vec2 (-0.115 + c*0.070) (0.240 - r*0.100)) }

--makeClassyPrisms ''LevelSelectWidget
