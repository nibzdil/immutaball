{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExplicitForAll, InstanceSigs, ScopedTypeVariables #-}

module Immutaball.Share.GUI
	(
		-- * widgets
		HasWid(..),
		HasWparent(..),
		HasFill(..),
		HasText(..),
		HasRect(..),
		Root(..), rootWid,
		Space(..), spaceWid, spaceWparent, spaceFill,
		Vstack(..), vstackWid, vstackWparent,
		Hstack(..), hstackWid, hstackWparent,
		Label(..), labelWid, labelWparent, labelText, labelRect,
		Button(..), buttonWid, buttonWparent, buttonText, buttonRect,
		Widget(..), AsWidget(..),

		-- * wires
		WidgetRequest(..), AsWidgetRequest(..),
		WidgetResponse(..), AsWidgetResponse(..),
		mkGUI,
		mkWidgetsAnalysis,
		mkWidgetBy,
		mkWidgetIdx,
		mkWidgetToIdx,
		mkGetChildren,
		mkGeometry,
		nextWidgetDirect,
		prevWidgetDirect,
		nextWidgetHier,
		prevWidgetHier,
		isSelectable,
		guiPaint,
		guiPaintWidget,
		focusDecayTime,
		focusScale
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
--import Data.Functor.Identity
import Data.Maybe

import Control.Lens
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified SDL.Raw.Enum as Raw

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.Math
import Immutaball.Share.State
import Immutaball.Share.State.Context
import Immutaball.Share.Utils
import Immutaball.Share.Wire

-- TODO: reomve this debugging.  Basically I've written a lot without being
-- able to see anything, and I just want to test our GUI before I write more advanced OpenGL.
import System.IO.Unsafe (unsafePerformIO)
import Graphics.GL.Internal.Shared as GL

-- * widgets

data Root id = Root {
	_rootWid :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Root
makeFields ''Root

data Space id = Space {
	_spaceWid     :: id,
	_spaceWparent :: id,

	-- | Max space to anchor at bottom.
	_spaceFill    :: Bool
}
	deriving (Eq, Ord, Show)
makeLenses ''Space
makeFields ''Space

instance HasWparent (Root id) id where wparent = wid

data Vstack id = Vstack {
	_vstackWid     :: id,
	_vstackWparent :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Vstack
makeFields ''Vstack

data Hstack id = Hstack {
	_hstackWid     :: id,
	_hstackWparent :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Hstack
makeFields ''Hstack

data Label id = Label {
	_labelWid     :: id,
	_labelWparent :: id,

	_labelText    :: String,
	_labelRect    :: Maybe (Rect Float)
}
	deriving (Eq, Ord, Show)
makeLenses ''Label
makeFields ''Label

data Button id = Button {
	_buttonWid     :: id,
	_buttonWparent :: id,

	_buttonText    :: String,
	_buttonRect    :: Maybe (Rect Float)
}
	deriving (Eq, Ord, Show)
makeLenses ''Button
makeFields ''Button

data Widget id =
	  RootWidget   (Root   id)
	| SpaceWidget  (Space  id)
	| VstackWidget (Vstack id)
	| HstackWidget (Hstack id)
	| LabelWidget  (Label  id)
	| ButtonWidget (Button id)
	deriving (Eq, Ord, Show)
makeClassyPrisms ''Widget

instance HasWid (Widget id) id where
	wid :: forall f. Functor f => (id -> f id) -> Widget id -> f (Widget id)
	wid accessor (RootWidget   root)   = RootWidget   <$> wid accessor root
	wid accessor (SpaceWidget  space)  = SpaceWidget  <$> wid accessor space
	wid accessor (VstackWidget vstack) = VstackWidget <$> wid accessor vstack
	wid accessor (HstackWidget hstack) = HstackWidget <$> wid accessor hstack
	wid accessor (LabelWidget  label)  = LabelWidget  <$> wid accessor label
	wid accessor (ButtonWidget button) = ButtonWidget <$> wid accessor button
instance HasWparent (Widget id) id where
	wparent :: forall f. Functor f => (id -> f id) -> Widget id -> f (Widget id)
	wparent accessor (RootWidget   root)   = RootWidget   <$> wparent accessor root
	wparent accessor (SpaceWidget  space)  = SpaceWidget  <$> wparent accessor space
	wparent accessor (VstackWidget vstack) = VstackWidget <$> wparent accessor vstack
	wparent accessor (HstackWidget hstack) = HstackWidget <$> wparent accessor hstack
	wparent accessor (LabelWidget  label)  = LabelWidget  <$> wparent accessor label
	wparent accessor (ButtonWidget button) = ButtonWidget <$> wparent accessor button

-- * wires

data WidgetRequest id =
	  GUIDrive Request
	| GUISetText id String
	| GUISetFocus id
	| ResetGUI [Widget id]
	deriving (Eq, Ord, Show)
makeClassyPrisms ''WidgetRequest

data WidgetResponse id =
	  NoWidgetAction
	| WidgetAction id
	deriving (Eq, Ord, Show)
makeClassyPrisms ''WidgetResponse

-- TODO: handle mouse input.
-- TODO: paint.

mkGUI :: forall id. (Eq id, Ord id) => [Widget id] -> Wire ImmutaballM (WidgetRequest id, IBStateContext) (WidgetResponse id, IBStateContext)
mkGUI initialWidgets = proc (request, cxtn) -> do
	-- Set up widgets.
	resetWidgets <- returnA -< either (const Nothing) Just $ matching _ResetGUI request
	widgets <- hold initialWidgets -< resetWidgets

	-- Analyze widgets.
	widgetsReq  <- returnA -< resetWidgets
	widgetBy    <- mkWidgetsAnalysis' mkWidgetBy    -< widgetsReq
	widgetIdx   <- mkWidgetsAnalysis' mkWidgetIdx   -< widgetsReq
	widgetToIdx <- mkWidgetsAnalysis' mkWidgetToIdx -< widgetsReq
	getChildren <- mkWidgetsAnalysis' mkGetChildren -< widgetsReq
	geometry   <- mkWidgetsAnalysis' mkGeometry    -< widgetsReq

	nextWidgetHier' <- returnA -< nextWidgetHier widgetBy getChildren
	prevWidgetHier' <- returnA -< prevWidgetHier widgetBy getChildren

	-- Manage focus.
	rec
		initialFocus <- returnA -< maybe 0 id $ flip M.lookup widgetIdx 0 >>= flip M.lookup widgetBy . nextWidgetHier' . prevWidgetHier' . (^.wid) >>= flip M.lookup widgetToIdx
		currentFocus <- holdWith -< (newFocus, initialFocus)
		lastFocus <- delayWith -< (currentFocus, initialFocus)
		newFocus <- case request of
			GUISetFocus wid_ -> returnA -< flip M.lookup widgetBy wid_ >>= flip M.lookup widgetToIdx
			GUIDrive (Keybd char True) -> returnA -<
				if' (char == fromIntegral Raw.SDLK_DOWN) (Just $ nextWidgetDirect widgetIdx lastFocus) .
				if' (char == fromIntegral Raw.SDLK_UP  ) (Just $ prevWidgetDirect widgetIdx lastFocus) $
				Nothing
			_ -> returnA -< Nothing

		-- Find 'widgetsFocusedSinceLastPaintIdx'.
		isPaint <- returnA -< case request of (GUIDrive (Paint _t)) -> True; _ -> False
		lastIsPaint <- delay False -< isPaint
		lastWidgetsFocusedSinceLastPaintIdx <- delay [] -< widgetsFocusedSinceLastPaintIdx
		(widgetsFocusedSinceLastPaintIdx :: [Integer]) <- returnA -<
			catMaybes [newFocus] ++ if' lastIsPaint [] lastWidgetsFocusedSinceLastPaintIdx
		(widgetsFocusedSinceLastPaint :: [id]) <- returnA -<
			flip mapMaybe widgetsFocusedSinceLastPaintIdx $ \idx -> (^.wid) <$> flip M.lookup widgetIdx idx

	-- Paint.
	cxtnp1 <- case request of
		GUIDrive (Paint t) -> guiPaint -< (widgets, geometry, widgetBy, widgetsFocusedSinceLastPaint, t, cxtn)
		_ -> returnA -< cxtn

	-- Set up response.
	response <- returnA -< case request of
		GUIDrive (Keybd char True) ->
			if' (char == fromIntegral Raw.SDLK_RETURN) (maybe NoWidgetAction id $ WidgetAction . (^.wid) <$> flip M.lookup widgetIdx currentFocus) $
			NoWidgetAction
		_ -> NoWidgetAction
	returnA -< (response, cxtnp1)
	where
		mkWidgetsAnalysis' = mkWidgetsAnalysis initialWidgets
		_warn :: String -> Wire ImmutaballM () ()
		_warn msg = proc () -> do
			() <- monadic -< liftIBIO . BasicIBIOF $ PutStrLn msg ()
			returnA -< ()

-- | Only recalculates the analysis on reset.
mkWidgetsAnalysis :: [Widget id] -> ([Widget id] -> a) -> Wire ImmutaballM (Maybe [Widget id]) a
mkWidgetsAnalysis initialWidgets analyzer = proc resetWidgets -> do
	hold (analyzer initialWidgets) -< analyzer <$> resetWidgets

mkWidgetBy :: (Eq id, Ord id) => [Widget id] -> M.Map id (Widget id)
mkWidgetBy widgets = M.fromList . flip map widgets $ \w -> ((w^.wid), w)

mkWidgetIdx :: [Widget id] -> M.Map Integer (Widget id)
mkWidgetIdx widgets = M.fromList $ zip [0..] widgets

mkWidgetToIdx :: (Eq id, Ord id) => [Widget id] -> M.Map (Widget id) Integer
mkWidgetToIdx widgets = M.fromList $ zip widgets [0..]

mkGetChildren :: (Eq id) => [Widget id] -> id -> [Widget id]
mkGetChildren widgets wid_ = filter (\w -> (w^.wid) == wid_) widgets

-- TODO: make a more sophisticated geometry.
-- For now I'll just specify the rect by hand.
mkGeometry :: forall id. (Eq id, Ord id) => [Widget id] -> M.Map id (Rect Float)
--mkGeometry widgets = M.fromList . flip mapMaybe widgets . (\(a,b)->(a,)<$>b) . (id &&&) $ \w -> case w of
mkGeometry widgets = M.fromList . map (first (^.wid)) . flip mapMaybe widgets . (uncurry fmap .) . ((,) &&&) $ \w -> case w of
	(LabelWidget label) -> label^.labelRect
	(ButtonWidget button) -> button^.buttonRect
	_ -> Nothing

-- | The order is the order reflected in the input widgets.
nextWidgetDirect :: (Eq id, Ord id) => M.Map Integer (Widget id) -> Integer -> Integer
nextWidgetDirect widgetIdx idx = tryNextWidget (idx+1)
	where tryNextWidget idx_
		| idx_ == idx = idx_
		| idx_ >= (fromIntegral $ M.size widgetIdx) = tryNextWidget 0
		| maybe False isSelectable $ M.lookup idx_ widgetIdx = idx_
		| otherwise = tryNextWidget (idx_+1)
prevWidgetDirect :: (Eq id, Ord id) => M.Map Integer (Widget id) -> Integer -> Integer
prevWidgetDirect widgetIdx idx = tryNextWidget (idx-1)
	where tryNextWidget idx_
		| idx_ == idx = idx_
		| idx_ < 0 = tryNextWidget ((fromIntegral $ M.size widgetIdx)-1)
		| maybe False isSelectable $ M.lookup idx_ widgetIdx = idx_
		| otherwise = tryNextWidget (idx_-1)

-- | The order is input order except container hierarchy takes precedence.
-- Does not check for cycles.
nextWidgetHier :: (Eq id, Ord id) => M.Map id (Widget id) -> (id -> [Widget id]) -> id -> id
nextWidgetHier widgetBy getChildren wid0 = maybe wid0 (nextWidgetUnder True (Just wid0) . (^.wparent)) $ M.lookup wid0 widgetBy
	where
		wBy = flip M.lookup widgetBy
		nextWidgetUnder isFirst mwid parent
			| mwid == Just wid0 = wid0
			| Nothing <- mwid
			, Just (rw@RootWidget {}) <- wBy parent =
				nextWidgetUnder isFirst (Just (rw^.wid)) parent
			| otherwise = withRemaining . maybe id (\wid_ -> drop 1 . dropWhile (\w -> (w^.wid) /= wid_)) mwid $ getChildren parent
			where
				withRemaining []                                = maybe wid0 (\parent_ -> nextWidgetUnder False (Just (parent_^.wid)) (parent_^.wparent)) $ wBy parent
				withRemaining (w:_remaining) | (w^.wid) == wid0 = w^.wid
				withRemaining (w:_remaining) | isSelectable w   = w^.wid
				withRemaining (w:_remaining) | otherwise        = nextWidgetUnder False Nothing (w^.wid)
prevWidgetHier :: (Eq id, Ord id) => M.Map id (Widget id) -> (id -> [Widget id]) -> id -> id
prevWidgetHier widgetBy getChildren wid0 = maybe wid0 (prevWidgetUnder True (Just wid0) . (^.wparent)) $ M.lookup wid0 widgetBy
	where
		wBy = flip M.lookup widgetBy
		prevWidgetUnder isFirst mwid parent
			| mwid == Just wid0 = wid0
			| Nothing <- mwid
			, Just (rw@RootWidget {}) <- wBy parent =
				prevWidgetUnder isFirst (Just (rw^.wid)) parent
			| otherwise = withRemaining . maybe id (\wid_ -> drop 1 . dropWhile (\w -> (w^.wid) /= wid_) . reverse) mwid $ getChildren parent
			where
				withRemaining []                                = maybe wid0 (\parent_ -> prevWidgetUnder False (Just (parent_^.wid)) (parent_^.wparent)) $ wBy parent
				withRemaining (w:_remaining) | (w^.wid) == wid0 = w^.wid
				withRemaining (w:_remaining) | isSelectable w   = w^.wid
				withRemaining (w:_remaining) | otherwise        = prevWidgetUnder False Nothing (w^.wid)

isSelectable :: Widget id -> Bool
isSelectable (ButtonWidget {}) = True
isSelectable _                 = False

guiPaint :: forall id. (Eq id, Ord id) => Wire ImmutaballM ([Widget id], M.Map id (Rect Float), M.Map id (Widget id), [id], Float, IBStateContext) IBStateContext
guiPaint = proc (widgets, geometry, widgetIdx, widgetsFocusedSinceLastPaint, t, cxtn) -> do
	_dt <- differentiate -< t
	rec
		(widgetLastFocusLast :: M.Map id Float) <- delay M.empty -< widgetLastFocus
		widgetLastFocus <- returnA -< M.filter (< t + focusDecayTime) $ foldr (\wid_ -> M.insert wid_ t) widgetLastFocusLast widgetsFocusedSinceLastPaint

	cxtnp1 <- foldrA guiPaintWidget -< (cxtn, flip map widgets $ \w -> (w, widgetLastFocus, geometry, widgetIdx, t))
	returnA -< cxtnp1

guiPaintWidget :: Wire ImmutaballM ((Widget id, M.Map id Float, M.Map id (Rect Float), M.Map id (Widget id), Float), IBStateContext) IBStateContext
guiPaintWidget = proc ((widget, widgetLastFocus, geometry, widgetIdx, t), cxtn) -> do
	_dt <- differentiate -< t
	-- TODO: clean up textures; just debugging for now to test what I have so far.
	-- TODO: remove debugging.  I just want to test what I have so far before I write more advanced OpenGL.
	-- TODO test
	case widget of
		(ButtonWidget button) -> do
			(((w, h), name), cxtnp1) <- cachingRenderText -< (T.pack $ button^.text, cxtn)
			let (Just r@(Rect (Vec2 ax ay) (Vec2 bx by))) = button^.rect
			-- TODO: remove debugging.  I just want to test what I have so far before I write more advanced OpenGL.
			() <- monadic -< unsafePerformIO $ do
				return $ pure ()
			returnA -< cxtnp1
		_ -> returnA -< cxtn

-- Optionally this could be moved to static config.
focusDecayTime :: Float
focusDecayTime = 0.25

focusScale :: Float
focusScale = 1.20
