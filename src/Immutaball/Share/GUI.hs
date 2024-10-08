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
		Root(..), rootWid,
		Space(..), spaceWid, spaceWparent, spaceFill,
		Vstack(..), vstackWid, vstackWparent,
		Hstack(..), hstackWid, hstackWparent,
		Label(..), labelWid, labelWparent, labelText,
		Button(..), buttonWid, buttonWparent, buttonText,
		Widget(..), AsWidget(..),

		-- * wires
		WidgetRequest(..), AsWidgetRequest(..),
		WidgetResponse(..), AsWidgetResponse(..),
		mkGUI
	) where

import Prelude ()
import Immutaball.Prelude

import Control.Arrow
--import Data.Functor.Identity

import Control.Lens
import qualified Data.Map.Lazy as M

import Immutaball.Share.ImmutaballIO
import Immutaball.Share.ImmutaballIO.BasicIO
import Immutaball.Share.State
--import Immutaball.Share.State.Context
import Immutaball.Share.Wire

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

	_labelText    :: String
}
	deriving (Eq, Ord, Show)
makeLenses ''Label
makeFields ''Label

data Button id = Button {
	_buttonWid     :: id,
	_buttonWparent :: id,

	_buttonText    :: String
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

-- TODO:
mkGUI :: forall id. (Eq id, Ord id) => [Widget id] -> Wire ImmutaballM (WidgetRequest id) (WidgetResponse id)
mkGUI initialWidgets = proc request -> do
	widgets <- hold initialWidgets -< either (const Nothing) Just $ matching _ResetGUI request
	widgetsReq <- returnA -< (widgets, request)
	_widgetBy <- mkWidgetsAnalysis mkWidgetBy -< widgetsReq
	_widgetIdx <- mkWidgetsAnalysis mkWidgetIdx -< widgetsReq
	_getChildren <- mkWidgetsAnalysis mkGetChildren -< widgetsReq
	-- TODO:
	returnA -< NoWidgetAction
	where
		-- | Only recalculates the analysis on reset.
		mkWidgetsAnalysis :: ([Widget id] -> a) -> Wire ImmutaballM ([Widget id], WidgetRequest id) a
		mkWidgetsAnalysis analyzer = do proc (widgets, request) -> hold (analyzer initialWidgets) -< either (const Nothing) (const . Just $ analyzer widgets) $ matching _ResetGUI request
		mkWidgetBy :: (Eq id, Ord id) => [Widget id] -> M.Map id (Widget id)
		mkWidgetBy widgets = M.fromList . flip map widgets $ \w -> ((w^.wid), w)
		mkWidgetIdx :: [Widget id] -> M.Map Integer (Widget id)
		mkWidgetIdx widgets = M.fromList $ zip [0..] widgets
		_warn :: String -> Wire ImmutaballM () ()
		_warn msg = proc () -> do
			_ <- monadic -< pure . liftIBIO . BasicImmutaballIOF $ PutStrLn msg ()
			returnA -< ()
		mkGetChildren :: (Eq id) => [Widget id] -> id -> [Widget id]
		mkGetChildren widgets wid_ = filter (\w -> (w^.wid) == wid_) widgets
