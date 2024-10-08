{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell, Arrows, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, ExplicitForAll, InstanceSigs #-}

module Immutaball.Share.GUI
	(
		-- * widgets
		Root(..), rootId,
		Label(..), labelId, labelParent, labelText,
		Button(..), buttonId, buttonParent, buttonText,
		Widget(..),
		WidgetId(..),
		WidgetParent(..),

		-- * wires
		WidgetRequest(..),
		WidgetResponse(..),
		mkGUI
	) where

import Prelude ()
import Immutaball.Prelude

--import Control.Arrow
--import Data.Functor.Identity

import Control.Lens

import Immutaball.Share.State
--import Immutaball.Share.State.Context
import Immutaball.Share.Wire

-- * widgets

data Root id = Root {
	_rootId :: id
}
	deriving (Eq, Ord, Show)
makeLenses ''Root

data Label id = Label {
	_labelId     :: id,
	_labelParent :: id,

	_labelText   :: String
}
	deriving (Eq, Ord, Show)
makeLenses ''Label

data Button id = Button {
	_buttonId     :: id,
	_buttonParent :: id,

	_buttonText   :: String
}
	deriving (Eq, Ord, Show)
makeLenses ''Button

data Widget id =
	  RootWidget   (Root   id)
	| LabelWidget  (Label  id)
	| ButtonWidget (Button id)
	deriving (Eq, Ord, Show)

class WidgetId w id | w -> id where
	widgetId :: Lens' w id

class WidgetParent w id | w -> id where
	widgetParent :: Lens' w id

instance WidgetId     (Root   id) id where widgetId     = rootId
instance WidgetParent (Root   id) id where widgetParent = rootId
instance WidgetId     (Label  id) id where widgetId     = labelId
instance WidgetParent (Label  id) id where widgetParent = labelParent
instance WidgetId     (Button id) id where widgetId     = buttonId
instance WidgetParent (Button id) id where widgetParent = buttonParent

instance WidgetId (Widget id) id where
	widgetId :: forall f. Functor f => (id -> f id) -> Widget id -> f (Widget id)
	widgetId accessor (RootWidget   root)   = RootWidget   <$> widgetId accessor root
	widgetId accessor (LabelWidget  label)  = LabelWidget  <$> widgetId accessor label
	widgetId accessor (ButtonWidget button) = ButtonWidget <$> widgetId accessor button
instance WidgetParent (Widget id) id where
	widgetParent :: forall f. Functor f => (id -> f id) -> Widget id -> f (Widget id)
	widgetParent accessor (RootWidget   root)   = RootWidget   <$> widgetParent accessor root
	widgetParent accessor (LabelWidget  label)  = LabelWidget  <$> widgetParent accessor label
	widgetParent accessor (ButtonWidget button) = ButtonWidget <$> widgetParent accessor button

-- * wires

data WidgetRequest id =
	  GUIDrive Request
	| GUISetText id String
	deriving (Eq, Ord, Show)

data WidgetResponse id =
	  NoWidgetAction
	| WidgetAction id
	deriving (Eq, Ord, Show)

-- TODO:
mkGUI :: [Widget id] -> Wire ImmutaballM (WidgetRequest id) (WidgetResponse id)
mkGUI _initialWidgets = error "TODO: unimplemented."
