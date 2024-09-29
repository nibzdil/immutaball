{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Config.Parser
	(
		nat,
		boolInt,
		strVal,
		natConf,
		boolConf,
		stringConf,
		neverballrc,
		parseNeverballrc,
		parseNeverballrc',
	) where

import Text.Parsec
import Text.Read

import Immutaball.Share.Config

nat :: (Integral i) => Parsec String () i
nat = do
	digits <- many1 digit
	let val = readMaybe digits  :: Maybe Integer
	maybe (unexpected "failed to parse nat") return (fromIntegral <$> val)

boolInt :: Parsec String () Bool
boolInt = do
	val <- nat
	return (val /= (0 :: Integer))

strVal :: Parsec String () String
strVal = do
	manyTill anyChar endOfLine

natConf :: (Integral i) => String -> Parsec String () i
natConf key = do
	string key *> space *> spaces *> nat <* endOfLine
boolConf :: String -> Parsec String () Bool
boolConf key = do
	string key *> space *> spaces *> boolInt <* endOfLine
stringConf :: String -> Parsec String () String
stringConf key = do
	string key *> space *> spaces *> strVal <* endOfLine

-- | This simple parser requires the same order.
-- TODO: support out of order.
neverballrc :: Parsec String () Config
neverballrc = Config <$>
	boolConf "fullscreen" <*>
	natConf  "display" <*>
	natConf  "width" <*>
	natConf  "height" <*>
	boolConf "stereo" <*>
	natConf  "camera" <*>
	boolConf "textures" <*>
	boolConf "reflection" <*>
	boolConf "multisample" <*>
	boolConf "mipmap" <*>
	natConf  "aniso" <*>
	boolConf "background" <*>
	boolConf "shadow" <*>
	natConf  "audio_buff" <*>
	natConf  "mouse_sense" <*>
	natConf  "mouse_response" <*>
	boolConf "mouse_invert" <*>
	boolConf "vsync" <*>
	boolConf "hmd" <*>
	boolConf "highdpi" <*>

	natConf  "mouse_camera_1" <*>
	natConf  "mouse_camera_1" <*>
	natConf  "mouse_camera_toggle" <*>
	natConf  "mouse_camera_l" <*>
	natConf  "mouse_camera_r" <*>

	natConf  "nice" <*>
	natConf  "fps" <*>
	natConf  "sound_volume" <*>
	natConf  "music_volume" <*>

	boolConf "joystick" <*>
	natConf  "joystick_response" <*>
	natConf  "joystick_axis_x0" <*>
	natConf  "joystick_axis_x1" <*>
	natConf  "joystick_axis_y0" <*>
	natConf  "joystick_axis_y1" <*>
	boolConf "joystick_axis_x0" <*>
	boolConf "joystick_axis_x1" <*>
	boolConf "joystick_axis_y0" <*>
	boolConf "joystick_axis_y1" <*>

	natConf  "joystick_button_a" <*>
	natConf  "joystick_button_b" <*>
	natConf  "joystick_button_x" <*>
	natConf  "joystick_button_y" <*>
	natConf  "joystick_button_l1" <*>
	natConf  "joystick_button_r1" <*>
	natConf  "joystick_button_l2" <*>
	natConf  "joystick_button_r2" <*>
	natConf  "joystick_button_select" <*>
	natConf  "joystick_button_start" <*>
	natConf  "joystick_dpad_l" <*>
	natConf  "joystick_dpad_r" <*>
	natConf  "joystick_dpad_u" <*>
	natConf  "joystick_dpad_d" <*>

	boolConf "wiimote_invert_pitch" <*>
	boolConf "wiimote_invert_roll" <*>
	natConf  "wiimote_pitch_sensitivity" <*>
	natConf  "wiimote_roll_sensitivity" <*>
	natConf  "wiimote_smooth_alpha" <*>
	boolConf "wiimote_hold_sideways" <*>

	natConf  "key_camera_1" <*>
	natConf  "key_camera_2" <*>
	natConf  "key_camera_3" <*>
	natConf  "key_camera_toggle" <*>
	natConf  "key_camera_r" <*>
	natConf  "key_camera_l" <*>
	natConf  "key_forward" <*>
	natConf  "key_backward" <*>
	natConf  "key_left" <*>
	natConf  "key_right" <*>
	natConf  "key_restart" <*>
	natConf  "key_score_next" <*>
	natConf  "key_rotate_fast" <*>

	natConf  "view_fov" <*>
	natConf  "view_dp" <*>
	natConf  "view_dc" <*>
	natConf  "view_dz" <*>
	natConf  "rotate_fast" <*>
	natConf  "rotate_slow" <*>
	boolConf "cheat" <*>
	boolConf "stats" <*>
	boolConf "screenshot" <*>
	boolConf "lock_goals" <*>

	natConf  "camera_1_speed" <*>
	natConf  "camera_2_speed" <*>
	natConf  "camera_3_speed" <*>

	natConf  "touch_rotate" <*>

	stringConf "player" <*>
	stringConf "ball_file" <*>
	stringConf "wiimote_addr" <*>
	stringConf "replay_name" <*>
	stringConf "language" <*>
	stringConf "theme"

parseNeverballrc :: String -> String -> Either String Config
parseNeverballrc inputName inputContents = either (Left . show) Right $ parseNeverballrc' inputName inputContents

parseNeverballrc' :: SourceName -> String -> Either ParseError Config
parseNeverballrc' inputName inputContents = parse neverballrc inputName inputContents
