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
		intConf,
		boolConf,
		stringConf,
		neverballrc,
		parseNeverballrc,
		parseNeverballrc',
	) where

import Control.Monad

import Text.Parsec
import Text.Read

import Immutaball.Share.Config

nat :: (Integral i) => Parsec String () i
nat = do
	digits <- many1 digit
	let val = readMaybe digits  :: Maybe Integer
	maybe (unexpected "failed to parse nat") return (fromIntegral <$> val)

int :: (Integral i) => Parsec String () i
int = do
	sign0 <- optionMaybe (char '-')
	sign1 <- optionMaybe (char '+')
	let sign0f = maybe id (const negate) sign0
	let sign1f = maybe id (const id)     sign1
	let next | (Nothing, Nothing) <- (sign0, sign1) = nat | otherwise = int
	sign0f . sign1f <$> next

boolInt :: Parsec String () Bool
boolInt = do
	val <- nat
	return (val /= (0 :: Integer))

strVal :: Parsec String () String
strVal = do
	manyTill anyChar (lookAhead . try $ endOfLine)

intConf :: (Integral i) => String -> Parsec String () i
intConf key = do
	string key *> space *> spaces *> int <* endOfLine
boolConf :: String -> Parsec String () Bool
boolConf key = do
	string key *> space *> spaces *> boolInt <* endOfLine
stringConf' :: Bool -> String -> Parsec String () String
stringConf' allowNospaceEol key = do
	--string key *> space' *> spaces *> strVal <* endOfLine
	string key *> space' *> manyTill space (lookAhead . try $ void endOfLine <|> void strVal) *> strVal <* endOfLine
	where space' | not allowNospaceEol = void (notFollowedBy endOfLine *> space) | otherwise = pure ()
stringConf :: String -> Parsec String () String
stringConf = stringConf' True

-- | This simple parser requires the same order.
-- TODO: support out of order.
neverballrc :: Parsec String () Config
neverballrc = Config <$>
	boolConf "fullscreen" <*>
	intConf  "display" <*>
	intConf  "width" <*>
	intConf  "height" <*>
	boolConf "stereo" <*>
	intConf  "camera" <*>
	boolConf "textures" <*>
	boolConf "reflection" <*>
	boolConf "multisample" <*>
	boolConf "mipmap" <*>
	intConf  "aniso" <*>
	boolConf "background" <*>
	boolConf "shadow" <*>
	intConf  "audio_buff" <*>
	intConf  "mouse_sense" <*>
	intConf  "mouse_response" <*>
	boolConf "mouse_invert" <*>
	boolConf "vsync" <*>
	boolConf "hmd" <*>
	boolConf "highdpi" <*>

	intConf  "mouse_camera_1" <*>
	intConf  "mouse_camera_1" <*>
	intConf  "mouse_camera_toggle" <*>
	intConf  "mouse_camera_l" <*>
	intConf  "mouse_camera_r" <*>

	intConf  "nice" <*>
	intConf  "fps" <*>
	intConf  "sound_volume" <*>
	intConf  "music_volume" <*>

	boolConf "joystick" <*>
	intConf  "joystick_response" <*>
	intConf  "joystick_axis_x0" <*>
	intConf  "joystick_axis_x1" <*>
	intConf  "joystick_axis_y0" <*>
	intConf  "joystick_axis_y1" <*>
	boolConf "joystick_axis_x0" <*>
	boolConf "joystick_axis_x1" <*>
	boolConf "joystick_axis_y0" <*>
	boolConf "joystick_axis_y1" <*>

	intConf  "joystick_button_a" <*>
	intConf  "joystick_button_b" <*>
	intConf  "joystick_button_x" <*>
	intConf  "joystick_button_y" <*>
	intConf  "joystick_button_l1" <*>
	intConf  "joystick_button_r1" <*>
	intConf  "joystick_button_l2" <*>
	intConf  "joystick_button_r2" <*>
	intConf  "joystick_button_select" <*>
	intConf  "joystick_button_start" <*>
	intConf  "joystick_dpad_l" <*>
	intConf  "joystick_dpad_r" <*>
	intConf  "joystick_dpad_u" <*>
	intConf  "joystick_dpad_d" <*>

	boolConf "wiimote_invert_pitch" <*>
	boolConf "wiimote_invert_roll" <*>
	intConf  "wiimote_pitch_sensitivity" <*>
	intConf  "wiimote_roll_sensitivity" <*>
	intConf  "wiimote_smooth_alpha" <*>
	boolConf "wiimote_hold_sideways" <*>

	intConf  "key_camera_1" <*>
	intConf  "key_camera_2" <*>
	intConf  "key_camera_3" <*>
	intConf  "key_camera_toggle" <*>
	intConf  "key_camera_r" <*>
	intConf  "key_camera_l" <*>
	intConf  "key_forward" <*>
	intConf  "key_backward" <*>
	intConf  "key_left" <*>
	intConf  "key_right" <*>
	intConf  "key_restart" <*>
	intConf  "key_score_next" <*>
	intConf  "key_rotate_fast" <*>

	intConf  "view_fov" <*>
	intConf  "view_dp" <*>
	intConf  "view_dc" <*>
	intConf  "view_dz" <*>
	intConf  "rotate_fast" <*>
	intConf  "rotate_slow" <*>
	boolConf "cheat" <*>
	boolConf "stats" <*>
	boolConf "screenshot" <*>
	boolConf "lock_goals" <*>

	intConf  "camera_1_speed" <*>
	intConf  "camera_2_speed" <*>
	intConf  "camera_3_speed" <*>

	intConf  "touch_rotate" <*>

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
