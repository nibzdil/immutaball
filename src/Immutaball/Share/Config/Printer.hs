{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- State.hs.

{-# LANGUAGE Haskell2010, RankNTypes #-}

module Immutaball.Share.Config.Printer
	(
		boolToInt,
		showNatConf,
		showBoolConf,
		showStringConf,
		showNeverballrc
	) where

import Data.List
import Text.Printf

import Control.Lens

import Immutaball.Share.Config

boolToInt :: (Integral i) => Bool -> i
boolToInt True = fromIntegral (1 :: Integer)
boolToInt False = fromIntegral (0 :: Integer)

showNatConf :: (Integral i, Show i) => Getter Neverballrc i -> String -> Neverballrc -> String
showNatConf field key cfg = printf "%-25s %s" key $ show (cfg^.field)

showBoolConf :: Getter Neverballrc Bool -> String -> Neverballrc -> String
showBoolConf field key cfg = printf "%-25s %s" key $ show (boolToInt $ cfg^.field  :: Integer)

showStringConf :: Getter Neverballrc String -> String -> Neverballrc -> String
showStringConf field key cfg = printf "%-25s %s" key $ cfg^.field

showNeverballrc :: Neverballrc -> String
showNeverballrc cfg = intercalate "\n" . map ($ cfg) $
	[
		showBoolConf fullscreen  "fullscreen",
		showNatConf  display     "display",
		showNatConf  width       "width",
		showNatConf  height      "height",
		showBoolConf stereo      "stereo",
		showNatConf  camera      "camera",
		showBoolConf textures    "textures",
		showBoolConf reflection  "reflection",
		showBoolConf multisample "multisample",
		showBoolConf mipmap      "mipmap",
		showNatConf  aniso       "aniso",
		showBoolConf background  "background",
		showBoolConf shadow      "shadow",
		showNatConf  audioBuff   "audio_buff",
		showNatConf  mouseSense  "mouse_sense",
		showNatConf  mouseResp   "mouse_response",
		showBoolConf mouseInvert "mouse_invert",
		showBoolConf mouseVsync  "vsync",
		showBoolConf hmd         "hmd",
		showBoolConf highdpi     "highdpi",

		showNatConf  mouseCamera1      "mouse_camera_1",
		showNatConf  mouseCamera2      "mouse_camera_1",
		showNatConf  mouseCameraToggle "mouse_camera_toggle",
		showNatConf  mouseCameraL      "mouse_camera_l",
		showNatConf  mouseCameraR      "mouse_camera_r",

		showNatConf  nice        "nice",
		showNatConf  fps         "fps",
		showNatConf  soundVolume "sound_volume",
		showNatConf  musicVolume "music_volume",

		showBoolConf joystick             "joystick",
		showNatConf  joystickResp         "joystick_response",
		showNatConf  joystickAxisX0       "joystick_axis_x0",
		showNatConf  joystickAxisY0       "joystick_axis_x1",
		showNatConf  joystickAxisX1       "joystick_axis_y0",
		showNatConf  joystickAxisY1       "joystick_axis_y1",
		showBoolConf joystickAxisX0Invert "joystick_axis_x0",
		showBoolConf joystickAxisY0Invert "joystick_axis_x1",
		showBoolConf joystickAxisX1Invert "joystick_axis_y0",
		showBoolConf joystickAxisY1Invert "joystick_axis_y1",

		showNatConf  joystickButtonA      "joystick_button_a",
		showNatConf  joystickButtonB      "joystick_button_b",
		showNatConf  joystickButtonX      "joystick_button_x",
		showNatConf  joystickButtonY      "joystick_button_y",
		showNatConf  joystickButtonL1     "joystick_button_l1",
		showNatConf  joystickButtonL2     "joystick_button_r1",
		showNatConf  joystickButtonR1     "joystick_button_l2",
		showNatConf  joystickButtonR2     "joystick_button_r2",
		showNatConf  joystickButtonSelect "joystick_button_select",
		showNatConf  joystickButtonStart  "joystick_button_start",
		showNatConf  joystickDpadL        "joystick_dpad_l",
		showNatConf  joystickDpadR        "joystick_dpad_r",
		showNatConf  joystickDpadU        "joystick_dpad_u",
		showNatConf  joystickDpadD        "joystick_dpad_d",

		showBoolConf wiimoteInvertPitch  "wiimote_invert_pitch",
		showBoolConf wiimoteInvertRoll   "wiimote_invert_roll",
		showNatConf  wiimotePitchSens    "wiimote_pitch_sensitivity",
		showNatConf  wiimoteRollSense    "wiimote_roll_sensitivity",
		showNatConf  wiimoteSmoothAlpha  "wiimote_smooth_alpha",
		showBoolConf wiimoteHoldSideways "wiimote_hold_sideways",

		showNatConf  keyCamera1      "key_camera_1",
		showNatConf  keyCamera2      "key_camera_2",
		showNatConf  keyCamera3      "key_camera_3",
		showNatConf  keyCameraToggle "key_camera_toggle",
		showNatConf  keyCameraR      "key_camera_r",
		showNatConf  keyCameraL      "key_camera_l",
		showNatConf  keyForward      "key_forward",
		showNatConf  keyBackward     "key_backward",
		showNatConf  keyLeft         "key_left",
		showNatConf  keyRight        "key_right",
		showNatConf  keyRestart      "key_restart",
		showNatConf  keyScoreNext    "key_score_next",
		showNatConf  keyRotateFast   "key_rotate_fast",

		showNatConf  viewFov    "view_fov",
		showNatConf  viewDp     "view_dp",
		showNatConf  viewDc     "view_dc",
		showNatConf  viewDz     "view_dz",
		showNatConf  rotateFast "rotate_fast",
		showNatConf  rotateSlow "rotate_slow",
		showBoolConf cheat      "cheat",
		showBoolConf stats      "stats",
		showBoolConf screenshot "screenshot",
		showBoolConf lockGoals  "lock_goals",

		showNatConf  camera1Speed "camera_1_speed",
		showNatConf  camera2Speed "camera_2_speed",
		showNatConf  camera3Speed "camera_3_speed",

		showNatConf  cameraTouchRotate "touch_rotate",

		showStringConf player      "player",
		showStringConf ballFile    "ball_file",
		showStringConf wiimoteAddr "wiimote_addr",
		showStringConf replayName  "replay_name",
		showStringConf language    "language",
		showStringConf theme       "theme"
	]
