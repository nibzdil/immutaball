{-# OPTIONS_GHC -fno-warn-tabs #-}  -- Support tab indentation better, for a better default of no warning if tabs are used: https://dmitryfrank.com/articles/indent_with_tabs_align_with_spaces .
-- Enable warnings:
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- CLI.hs.

{-# LANGUAGE Haskell2010 #-}

module Immutaball.Share.Video.Shaders
	(
		vertexShader,
		fragmentShader
	) where

import Prelude ()
import Immutaball.Prelude

vertexShader :: String
vertexShader = unlines $
	[
		"#version 450 core",
		"",
		"layout(location = 0) in vec3 position;",
		"layout(location = 1) in vec3 color;",
		"layout(location = 2) in vec2 texCoords;",
		"",
		"out vec3 vertexColor;",
		"out vec2 vertexTexCoords;",
		"",
		"// Required for linking with GL_PROGRAM_SEPARABLE apparently.",
		"out gl_PerVertex {",
		"\tvec4  gl_Position;",
		"\tfloat gl_PointSize;",
		"\tfloat gl_ClipDistance[];",
		"};",
		"",
		"void main() {",
		"\tgl_Position = vec4(position, 1.0);",
		"\tvertexColor = color;",
		"\tvertexTexCoords = texCoords;",
		"}",
		"",
		""
	]

fragmentShader :: String
fragmentShader = unlines $
	[
		"#version 450 core",
		"",
		"out vec4 fragmentColor;",
		"",
		"in vec3 vertexColor;",
		"in vec2 vertexTexCoords;",
		"",
		"layout(binding = 0) uniform sampler2D texture0;",
		"",
		"void main() {",
		"\tfragmentColor = texture(texture0, vertexTexCoords);",
		"}",
		"",
		""
	]
