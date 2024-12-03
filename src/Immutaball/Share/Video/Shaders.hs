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
		"layout(location = 1) in vec4 modulateColor;",
		"layout(location = 2) in vec2 texCoords;",
		"layout(location = 3) in int  texLayer;",
		"layout(location = 4) in int  elem;",
		"",
		"out      vec4 vertexModulateColor;",
		"out      vec2 vertexTexCoords;",
		"out flat int  vertexTexLayer;",
		"",
		"layout(location = 16) uniform int enableSceneData;",
		"",
		"// 16 SOL render analysis arrays (scene data), locations 17-32",
		"layout(std430, binding = 17) buffer layoutSSBOVertexData {",
		"\tdouble ssboVertexData[];",
		"};",
		"layout(std430, binding = 18) buffer layoutSSBOGeomData {",
		"\tint ssboGeomData[];",
		"};",
		"layout(std430, binding = 19) buffer layoutSSBOLumpData {",
		"\tint ssboLumpData[];",
		"};",
		"layout(std430, binding = 20) buffer layoutSSBOPathDoublesData {",
		"\tdouble ssboPathDoublesData[];",
		"};",
		"layout(std430, binding = 21) buffer layoutSSBOPathInt32sData {",
		"\tdouble ssboPathInt32sData[];",
		"};",
		"layout(std430, binding = 22) buffer layoutSSBOBodyData {",
		"\tint ssboBodyData[];",
		"};",
		"layout(std430, binding = 23) buffer layoutSSBOGcData {",
		"\tint ssboGcData[];",
		"};",
		"layout(std430, binding = 24) buffer layoutSSBOAllGeomPassMvData {",
		"\tint ssboAllGeomPassMvData[];",
		"};",
		"layout(std430, binding = 25) buffer layoutSSBOAllGeomPassTexturesData {",
		"\tint ssboAllGeomPassTexturesData[];",
		"};",
		"layout(std430, binding = 26) buffer layoutSSBOAllGeomPassGisData {",
		"\tint ssboAllGeomPassGisData[];",
		"};",
		"layout(std430, binding = 27) buffer layoutSSBOGeomPassMvRangesData {",
		"\tint ssboGeomPassMvRangesData[];",
		"};",
		"layout(std430, binding = 28) buffer layoutSSBOGeomPassTexturesRangesData {",
		"\tint ssboGeomPassTexturesRangesData[];",
		"};",
		"layout(std430, binding = 29) buffer layoutSSBOGeomPassGisRangesData {",
		"\tint ssboGeomPassGisRangesData[];",
		"};",
		"layout(std430, binding = 30) buffer layoutSSBOGeomPassBisData {",
		"\tint ssboGeomPassBisData[];",
		"};",
		"",
		"layout(std430, binding = 31) buffer layoutSSBOTransformationData {",
		"\tdouble ssboTransformationData[];",
		"};",
		"",
		"layout(std430, binding = 32) buffer layoutSSBOTexcoordsDoubleData {",
		"\tdouble ssboTexcoordsDoubleData[];",
		"};",
		"",
		"layout(location = 33) uniform int sceneGeomPassIdx;",
		"",
		"// Required for linking with GL_PROGRAM_SEPARABLE apparently.",
		"out gl_PerVertex {",
		"\tvec4  gl_Position;",
		"\tfloat gl_PointSize;",
		"\tfloat gl_ClipDistance[];",
		"};",
		"",
		"vec4 rowMajorMatMulVecdf(double mat[16], vec4 untransformed) {",
		"\tfloat transformedx = float(mat[0]) * untransformed.x + float(mat[1]) * untransformed.y + float(mat[2]) * untransformed.z + float(mat[3]) * untransformed.w;",
		"\tfloat transformedy = float(mat[4]) * untransformed.x + float(mat[5]) * untransformed.y + float(mat[6]) * untransformed.z + float(mat[7]) * untransformed.w;",
		"\tfloat transformedz = float(mat[8]) * untransformed.x + float(mat[9]) * untransformed.y + float(mat[10]) * untransformed.z + float(mat[11]) * untransformed.w;",
		"\tfloat transformedw = float(mat[12]) * untransformed.x + float(mat[13]) * untransformed.y + float(mat[14]) * untransformed.z + float(mat[15]) * untransformed.w;",
		"\treturn vec4(transformedx, transformedy, transformedz, transformedw);",
		"}",
		"",
		"vec4 transfMul(vec4 untransformed) {",
		"\t//vec4 transformed = rowMajorMatMulVecdf(ssboTransformationData, untransformed);",
		"\tfloat transformedx = float(ssboTransformationData[0]) * untransformed.x + float(ssboTransformationData[1]) * untransformed.y + float(ssboTransformationData[2]) * untransformed.z + float(ssboTransformationData[3]) * untransformed.w;",
		"\tfloat transformedy = float(ssboTransformationData[4]) * untransformed.x + float(ssboTransformationData[5]) * untransformed.y + float(ssboTransformationData[6]) * untransformed.z + float(ssboTransformationData[7]) * untransformed.w;",
		"\tfloat transformedz = float(ssboTransformationData[8]) * untransformed.x + float(ssboTransformationData[9]) * untransformed.y + float(ssboTransformationData[10]) * untransformed.z + float(ssboTransformationData[11]) * untransformed.w;",
		"\tfloat transformedw = float(ssboTransformationData[12]) * untransformed.x + float(ssboTransformationData[13]) * untransformed.y + float(ssboTransformationData[14]) * untransformed.z + float(ssboTransformationData[15]) * untransformed.w;",
		"\tvec4 transformed = vec4(transformedx, transformedy, transformedz, transformedw);",
		"\treturn transformed;",
		"}",
		"",
		"void mainGUI() {",
		"\tgl_Position = vec4(position, 1.0);",
		"\tvertexModulateColor = modulateColor;",
		"\tvertexTexCoords = texCoords;",
		"\tvertexTexLayer = texLayer;",
		"}",
		"",
		"void mainScene() {",
		"\t// Render elem ‘elem’ in the geom pass ‘sceneGeomPassIdx’.",
		"\tint geomPassTexture0 = ssboGeomPassTexturesRangesData[2*sceneGeomPassIdx+0];",
		"\tint geomPassTexturec = ssboGeomPassTexturesRangesData[2*sceneGeomPassIdx+1];",
		"\tint geomPassGi0      = ssboGeomPassGisRangesData[2*sceneGeomPassIdx+0];",
		"\tint geomPassGic      = ssboGeomPassGisRangesData[2*sceneGeomPassIdx+1];",
		"",
		"\tint texture = ssboGeomPassTexturesRangesData[2*sceneGeomPassIdx+1];",
		"\tint gi      = ssboGeomPassGisRangesData[2*sceneGeomPassIdx+1];",
		"",
		"\tint whichVert = elem % 3;",
		"",
		"\tint vi = ssboGeomData[9*gi+0];",
		"\tint vj = ssboGeomData[9*gi+1];",
		"\tint vk = ssboGeomData[9*gi+2];",
		"\tint ti = ssboGeomData[9*gi+3];",
		"\tint tj = ssboGeomData[9*gi+4];",
		"\tint tk = ssboGeomData[9*gi+5];",
		"\tint si = ssboGeomData[9*gi+6];",
		"\tint sj = ssboGeomData[9*gi+7];",
		"\tint sk = ssboGeomData[9*gi+8];",
		"",
		"\tint v = ssboGeomData[9*gi+0+(whichVert)];",
		"\tint t = ssboGeomData[9*gi+3+(whichVert)];",
		"\tint s = ssboGeomData[9*gi+6+(whichVert)];",
		"",
		"\t// Texture coords.",
		"\tdouble texs  = ssboTexcoordsDoubleData[2*t+0];",
		"\tdouble text  = ssboTexcoordsDoubleData[2*t+1];",
		"\tfloat  texsf = float(texs);",
		"\tfloat  textf = float(text);",
		"",
		"\tdouble vx = ssboVertexData[3*v+0];",
		"\tdouble vy = ssboVertexData[3*v+1];",
		"\tdouble vz = ssboVertexData[3*v+2];",
		"\tfloat vxf = float(vx);",
		"\tfloat vyf = float(vy);",
		"\tfloat vzf = float(vz);",
		"\tvec3 vfbase = vec3(vxf, vyf, vzf);",
		"\tvec4 vfuntransformed = vec4(vfbase, 1.0);",
		"\tvec4 vf = transfMul(vfuntransformed);",
		"\tvec4 out_gl_Position = vf;",
		"",
		"\tvec4 out_modulateColor = vec4(1.0, 1.0, 1.0, 1.0);",
		"",
		"\tvec2 out_texCoords = vec2(texsf, textf);",
		"",
		"\tint out_texLayer = texture;",
		"",
		"\tgl_Position = out_gl_Position;",
		--"\tgl_Position = whichVert==0?vec4(vf.x,0.0,0.0,1.0):(whichVert==1?vec4(0.0,1.0,0.0,1.0):vec4(1.0,1.0,0.0,1.0));",  -- TODO DEBUG
		--"\tgl_Position = vfuntransformed;",  -- TODO DEBUG
		--"\tgl_Position = (sceneGeomPassIdx%3)==0?vec4(0.0,0.0,0.0,1.0):((sceneGeomPassIdx%3)==1?vec4(0.0,1.0,0.0,1.0):vec4(1.0,1.0,0.0,1.0));",  -- TODO DEBUG
		"\tvertexModulateColor = out_modulateColor;",
		"\tvertexTexCoords = out_texCoords;",
		"\tvertexTexLayer = out_texLayer;",
		"}",
		"",
		"void main() {",
		"\tif (!(enableSceneData != 0)) {",
		"\t\tmainGUI();",
		"\t} else {",
		"\t\tmainScene();",
		"\t}",
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
		"in      vec4 vertexModulateColor;",
		"in      vec2 vertexTexCoords;",
		"in flat int  vertexTexLayer;",
		"",
		"layout(location = 16) uniform int       enableSceneData;",
		"",
		"layout(location =  0) uniform sampler2D texture0;",
		"layout(location =  1) uniform sampler2D texture1;",
		"layout(location =  2) uniform sampler2D texture2;",
		"layout(location =  3) uniform sampler2D texture3;",
		"layout(location =  4) uniform sampler2D texture4;",
		"layout(location =  5) uniform sampler2D texture5;",
		"layout(location =  6) uniform sampler2D texture6;",
		"layout(location =  7) uniform sampler2D texture7;",
		"layout(location =  8) uniform sampler2D texture8;",
		"layout(location =  9) uniform sampler2D texture9;",
		"layout(location = 10) uniform sampler2D texture10;",
		"layout(location = 11) uniform sampler2D texture11;",
		"layout(location = 12) uniform sampler2D texture12;",
		"layout(location = 13) uniform sampler2D texture13;",
		"layout(location = 14) uniform sampler2D texture14;",
		"layout(location = 15) uniform sampler2D texture15;",
		"",
		"void main() {",
		--"\tfragmentColor = texture(texture0, vertexTexCoords);",
		"\tfragmentColor = ",
		"\t\tvertexTexLayer ==  0 ? texture(texture0,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  1 ? texture(texture1,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  2 ? texture(texture2,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  3 ? texture(texture3,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  4 ? texture(texture4,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  5 ? texture(texture5,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  6 ? texture(texture6,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  7 ? texture(texture7,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  8 ? texture(texture8,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer ==  9 ? texture(texture9,  vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 10 ? texture(texture10, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 11 ? texture(texture11, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 12 ? texture(texture12, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 13 ? texture(texture13, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 14 ? texture(texture14, vertexTexCoords) * vertexModulateColor :",
		"\t\tvertexTexLayer == 15 ? texture(texture15, vertexTexCoords) * vertexModulateColor :",
		"\t\tvec4(0.0, 0.0, 0.0, 0.0);",
		"}",
		"",
		""
	]
