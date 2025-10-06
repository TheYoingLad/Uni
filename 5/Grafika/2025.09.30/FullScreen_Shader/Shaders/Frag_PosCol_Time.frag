#version 420

// pipeline-ból bejövő per-fragment attribútumok 
in vec4 color;
in vec2 pos;

// kimenő érték - a fragment színe 
out vec4 outputColor;

// !!!!! VARÁZSLAT !!!!
// Erről bővebben később...
uniform float ElapsedTimeInSec = 0.0;
uniform float ScreenRatioX = 0.0;
uniform float ScreenRatioY = 0.0;
// !!!!!!!!!!!!

const float epsilon = 0.005;

void main()
{
//	outputColor = color;

//	outputColor = vec4(pos.x / 2 + 0.5, pos.y / 2 + 0.5, 0, 1);
//	outputColor = vec4(pos.xy / 2 + 0.5, 0, 1);

//	if(pow(pos.x, 2) + pow(pos.y, 2) >= 1){
//		outputColor = vec4(1);
//	} else outputColor = vec4(vec3(0),1);

//	if(pow(pos.x * ScreenRatioX, 2) + pow(pos.y * ScreenRatioY, 2) >= 1){
//		outputColor = vec4(1);
//	} else outputColor = vec4(vec3(0),1);

	if(abs(length(pos * vec2(ScreenRatioX, ScreenRatioY)) - 1) >= epsilon){
		outputColor = vec4(1);
	} else outputColor = vec4(vec3(0),1);
}