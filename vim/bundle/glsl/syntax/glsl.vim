if exists("b:current_syntax")
	finish
endif

syntax keyword glslKeyword layout out in uniform void

syntax keyword glslType vec2 vec3 vec4 mat4 sampler2D float

syntax match glslPreProc "\v#.*$"

syntax match glslComment "\v\/\/.*$"

syntax match glslNumber "\v<\d*>"
syntax match glslNumber "\v<0+x\w+>"
syntax match glslNumber "\v<\d*\.\d*f=>"
syntax match glslNumber "\v<\d+\.=\d*e[-+]\d+>"

syntax region glslString start=/\v["']/ skip=/\v\\./ end=/\v["']/

highlight link glslKeyword	Keyword
highlight link glslType		Type
highlight link glslPreProc	PreProc
highlight link glslComment	Comment
highlight link glslNumber	Number
highlight link glslString	String

let b:current_syntax = "glsl"
