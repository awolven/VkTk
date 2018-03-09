del Debug\*.*
rmdir Debug
mkdir Debug
cl /nologo /Zi /MD /I c:\Users\awolven\cimgui\imgui /I c:\Users\awolven\glfw-3.2.1.bin.WIN64\glfw-3.2.1.bin.WIN64\include /I %VULKAN_SDK%\include c:\Users\awolven\cimgui\imgui\examples\vulkan_example\imgui_impl_glfw_vulkan.cpp /LDd /DLL /DEF c:\Users\awolven\vulkan-demos\imgui-glfw-vulkan-1.def /FeDebug/imgui-glfw-vulkan-1.dll /link /NODEFAULTLIB:MSVCRTD /libpath:c:\Users\awolven\cimgui\cimgui\x64\Debug /libpath:c:\Users\awolven\glfw-3.2.1.bin.WIN64\glfw-3.2.1.bin.WIN64\lib-vc2015 /libpath:%VULKAN_SDK%\lib cimgui.lib glfw3.lib glfw3dll.lib opengl32.lib gdi32.lib shell32.lib vulkan-1.lib
