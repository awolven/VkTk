cl /LD ^
   /I c:\Users\awolven\glfw-3.2.1.bin.WIN64\glfw-3.2.1.bin.WIN64\include ^
   /I c:\Users\awolven\cimgui\imgui ^
   /I c:\Users\awolven\cimgui\cimgui ^
   /I %VULKAN_SDK%\include ^
   c:\Users\awolven\cimgui\imgui\examples\vulkan_example\cimgui_impl_glfw_vulkan.cpp ^
   glfw3dll.lib ^
   cimgui.lib ^
   vulkan-1.lib ^
   /link ^
   /libpath:c:\Users\awolven\glfw-3.2.1.bin.WIN64\glfw-3.2.1.bin.WIN64\lib-vc2015 ^
   /libpath:c:\Users\awolven\cimgui\cimgui\x64\Debug ^
   /libpath:%VULKAN_SDK%\Source\lib ^
   /NODEFAULTLIB:MSVCRTD ^
   /OUT:cimgui-impl-glfw-vulkan.dll
