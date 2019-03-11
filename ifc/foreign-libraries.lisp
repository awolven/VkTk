(in-package :cl-user)

#+darwin
(progn
  (sb-posix:setenv "VULKAN_SDK" "/Users/awolven/vulkansdk-macos-1.1.97.0/macOS" 0)
  (sb-posix:setenv "VK_ICD_FILENAMES" "/Users/awolven/vulkansdk-macos-1.1.97.0/macOS/etc/vulkan/icd.d/MoltenVK_icd.json" 0)
  (sb-posix:setenv "VK_LAYER_PATH" "/Users/awolven/vulkansdk-macos-1.1.97.0/macOS/etc/vulkan/explicit_layers.d" 0)
  (sb-posix:setenv "DYLD_LIBRARY_PATH" "/Users/awolven/vulkansdk-macos-1.1.97.0/macOS/lib" 0)
  (sb-posix:setenv "VULKAN_FRAMEWORK_PATH" "/Users/awolven/vulkansdk-macos-1.1.97.0/macOS/Frameworks" 0)
  (sb-posix:setenv "DYLD_FRAMEWORK_PATH" "/Users/awolven/vulkansdk-macos-1.1.97.0/macOS/Frameworks" 0)
)

(cffi:define-foreign-library vulkan-loader
  (:darwin "libvulkan.1.dylib")
  (:windows "C:/VulkanSDK/1.1.97.0/Source/lib/vulkan-1.dll"))

(cffi:define-foreign-library cimgui
  (:darwin "/usr/local/lib/cimgui.dylib")
  (:windows "~/vktk/cimgui/build/Debug/cimgui.dll"))

(cffi:define-foreign-library glfw3
  (:darwin "/usr/local/lib/libglfw.dylib")
  (:windows "~/vktk/glfw/build/src/Debug/glfw3.dll"))

(cffi:use-foreign-library vulkan-loader)
(cffi:use-foreign-library cimgui)
(cffi:use-foreign-library glfw3)
