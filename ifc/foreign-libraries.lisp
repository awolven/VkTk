(in-package :cl-user)

(in-package :cl-user)

(defparameter *home-dir* #+darwin "/Users/awolven" #+windows "C:/Users/awolven")

(defparameter *vulkan-sdk-path*
  #+darwin (concatenate 'string *home-dir* "/vulkansdk-macos-1.1.106.0/macOS")
  #+windows "C:/VulkanSDK/1.1.97.0")

#+darwin
(progn
  (sb-posix:setenv "VULKAN_SDK" *vulkan-sdk-path* 0)
  (sb-posix:setenv "VK_ICD_FILENAMES" (concatenate 'string *vulkan-sdk-path* "/etc/vulkan/icd.d/MoltenVK_icd.json") 0)
  (sb-posix:setenv "VK_LAYER_PATH" (concatenate 'string *vulkan-sdk-path* "/etc/vulkan/explicit_layer.d") 0)
  (sb-posix:setenv "DYLD_LIBRARY_PATH" (concatenate 'string *vulkan-sdk-path* "/lib") 0)
  (sb-posix:setenv "VULKAN_FRAMEWORK_PATH" (concatenate 'string *vulkan-sdk-path* "/Frameworks") 0)
  (sb-posix:setenv "DYLD_FRAMEWORK_PATH" (concatenate 'string *vulkan-sdk-path* "/Frameworks") 0)
)

(cffi:define-foreign-library vulkan-loader
  (:darwin "libvulkan.1.dylib")
  (:windows (concatenate 'string *vulkan-sdk-path* "/Source/lib/vulkan-1.dll")))

(cffi:define-foreign-library cimgui
  (:darwin "/usr/local/lib/cimgui.dylib")
  (:windows (concatenate 'string *home-dir* "/vktk/cimgui/build/Debug/cimgui.dll")))

(cffi:define-foreign-library glfw3
  (:darwin "/usr/local/lib/libglfw.dylib")
  (:windows (concatenate 'string *home-dir* "/vktk/glfw/build/src/Debug/glfw3.dll")))

(cffi:use-foreign-library vulkan-loader)
(cffi:use-foreign-library cimgui)
(cffi:use-foreign-library glfw3)
