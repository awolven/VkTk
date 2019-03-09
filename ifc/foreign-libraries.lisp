(in-package :cl-user)

(cffi:define-foreign-library vulkan-loader
  (:windows "C:/VulkanSDK/1.1.97.0/Source/lib/vulkan-1.dll"))

(cffi:define-foreign-library cimgui
  (:windows "~/vktk/cimgui/build/Debug/cimgui.dll"))

(cffi:define-foreign-library glfw3
  (:windows "~/vktk/glfw/build/src/Debug/glfw3.dll"))

(cffi:use-foreign-library vulkan-loader)
(cffi:use-foreign-library cimgui)
(cffi:use-foreign-library glfw3)
