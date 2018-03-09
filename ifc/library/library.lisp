(in-package :cl-user)

(cffi:define-foreign-library libffi
  (:windows "C:/Users/awolven/vulkan-demos/Debug/libffi.dll"))
  
(cffi:define-foreign-library imgui-glfw-vulkan-1
  (:windows "C:/Users/awolven/vulkan-demos/Debug/imgui-glfw-vulkan-1.dll"))

(cffi:use-foreign-library libffi)
(cffi:use-foreign-library imgui-glfw-vulkan-1)

