(in-package :cl-user)

(cffi:define-foreign-library libffi
  (:windows "ifc/libffi/libffi.dll"))
  
(cffi:define-foreign-library imgui-glfw-vulkan-1
  (:windows "ifc/library/Debug/imgui-glfw-vulkan-1.dll"))

(cffi:use-foreign-library libffi)
(cffi:use-foreign-library imgui-glfw-vulkan-1)

