(in-package :cl-user)

(defpackage :vulkan-toolkit
  (:nicknames :vktk)
  (:use :cl :cffi :imgui :glfw :vulkan :3d-vectors :3d-matrices :lisp-unit)
  (:import-from :sb-ext #:quit #:exit))
  
