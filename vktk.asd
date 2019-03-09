(defsystem vktk
  :description "A Vulkan application toolkit using GLFW and ImGui."
  :depends-on (:cffi :3d-matrices :lisp-unit)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "ifc/foreign-libraries")
   (:file "ifc/cimgui/imgui-package")
   (:file "ifc/glfw/glfw-package")
   (:file "ifc/vulkan/vk-package")
   (:file "src/vktk-package")
   (:file "ifc/cimgui/cimgui")
   (:file "ifc/glfw/glfw")
   (:file "ifc/vulkan/vk-types")
   (:file "ifc/vulkan/s-type-table")
   (:file "ifc/vulkan/vk-macros")
   (:file "ifc/vulkan/vk-funcs")
   (:file "src/macros")
   (:file "src/support")
   (:file "src/vulkan-toolkit")
   (:file "ifc/imgui-glfw-vulkan")))

