(defsystem vktk-imgui
  :description "Dear ImGui support implemented in VkTk."
  :depends-on (:vktk)
  :author "Andrew K Wolven <awolven@gmail.com>"
  :components
  ((:file "ifc/cimgui/imgui-package")
   (:file "ifc/cimgui/cimgui")
   (:file "ifc/cimgui/lisp-wrappers")
   (:file "ifc/imgui-src/imgui-class")   
   (:file "ifc/imgui-src/glfw-imgui")
   (:file "ifc/imgui-src/vulkan-imgui")
   (:file "ifc/imgui-src/general-imgui")
   (:file "ifc/cimgui-library")))
