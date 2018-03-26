(in-package :cl-user)

(cffi:define-foreign-library libffi
  (:windows "~/vkapp/ifc/libffi/libffi.dll"))
  
(cffi:define-foreign-library vulkan-1
  (:windows "c:/VulkanSDK/1.1.70.1/Source/lib/vulkan-1.dll"))

(defun load-vulkan-layer-libs ()
  (let ((libraries (list "VkLayer_api_dump.dll"
			 "VkLayer_screenshot.dll"
			 "VkLayer_core_validation.dll"
			 "VkLayer_threading.dll"
			 "VkLayer_device_simulation.dll"
			 "VkLayer_unique_objects.dll"
			 "VkLayer_monitor.dll"
			 "VkLayer_vktrace_layer.dll"
			 "VkLayer_object_tracker.dll"      
			 "VkLayer_parameter_validation.dll")))
    (loop for lib in libraries
       do (cffi:load-foreign-library (concatenate 'string "c:/VulkanSDK/1.0.65.1/Source/lib/" lib)))))

(cffi:define-foreign-library cimgui
  (:windows "~/cimgui/cimgui/x64/debug/cimgui.dll"))

(cffi:define-foreign-library glfw3
  (:windows "~/glfw-3.2.1.bin.WIN64/glfw-3.2.1.bin.WIN64/lib-vc2015/glfw3.dll"))

(cffi:define-foreign-library cimgui-impl-glfw-vulkan
  (:windows "~/vkapp/ifc/cimgui-impl-glfw-vulkan.dll"))

(cffi:use-foreign-library libffi)
(cffi:use-foreign-library vulkan-1)
(load-vulkan-layer-libs)
(cffi:use-foreign-library cimgui)
(cffi:use-foreign-library glfw3)
(cffi:use-foreign-library cimgui-impl-glfw-vulkan)




			
