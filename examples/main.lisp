(in-package :vktk)

(defclass imgui-enabled-application (vulkan-application-mixin)
  ((imgui :accessor imgui-module)))

(defmethod initialize-instance :after ((app imgui-enabled-application) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((device (default-logical-device app))
	 (pipeline-cache (pipeline-cache app))
	 (descriptor-pool (first (vk::descriptor-pools device))))
    (setf (imgui-module app)
	  (make-instance 'imgui
			 :application app
			 :pipeline-cache pipeline-cache
			 :descriptor-pool descriptor-pool))
    (values)))

(defmethod on-key-event ((app imgui-enabled-application) key action)
  (declare (ignore key action))
  (values))

(defmethod main ((app imgui-enabled-application) &rest args)
  (declare (ignore args))

  (sb-int:with-float-traps-masked
      (:invalid
       :inexact
       :overflow
       :underflow
       :divide-by-zero)

    (let* ((device (default-logical-device app))
	   (main-window (first (last (vktk::window-registry app))))
	   (index (queue-family-index (render-surface main-window)))
	   (queue (find-queue device index))
	   (command-pool (find-command-pool device index))
	   (command-buffer (elt (command-buffers command-pool) 0))
	   (show-demo-window t))
      
      (device-wait-idle device)
      
      (reset-command-pool device command-pool)
      
      (begin-command-buffer command-buffer)
      
      ;; another queue and another command buffer could be used for this
      (imgui-vulkan-create-fonts-texture (imgui-module app) command-buffer)
      
      (end-command-buffer command-buffer)
      
      (queue-submit1 queue command-buffer)
      
      (device-wait-idle device)
      
      (imgui-vulkan-destroy-font-upload-objects (imgui-module app))
      
      (let* ((current-frame 0)
	     (image-index)
	     (exit? nil))

	(loop while (zerop (glfwWindowShouldClose (h main-window))) until exit?
	   do
	     (glfwPollEvents)

	     (when (recreate-swapchain? main-window)
	       (device-wait-idle device) ;;hack
	       (multiple-value-bind (width height) (get-framebuffer-size main-window)
		 (vulkan-create-window main-window width height)))

	     (let* ((swapchain (swapchain main-window))
		    (command-buffer (frame-command-buffer (elt (frame-resources swapchain) current-frame))))

	   
	       (imgui-glfw-new-frame (imgui-module app))

	       (ig::igNewFrame)

	       (setf image-index
		     (frame-begin swapchain (render-pass swapchain) current-frame
				  (clear-value main-window) command-pool))



	       (when show-demo-window
		 (setq show-demo-window (ig:show-demo-window show-demo-window)))

	       (ig:igBeginMainMenuBar)

	       (when (ig:begin-menu "File")
		 (ig:menu-item "New")
		 (ig:menu-item "Open")
		 (when (ig:menu-item "Exit")
		   (setq exit? t))
		 (ig:end-menu))

	       (when (ig:begin-menu "Edit")
		 (ig:menu-item "Create")
		 (ig:menu-item "Move")
		 (ig:menu-item "Delete")
		 (ig:end-menu))
	       
	       (when (ig:begin-menu "View")
		 (when (ig:menu-item "Reset Camera"))
		 (ig:end-menu))
	       
	       (ig:igEndMainMenuBar)
	       
	       (ig:igRender)	
	       
	       (imgui-vulkan-render-draw-data
		(imgui-module app) (ig::igGetMainViewport) command-buffer current-frame)

	       (frame-end swapchain queue current-frame)

	       (when (not (zerop (logand (foreign-slot-value (ig::igGetIO) '(:struct ig::ImguiIO)
							     'ig::ConfigFlags)
					 ig::ImGuiConfigFlags_ViewportsEnable)))
		 (ig::igUpdatePlatformWindows)
		 (ig::igRenderPlatformWindowsDefault))
	       
	       (frame-present swapchain queue current-frame image-index main-window)
	       (setf current-frame (mod (1+ current-frame) (number-of-images swapchain)))))
	
	
	  (shutdown-application app)))))
