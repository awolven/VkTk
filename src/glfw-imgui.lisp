(in-package :vktk)

#+windows
(load-foreign-library "User32.dll")

#+windows
(defcfun ("glfwGetWin32Window" glfwGetWin32Window) :pointer
  (window :pointer))

#+windows
(defcfun ("SetParent" win32SetParent) :pointer
  (hchild :pointer)
  (hparent :pointer))

#+windows
(defcfun ("SetWindowLongPtrA" win32SetWindowLongPtrA) :uint64
  (hWnd :pointer)
  (nIndex :int)
  (dwNewLong :uint64))

#+windows
(defcfun ("GetWindowLongPtrA" win32GetWindowLongPtrA) :uint64
  (hWnd :pointer)
  (nIndex :int))

(defun set-parent (child parent)
  (let ((hwnd-child (glfwGetWin32Window (h child)))
	(hwnd-parent (glfwGetWin32Window (h parent))))
    (win32SetParent hwnd-child hwnd-parent)))

#+windows
(cl:defconstant WS_POPUP #x80000000)
#+windows
(cl:defconstant GWL_EXSTYLE -20)
#+windows
(cl:defconstant GWL_STYLE -16)
#+windows
(cl:defconstant WS_EX_TOOLWINDOW #x00000080)

(defun set-popup (window style)
  #+windows
  (let ((hwnd (glfwGetWin32Window (h window))))
    (win32SetWindowLongPtrA hwnd GWL_STYLE (logior style WS_POPUP))))

(defun get-style (window)
  #+windows
  (let ((hwnd (glfwGetWin32Window (h window))))
    (win32GetWindowLongPtrA hwnd GWL_STYLE)))

(defun set-toolwindow (window)
  #+windows
  (let ((hwnd (glfwGetWin32Window (h window))))
    (win32SetWindowLongPtrA hwnd GWL_EXSTYLE WS_EX_TOOLWINDOW)))

(defcallback imgui-glfw-get-clipboard-text-callback :string ((window :pointer))
  (glfwGetClipboardString window))

(defcallback imgui-glfw-set-clipboard-text-callback :void ((window :pointer) (text :string))
  (glfwSetClipboardString window text))

(defgeneric imgui-module (application))

(defun imgui-only-capture-mouse? ()
  (foreign-slot-value (igGetIO) '(:struct ig::ImGuiIO) 'ig::WantCaptureMouse))

(defmethod imgui-glfw-mouse-button ((window window) button action mods)
  (when (and (prev-user-callback-mouse-button (imgui-module (vk::application window)))
	     (pointer-eq (h (main-window (vk::application window)))
			 (h window)))
    (funcall (prev-user-callback-mouse-button (imgui-module (vk::application window))) window button action mods))
  (let ((mouse-just-pressed (mouse-just-pressed (imgui-module (vk::application window)))))
    (when (and (eq action GLFW_PRESS)
	       (< button (length mouse-just-pressed)))
      (setf (aref mouse-just-pressed button) t)
      (unless (imgui-only-capture-mouse?)
	(on-mouse-click (vk::application window) mouse-just-pressed action mods))))
  (values))

(defmethod on-mouse-click ((application vulkan-application-mixin) buttons-pressed action mods)
  (declare (ignore buttons-pressed action mods)))

(defcallback imgui-glfw-mouse-button-callback :void ((user-data :pointer) (button :int) (action :int) (mods :int))
  (let* ((window (find-window user-data)))
    (imgui-glfw-mouse-button window button action mods))
  (values))

(defcallback imgui-glfw-scroll-callback :void ((user-data :pointer) (xoffset :double) (yoffset :double))
  (imgui-glfw-scroll-event (find-window user-data) xoffset yoffset)
  (values))

(defcallback imgui-glfw-cursor-position-callback :void ((window :pointer) (xpos :double) (ypos :double))
  (let* ((window (find-window window)))
    (imgui-glfw-cursor-position (slot-value window 'vk::application) window xpos ypos)
    (values)))

(defmethod imgui-glfw-cursor-position ((application vulkan-application-mixin) window xpos ypos)
  (declare (ignorable window))
  (values))

(defun setup-cursor-pos-callback (app)
  (glfwSetCursorPosCallback (h (main-window app)) (callback imgui-glfw-cursor-position-callback)))

(defmethod on-scroll (application xoffset yoffset)
  (declare (ignore xoffset yoffset))
  (values))

(defun imgui-glfw-scroll-event (window xoffset yoffset)
  
  (when (and (prev-user-callback-mouse-button (imgui-module (vk::application window)))
	     (pointer-eq (h (main-window (vk::application window))) (h window)))
    (funcall (prev-user-callback-scroll
	      (imgui-module (vk::application window)))
	     window xoffset yoffset))
  
  (let ((io (ig::igGetIO)))
    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::MouseWheelH)
	  (+ (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::MouseWheelH)
	     (coerce xoffset 'single-float))

	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::MouseWheel)
	  (+ (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::MouseWheel)
	     (coerce yoffset 'single-float)))

    (unless (imgui-only-capture-mouse?)
      (on-scroll (vk::application window) xoffset yoffset)))
				  
  (values))

(defgeneric on-key-event (app key action))  

(defun shift-key-down? ()
  (let* ((io (ig::igGetIO))
	 (p-keys-down (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeysDown)))
    (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_SHIFT)
	(mem-aref p-keys-down :bool GLFW_KEY_RIGHT_SHIFT))))

(defun control-key-down? ()
  (let* ((io (ig::igGetIO))
	 (p-keys-down (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeysDown)))
    (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_CONTROL)
	(mem-aref p-keys-down :bool GLFW_KEY_RIGHT_CONTROL)
	(mem-aref p-keys-down :bool GLFW_KEY_CAPS_LOCK))))

(defun meta-key-down? ()
  (let* ((io (ig::igGetIO))
	 (p-keys-down (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeysDown)))
    (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_ALT)
	(mem-aref p-keys-down :bool GLFW_KEY_RIGHT_ALT))))

(defun super-key-down? ()
  (let* ((io (ig::igGetIO))
	 (p-keys-down (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeysDown)))
    (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_SUPER)
	(mem-aref p-keys-down :bool GLFW_KEY_RIGHT_SUPER))))

(defun imgui-glfw-key-callback-function (key action)
  (let* ((io (ig::igGetIO))
	 (p-keys-down (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::KeysDown)))
    (when (eq action GLFW_PRESS)
      (setf (mem-aref p-keys-down :bool key) t))
    (when (eq action GLFW_RELEASE)
      (setf (mem-aref p-keys-down :bool key) nil))

    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyCtrl)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_CONTROL)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_CONTROL))
		   
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyShift)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_SHIFT)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_SHIFT))

	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyAlt)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_ALT)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_ALT))

	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeySuper)
	  (or (mem-aref p-keys-down :bool GLFW_KEY_LEFT_SUPER)
	      (mem-aref p-keys-down :bool GLFW_KEY_RIGHT_SUPER)))

    (on-key-event *app* key action)
	       
    (values)))

(defcallback imgui-glfw-key-callback :void ((user-data :pointer) (key :int) (arg2 :int) (action :int) (mods :int))
  (declare (ignorable user-data arg2 mods))
  (imgui-glfw-key-callback-function key action))

(defcallback imgui-glfw-char-callback :void ((user-data :pointer) (c :uint32))
  (declare (ignorable user-data))
  (let ((io (ig::igGetIO)))
    (when (and (> c 0) (< c #x10000))
      (ig::ImGuiIO_AddInputCharacter io c)))
  (values))

(defcallback imgui-glfw-monitor-callback :void ((monitor :pointer) (ignore :int))
  (declare (ignore monitor ignore))
  (setf (want-update-monitors? (imgui-module *app*)) t)
  (values))

(defun imgui-glfw-init (imgui window &key (install-callbacks? t))
  (setf (main-window imgui) window
	(imgui-time imgui) 0.0f0)
  
  (let ((io (ig::igGetIO)))
    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::BackendFlags)
	  (logior (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::BackendFlags)
		  ig::ImGuiBackendFlags_HasMouseCursors
		  ig::ImGuiBackendFlags_HasSetMousePos
		  ig::ImGuiBackendFlags_PlatformHasViewports
		  #+nil-windows ig::ImGuiBackendFlags_HasMouseHoveredViewport))
    
    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::BackendPlatformName)
	  (foreign-string-alloc "imgui-impl-glfw"))

    (let ((p-key-map (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::KeyMap)))
      
      (setf (mem-aref p-key-map :int ig::ImGuiKey_Tab) GLFW_KEY_TAB
	      (mem-aref p-key-map :int ig::ImGuiKey_LeftArrow) GLFW_KEY_LEFT
	      (mem-aref p-key-map :int ig::ImGuiKey_RightArrow) GLFW_KEY_RIGHT
	      (mem-aref p-key-map :int ig::ImGuiKey_UpArrow) GLFW_KEY_UP
	      (mem-aref p-key-map :int ig::ImGuiKey_DownArrow) GLFW_KEY_DOWN
	      (mem-aref p-key-map :int ig::ImGuiKey_PageUp) GLFW_KEY_PAGE_UP
	      (mem-aref p-key-map :int ig::ImGuiKey_PageDown) GLFW_KEY_PAGE_DOWN
	      (mem-aref p-key-map :int ig::ImGuiKey_Home) GLFW_KEY_HOME
	      (mem-aref p-key-map :int ig::ImGuiKey_End) GLFW_KEY_END
	      (mem-aref p-key-map :int ig::ImGuiKey_Insert) GLFW_KEY_INSERT
	      (mem-aref p-key-map :int ig::ImGuiKey_Delete) GLFW_KEY_DELETE
	      (mem-aref p-key-map :int ig::ImGuiKey_Backspace) GLFW_KEY_BACKSPACE
	      (mem-aref p-key-map :int ig::ImGuiKey_Space) GLFW_KEY_SPACE
	      (mem-aref p-key-map :int ig::ImGuiKey_Enter) GLFW_KEY_ENTER
	      (mem-aref p-key-map :int ig::ImGuiKey_Escape) GLFW_KEY_ESCAPE
	      (mem-aref p-key-map :int ig::ImGuiKey_A) GLFW_KEY_A
	      (mem-aref p-key-map :int ig::ImGuiKey_C) GLFW_KEY_C
	      (mem-aref p-key-map :int ig::ImGuiKey_V) GLFW_KEY_V
	      (mem-aref p-key-map :int ig::ImGuiKey_X) GLFW_KEY_X
	      (mem-aref p-key-map :int ig::ImGuiKey_Y) GLFW_KEY_Y
	      (mem-aref p-key-map :int ig::ImGuiKey_Z) GLFW_KEY_Z))

    (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::SetClipboardTextFn)
	  (callback imgui-glfw-set-clipboard-text-callback)
	  
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::GetClipboardTextFn)
	  (callback imgui-glfw-get-clipboard-text-callback)
	  
	  (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ClipBoardUserData)
	  (h window))
    
    (let ((prev-error-callback (glfwSetErrorCallback +nullptr+)))

      (let ((mouse-cursors (mouse-cursors imgui)))
	(setf (aref mouse-cursors ig::ImGuiMouseCursor_Arrow)
	      (glfwCreateStandardCursor GLFW_ARROW_CURSOR)

	      (aref mouse-cursors ig::ImGuiMouseCursor_TextInput)
	      (glfwCreateStandardCursor GLFW_IBEAM_CURSOR)

	      (aref mouse-cursors ig::ImGuiMouseCursor_ResizeAll)
	      (glfwCreateStandardCursor GLFW_ARROW_CURSOR)

	      (aref mouse-cursors ig::ImGuiMouseCursor_ResizeNS)
	      (glfwCreateStandardCursor GLFW_VRESIZE_CURSOR)
	    
	      (aref mouse-cursors ig::ImGuiMouseCursor_ResizeEW)
	      (glfwCreateStandardCursor GLFW_HRESIZE_CURSOR)
	    
	      (aref mouse-cursors ig::ImGuiMouseCursor_ResizeNESW)
	      (glfwCreateStandardCursor GLFW_ARROW_CURSOR)

	      (aref mouse-cursors ig::ImGuiMouseCursor_ResizeNWSE)
	      (glfwCreateStandardCursor GLFW_ARROW_CURSOR)

	      (aref mouse-cursors ig::ImGuiMouseCursor_Hand)
	      (glfwCreateStandardCursor GLFW_HAND_CURSOR)))

      (glfwSetErrorCallback prev-error-callback))

    (when install-callbacks?
      (setf (prev-user-callback-mouse-button imgui)
	    (let ((callback (glfwSetMouseButtonCallback (h window) (callback imgui-glfw-mouse-button-callback))))
	      (if (null-pointer-p callback)
		  nil
		  callback)))
      
      (setup-cursor-pos-callback (slot-value window 'vk::application))
      (setf	    
	    (prev-user-callback-scroll imgui)
	    (let ((callback (glfwSetScrollCallback (h window) (callback imgui-glfw-scroll-callback))))
	      (if (null-pointer-p callback) nil callback))
	    
	    (prev-user-callback-key imgui)
	    (let ((callback (glfwSetKeyCallback (h window) (callback imgui-glfw-key-callback))))
	      (if (null-pointer-p callback) nil callback))
	    
	    (prev-user-callback-char imgui)
	    (let ((callback (glfwSetCharCallback (h window) (callback imgui-glfw-char-callback))))
	      (if (null-pointer-p callback) nil callback))

	    (prev-user-monitor-callback imgui)
	    (let ((callback (glfwSetMonitorCallback (callback imgui-glfw-monitor-callback))))
	      (if (null-pointer-p callback) nil callback))))

    (imgui-glfw-update-monitors imgui)
    (glfwSetMonitorCallback (callback imgui-glfw-monitor-callback))
    
    (let ((main-viewport (ig::igGetMainViewport)))
      (setf (foreign-slot-value main-viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandle)
	    (h window))
      
      #+windows
      (setf (foreign-slot-value main-viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandleRaw)
	    (glfwGetWin32Window (h window))))

    (when (not (zerop (logand (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ConfigFlags) 
			      ig::ImGuiConfigFlags_ViewportsEnable)))
      
      (imgui-glfw-init-platform-interface imgui))

    t))

(defun number-of-viewports ()
  (let* ((platform-io (ig::igGetPlatformIO))
	 (p-viewports (foreign-slot-pointer platform-io '(:struct ig::ImGuiPlatformIO) 'ig::Viewports)))
    (foreign-slot-value p-viewports '(:struct ig::ImVector_ImGuiViewport) 'ig::Size)))

(defun imgui-glfw-update-mouse-pos-and-buttons (imgui)
  (let* ((io (ig:igGetIO))
	 (window (main-window imgui))
	 (p-mousedown (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MouseDown)))

    (loop for i from 0 below 5
       do (setf (mem-aref p-mousedown :bool i)
		(or (elt (mouse-just-pressed imgui) i)
		    (not (zerop (glfwGetMouseButton (h window) i)))))
	 (setf (elt (mouse-just-pressed imgui) i) nil))
    
    (let* ((p-mouse-pos (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MousePos))
	   (mouse-pos-x-backup (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::x))
	   (mouse-pos-y-backup (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::y))
	   (-FLT_MAX (- (ig::igGet_FLT_MAX))))

      (setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::x) -FLT_MAX
	    (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::y) -FLT_MAX)

      (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::MouseHoveredViewport) 0)

      (let* ((platform-io (ig::igGetPlatformIO))
	     (p-viewports (foreign-slot-pointer platform-io '(:struct ig::ImGuiPlatformIO) 'ig::Viewports))
	     (p-viewports-data (foreign-slot-value p-viewports '(:struct ig::ImVector_ImGuiViewport) 'ig::Data))
	     (size (foreign-slot-value p-viewports '(:struct ig::ImVector_ImGuiViewport) 'ig::Size)))

	(loop for n from 0 below size
	   do (let* ((p-viewport (mem-aref p-viewports-data :pointer n))
		     (p-window (foreign-slot-value p-viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandle))
		     (focused (not (zerop (glfwGetWindowAttrib p-window GLFW_FOCUSED)))))

		(when focused
		  (if (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::WantSetMousePos)

		      (let ((p-pos (foreign-slot-pointer p-viewport '(:struct ig::ImGuiViewport) 'ig::Pos)))
			(glfwSetCursorPos p-window
					  (coerce
					   (- mouse-pos-x-backup
					      (foreign-slot-value p-pos '(:struct ig::ImVec2) 'ig::x))
					   'double-float)
					  (coerce 
					   (- mouse-pos-y-backup
					      (foreign-slot-value p-pos '(:struct ig::ImVec2) 'ig::y))
					   'double-float)))

		      (with-foreign-objects ((&mouse-x :double)
					     (&mouse-y :double))
			(glfwGetCursorPos p-window &mouse-x &mouse-y)
			(if (not (zerop (logand
					 (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::ConfigFlags)
					 ig::ImGuiConfigFlags_ViewportsEnable)))

			    (with-foreign-objects ((&window-x :int)
						   (&window-y :int))
			      (glfwGetWindowPos p-window &window-x &window-y)
			      (let ((p-mouse-pos (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MousePos)))
				(setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::x)
				      (+ (coerce (mem-aref &mouse-x :double) 'single-float)
					 (mem-aref &window-x :int)))

				(setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::y)
				      (+ (coerce (mem-aref &mouse-y :double) 'single-float)
					 (mem-aref &window-y :int)))))
				  
			    (let ((p-mouse-pos (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::MousePos)))
			      (setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::x)
				    (coerce (mem-aref &mouse-x :double) 'single-float))

			      (setf (foreign-slot-value p-mouse-pos '(:struct ig::ImVec2) 'ig::y)
				    (coerce (mem-aref &mouse-y :double) 'single-float))))))

		  (loop for i from 0 below 5
		     do (setf (mem-aref p-mousedown :bool i)
			      (or (mem-aref p-mousedown :bool i)
				  (not (zerop (glfwGetMouseButton p-window i)))))))))))))

(defun imgui-glfw-update-monitors (imgui)
  (let ((platform-io (ig::igGetPlatformIO)))
    (with-foreign-object (&monitors-count :int)
      (let ((p-glfw-monitors (glfwGetMonitors &monitors-count))
	    (p-imgui-monitors (foreign-slot-pointer
			       platform-io '(:struct ig::ImGuiPlatformIO) 'ig::Monitors)))
	(ig::ImVector_ImGuiPlatformMonitor_resize p-imgui-monitors 0)
	(loop for n from 0 below (mem-aref &monitors-count :int)
	   do #+NIL(ig::ImVector_ImGuiPlatformMonitor_resize p-imgui-monitors (1+ n))
	     (with-foreign-object (monitor '(:struct ig::ImGuiPlatformMonitor))
	       (let ((glfw-monitors-n (mem-aref p-glfw-monitors :pointer n)))
		 #+NIL(mem-aptr (foreign-slot-value p-imgui-monitors '(:struct ig::ImVector) 'ig::Data)
				'(:struct ig::ImGuiPlatformMonitor) n)
		 (with-foreign-objects ((&x :int)
					(&y :int))
		   (glfwGetMonitorPos glfw-monitors-n &x &y)
		   (let ((vid-mode (glfwGetVideoMode glfw-monitors-n))
			 (p-main-pos (foreign-slot-pointer monitor '(:struct ig::ImGuiPlatformMonitor) 'ig::MainPos))
			 (p-main-size (foreign-slot-pointer monitor '(:struct ig::ImGuiPlatformMonitor) 'ig::MainSize))
			 (p-work-pos (foreign-slot-pointer monitor '(:struct ig::ImGuiPlatformMonitor) 'ig::WorkPos))
			 (p-work-size (foreign-slot-pointer monitor '(:struct ig::ImGuiPlatformMonitor) 'ig::WorkSize)))
		     (setf (foreign-slot-value p-main-pos '(:struct ig::ImVec2) 'ig::x)
			   (coerce (mem-aref &x :int) 'single-float)
			  
			   (foreign-slot-value p-main-pos '(:struct ig::ImVec2) 'ig::y)
			   (coerce (mem-aref &y :int) 'single-float)
			  
			   (foreign-slot-value p-main-size '(:struct ig::ImVec2) 'ig::x)
			   (coerce
			    (foreign-slot-value vid-mode '(:struct GLFWvidmode) '%glfw::width)
			    'single-float)
			  
			   (foreign-slot-value p-main-size '(:struct ig::ImVec2) 'ig::y)
			   (coerce
			    (foreign-slot-value vid-mode '(:struct GLFWvidmode) '%glfw::height)
			    'single-float))

		     (with-foreign-objects ((&w :int)
					    (&h :int))
		       (if (boundp '%glfw::glfwGetMonitorWorkarea)
			   (progn
			     (funcall '%glfw::glfwGetMonitorWorkarea
				      glfw-monitors-n &x &y &w &h)
			    
			     (setf (foreign-slot-value p-work-pos '(:struct ig::ImVec2) 'ig::x)
				   (coerce (mem-aref &x :int) 'single-float)
				  
				   (foreign-slot-value p-work-pos '(:struct ig::ImVec2) 'ig::y)
				   (coerce (mem-aref &y :int) 'single-float)
				  
				   (foreign-slot-value p-work-size '(:struct ig::ImVec2) 'ig::x)
				   (coerce (mem-aref &w :int) 'single-float)
				  
				   (foreign-slot-value p-work-size '(:struct ig::ImVec2) 'ig::y)
				   (coerce (mem-aref &h :int) 'single-float)))
			  
			   (setf (foreign-slot-value p-work-pos '(:struct ig::ImVec2) 'ig::x)
				 (foreign-slot-value p-main-pos '(:struct ig::ImVec2) 'ig::x)
				  
				 (foreign-slot-value p-work-pos '(:struct ig::ImVec2) 'ig::y)
				 (foreign-slot-value p-main-pos '(:struct ig::ImVec2) 'ig::y)
				  
				 (foreign-slot-value p-work-size '(:struct ig::ImVec2) 'ig::x)
				 (foreign-slot-value p-main-size '(:struct ig::ImVec2) 'ig::x)
				  
				 (foreign-slot-value p-work-size '(:struct ig::ImVec2) 'ig::y)
				 (foreign-slot-value p-main-size '(:struct ig::ImVec2) 'ig::y)))))))
	       (ig::ImVector_ImGuiPlatformMonitor_push_back p-imgui-monitors monitor)))))
	       
    (setf (want-update-monitors? imgui) nil)
    (values)))

(defun imgui-glfw-new-frame (imgui)
  (let* ((io (ig:igGetIO))
	 (window (main-window imgui)))
    
    (multiple-value-bind (width height) (get-window-size window)

      (let ((p-display-size (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::DisplaySize)))
	(setf (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::x) (coerce width 'single-float))
	(setf (foreign-slot-value p-display-size '(:struct ig::ImVec2) 'ig::y) (coerce height 'single-float)))
      
      (multiple-value-bind (display-w display-h) (get-framebuffer-size window)
	(let ((p-framebuffer-scale (foreign-slot-pointer io '(:struct ig::ImGuiIO) 'ig::DisplayFramebufferScale)))

	  (setf (foreign-slot-value p-framebuffer-scale '(:struct ig::ImVec2) 'ig::x)
		(if (> width 0)
		    (coerce (/ display-w width) 'single-float)
		    0.0f0)
		(foreign-slot-value p-framebuffer-scale '(:struct ig::ImVec2) 'ig::y)
		(if (> height 0)
		    (coerce (/ display-h height) 'single-float)
		    0.0f0)))))
    
    (when (want-update-monitors? imgui)
      (imgui-glfw-update-monitors imgui))
    
    (let ((current-time (glfwGetTime)))
      (setf (foreign-slot-value io '(:struct ig::ImGuiIO) 'ig::DeltaTime)
	    (coerce (if (> (imgui-time imgui) 0.0f0)
			(if (zerop (- current-time (imgui-time imgui)))
			    (/ 1.0f0 60.0f0)
			    (- current-time (imgui-time imgui)))
			(/ 1.0f0 60.0f0))
		    'single-float))

      (setf (imgui-time imgui) current-time))

    (imgui-glfw-update-mouse-pos-and-buttons imgui)
    #+NOTYET(imgui-glfw-update-mouse-cursor imgui)
    ;;(print 'ismousedown0)(print (ig::igIsMouseDown 0))

    (values)))

(cffi:defcstruct ImGuiViewportDataGlfw
  (Window :pointer)
  (WindowOwned :bool)
  (IgnoreWindowPosEventFrame :int)
  (IgnoreWindowSizeEventFrame :int))

(defun platform-user-data (p-viewport)
  (foreign-slot-value p-viewport '(:struct ig::ImGuiViewport) 'ig::PlatformUserData))

(defun (setf platform-user-data) (p-data p-viewport)
  (setf (foreign-slot-value p-viewport '(:struct ig::ImGuiViewport) 'ig::PlatformUserData)
	p-data))

(defun renderer-user-data (p-viewport)
  (foreign-slot-value p-viewport '(:struct ig::ImGuiViewport) 'ig::RendererUserData))

(defun (setf renderer-user-data) (p-data p-viewport)
  (setf (foreign-slot-value p-viewport '(:struct ig::ImGuiViewport) 'ig::RendererUserData)
	p-data))

(defun platform-user-data-window (platform-user-data)
  (foreign-slot-value platform-user-data '(:struct ImGuiViewportDataGlfw) 'Window))

(defun (setf platform-user-data-window) (window-handle platform-user-data)
  (setf (foreign-slot-value platform-user-data '(:struct ImGuiViewportDataGlfw) 'Window)
	window-handle))

(cffi:defcallback imgui-glfw-window-close-callback :void ((window :pointer)
							  (ignore1 :int)
							  (ignore2 :int))
  (declare (ignore ignore1 ignore2))
  (on-window-close (find-window window)))

(defmethod on-window-close ((window window))
  (let ((viewport (ig::igFindViewportByPlatformHandle (h window))))
    (setf (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformRequestClose) t)
    (imgui-vulkan-shutdown (imgui *app*))
    (values)))

(cffi:defcallback imgui-glfw-window-pos-callback :void ((window :pointer)
							(ignore1 :int)
							(ignore2 :int))
  (declare (ignore ignore1 ignore2))
  (on-window-pos (find-window window)))

(defmethod on-window-pos ((window window))
  (block on-window-pos
    (let* ((viewport (ig::igFindViewportByPlatformHandle (h window)))
	   (data (platform-user-data viewport)))
      (unless (null-pointer-p data)
	(let ((ignore-event?
	       (<= (ig:igGetFrameCount)
		   (1+ (foreign-slot-value
			data '(:struct ImGuiViewportDataGlfw) 'IgnoreWindowPosEventFrame)))))
	  (when ignore-event?
	    (return-from on-window-pos (values)))))
      (setf (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformRequestMove)
	    t)
      (values))))

(cffi:defcallback imgui-glfw-window-size-callback :void ((window :pointer)
							 (ignore1 :int)
							 (ignore2 :int))
		  ;;(declare (ignore ignore1 ignore2))
  (on-window-resize (find-window window) ignore1 ignore2))

(defmethod on-window-resize ((window window) width height)
  (block on-window-resize
    (let* ((viewport (ig::igFindViewportByPlatformHandle (h window)))
	   (data (platform-user-data viewport)))
      (unless (null-pointer-p data)
	(let ((ignore-event?
	       (<= (ig:igGetFrameCount)
		   (1+ (foreign-slot-value
			data '(:struct ImGuiViewportDataGlfw) 'IgnoreWindowSizeEventFrame)))))
	  (when ignore-event?
	    (return-from on-window-resize (values)))))
      (setf (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformRequestResize)
	    t)
      (setf (recreate-swapchain? window) t
	    (vk::new-width window) width
	    (vk::new-height window) height)
      (values))))

(cffi:defcallback imgui-glfw-create-window-callback :void ((viewport :pointer)
							   (ignore1 :int)
							   (ignore2 :int))
  (declare (ignore ignore1 ignore2))
  (create-window-from-viewport viewport))

(defun alloc-platform-user-data ()
  (let ((data (foreign-alloc '(:struct ImGuiViewportDataGlfw))))
    (setf (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'Window) +nullptr+
	  (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'WindowOwned) nil
	  (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'IgnoreWindowSizeEventFrame) -1
	  (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'IgnoreWindowPosEventFrame) -1)
    data))

(defun create-window-from-viewport (viewport)
  (let ((data (alloc-platform-user-data)))    
	  
    (setf (platform-user-data viewport) data)

    (glfwWindowHint GLFW_VISIBLE 0)
    (glfwWindowHint GLFW_FOCUSED 0)

    (glfwWindowHint %glfw::GLFW_FOCUS_ON_SHOW 0)

    (glfwWindowHint GLFW_DECORATED
		    (if (not (zerop (logand (print (foreign-slot-value
					     viewport '(:struct ig::ImGuiViewport) 'ig::Flags))
					    ig::ImGuiViewportFlags_NoDecoration)))
			0
			1))

    (glfwWindowHint GLFW_FLOATING
		    (if (not (zerop (logand (foreign-slot-value
					     viewport '(:struct ig::ImGuiViewport) 'ig::Flags)
					    ig::ImGuiViewportFlags_TopMost)))
			1
			0))
    (let ((window-handle (glfwCreateWindow (round
			     (foreign-slot-value
			      (foreign-slot-pointer
			       viewport '(:struct ig::ImGuiViewport) 'ig::Size)
			      '(:struct ig::ImVec2) 'ig::x))
			    (round
			     (foreign-slot-value
			      (foreign-slot-pointer
			       viewport '(:struct ig::ImGuiViewport) 'ig::Size)
			      '(:struct ig::ImVec2) 'ig::y))
			    "No title yet" +nullptr+ +nullptr+)))
      ;;(break)
      ;;(setf (platform-user-data-window data)  window-handle)
      (setf (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'WindowOwned) t
	    (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandle)
	    window-handle
	    #+windows
	    (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandleRaw)
	    #+windows
	    (glfwGetWin32Window window-handle))
      
      (let* ((window (make-instance (window-class *app*) :app *app* :handle window-handle)))
      
	(push window (window-registry *app*))

	(glfwSetWindowPos window-handle
			  (round
			   (foreign-slot-value
			    (foreign-slot-pointer
			     viewport '(:struct ig::ImGuiViewport) 'ig::Pos)
			    '(:struct ig::ImVec2) 'ig::x))
			  (round
			   (foreign-slot-value
			    (foreign-slot-pointer
			     viewport '(:struct ig::ImGuiViewport) 'ig::Pos)
			    '(:struct ig::ImVec2) 'ig::y)))
      
	(glfwSetMouseButtonCallback window-handle (callback imgui-glfw-mouse-button-callback))
	(glfwSetScrollCallback window-handle (callback imgui-glfw-scroll-callback))
	(glfwSetKeyCallback window-handle (callback imgui-glfw-key-callback))
	(glfwSetCharCallback window-handle (callback imgui-glfw-char-callback))
	(glfwSetWindowCloseCallback window-handle (callback imgui-glfw-window-close-callback))
	(glfwSetWindowPosCallback window-handle (callback imgui-glfw-window-pos-callback))
	(glfwSetWindowSizeCallback window-handle (callback imgui-glfw-window-size-callback))
	#+NIL(glfwSetWindowFocusCallback window-handle (callback imgui-glfw-window-focus-callback))

	#+windows-nil
	(let ((style (get-style window)))
	  (set-parent window (main-window (imgui-module *app*)))
	  
	  (set-popup window style)
	  #+NIL(set-toolwindow window)
	  )
	
	(values)))))

(cffi:defcallback imgui-glfw-destroy-window-callback :void ((viewport :pointer)
							    (ignore1 :int)
							    (ignore2 :int))
  (declare (ignore ignore1 ignore2))
  (destroy-window-and-viewport viewport))

(defun destroy-window-and-viewport (viewport)
  (let ((window (get-viewport-window viewport)))
    (when window
      (let* ((data (platform-user-data viewport))
	     #+NIL(swapchain (swapchain window)))
	;;(when swapchain
	;;(destroy-swapchain swapchain))
	(when (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'WindowOwned)
	  (glfwDestroyWindow (h window)))
	(setf ;;(foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'Window) +nullptr+

	 (platform-user-data viewport)
	 +nullptr+
	  
	 (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandle)
	 +nullptr+)
	(foreign-free data)
	(setf (window-registry *app*)
	      (remove (h window) (window-registry *app*) :key #'h :test #'pointer-eq))
	(values)))))

(cffi:defcallback imgui-glfw-show-window-callback :void ((viewport :pointer)
							 (ignore1 :int)
							 (ignore2 :int))
  (declare (ignore ignore1 ignore2))
  (show-viewport viewport))

(defun show-viewport (viewport)
  (let ((window (get-viewport-window viewport)))
    (show-window window)
    (values)))

(cffi:defcallback imgui-glfw-get-window-pos-callback-internal :void ((viewport :pointer)
								     (*x :pointer)
								     (*y :pointer))
  (let* ((window (get-viewport-window viewport)))
    (multiple-value-bind (x y) (get-window-pos window)
      (setf (mem-aref *x :int) x
	    (mem-aref *y :int) y)
      (values))))

(cffi:defcallback imgui-glfw-set-window-pos-callback-internal :void ((viewport :pointer)
								     (x :float)
								     (y :float))
  (let ((window (get-viewport-window viewport)))
    (set-window-pos window x y))
  (values))

(cffi:defcallback imgui-glfw-get-window-size-callback-internal :void ((viewport :pointer)
								      (*x :pointer)
								      (*y :pointer))
  (let* ((window (get-viewport-window viewport)))
    (multiple-value-bind (x y) (get-window-size window)
      (setf (mem-aref *x :int) x
	    (mem-aref *y :int) y))
    (values)))

(cffi:defcallback imgui-glfw-set-window-size-callback-internal :void ((viewport :pointer)
								      (w :float)
								      (h :float))
  (let* ((window (get-viewport-window viewport))
	 (data (platform-user-data viewport)))
    (setf (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'IgnoreWindowSizeEventFrame)
	  (ig::igGetFrameCount))
    (set-window-size window (round w) (round h))
    (values)))

(cffi:defcallback imgui-glfw-set-window-title-callback :void ((viewport :pointer)
							      (title :string)
							      (ignore2 :int))
  (declare (ignore ignore2))
  (set-viewport-title viewport title))

(defun set-viewport-title (viewport title)
  (glfwSetWindowTitle (h (get-viewport-window viewport)) title)
  (values))

(defcallback imgui-glfw-window-focus-callback :void ((window-handle :pointer))
  (mapcar #'show-window (remove-if #'(lambda (window)
				       (pointer-eq (h window) window-handle))
				   (window-registry *app*)))
  (glfwShowWindow window-handle)
  (values))

(defcallback imgui-glfw-window-focus-callback2 :void ((window-handle :pointer))
  (mapcar #'show-window (remove-if #'(lambda (window)
				       (pointer-eq (h window) window-handle))
				   (window-registry *app*)))
  ;;(glfwShowWindow window-handle)
  (values))

(cffi:defcallback imgui-glfw-set-window-focus-callback :void ((viewport :pointer))
  (set-viewport-focus viewport))

(defun set-viewport-focus (viewport)
  (let ((window (get-viewport-window viewport)))
    (glfwFocusWindow (h window))
    (values)))

(cffi:defcallback imgui-glfw-get-window-focus-callback :bool ((viewport :pointer))
  (get-viewport-focus viewport))

(defun get-viewport-focus (viewport)
  (let ((window (get-viewport-window viewport)))
    (not (zerop (glfwGetWindowAttrib (h window) GLFW_FOCUSED)))))

(cffi:defcallback imgui-glfw-get-window-minimized-callback :bool ((viewport :pointer))
  (get-viewport-minimized viewport))

(defun get-viewport-minimized (viewport)
  (let ((window (get-viewport-window viewport)))
    (not (zerop (glfwGetWindowAttrib (h window) GLFW_ICONIFIED)))))

(cffi:defcallback imgui-glfw-set-window-alpha-callback :void ((viewport :pointer)
							      (alpha :float)
							      (ignore2 :int))
  (declare (ignore ignore2))
  (set-viewport-alpha viewport alpha))

(defun set-viewport-alpha (viewport alpha)
  (declare (ignore viewport alpha))
  #+NOTYET
  (let ((data (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformUserData)))
    (glfwSetWindowOpacity (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'Window)
			  alpha)))

(cffi:defcallback imgui-glfw-render-window-callback :void ((viewport :pointer)
							   (ignore1 :int)
							   (ignore2 :int))
  (declare (ignore viewport ignore1 ignore2))
  (values))

(cffi:defcallback imgui-glfw-swap-buffers-callback :void ((viewport :pointer)
							  (ignore1 :int)
							  (ignore2 :int))
  (declare (ignore viewport ignore1 ignore2))
  (values))

(cffi:defcallback imgui-glfw-set-ime-input-pos-callback-internal :void ((viewport :pointer)
									(x :int)
									(y :int))
  (viewport-set-ime-input-pos viewport x y))

(defun viewport-set-ime-input-pos (viewport x y)
  (declare (ignore viewport x y))
  (values))

(cffi:defcallback imgui-glfw-create-vk-surface-callback :void
  ((viewport :pointer)
   (instance :pointer)
   (allocator :pointer)
   (out-surface :pointer))
  (viewport-create-vk-surface viewport instance allocator out-surface)
  (values))		  

(defun viewport-create-vk-surface (viewport instance allocator out-surface)
  (let* ((window-handle (get-viewport-window viewport))
	 (window (find-window window-handle)))
    (check-vk-result (vk::glfwCreateWindowSurface instance
						  window-handle
						  allocator
						  out-surface))
    (setf (render-surface window)
	  (make-instance 'surface
			 :handle (mem-aref out-surface 'VkSurfaceKHR)
			 :window window
			 :instance (make-instance 'instance :handle instance)
			 :allocator (make-instance 'allocator :handle allocator)))
    (values)))

(defun imgui-glfw-init-platform-interface (imgui)
  (let ((platform-io (ig::igGetPlatformIO)))
    (setf (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_CreateWindow)
	  (callback imgui-glfw-create-window-callback)
	  
	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_DestroyWindow)
	  (callback imgui-glfw-destroy-window-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_ShowWindow)
	  (callback imgui-glfw-show-window-callback))

    (ig::install_Platform_SetWindowPos_callback platform-io (callback imgui-glfw-set-window-pos-callback-internal))

    (ig::install_Platform_GetWindowPos_callback platform-io (callback imgui-glfw-get-window-pos-callback-internal))

    (ig::install_Platform_SetWindowSize_callback platform-io (callback imgui-glfw-set-window-size-callback-internal))

    (ig::install_Platform_GetWindowSize_callback platform-io (callback imgui-glfw-get-window-size-callback-internal))
	  
    (setf (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_SetWindowFocus)
	  (callback imgui-glfw-set-window-focus-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_GetWindowFocus)
	  (callback imgui-glfw-get-window-focus-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_GetWindowMinimized)
	  (callback imgui-glfw-get-window-minimized-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_SetWindowTitle)
	  (callback imgui-glfw-set-window-title-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_RenderWindow)
	  (callback imgui-glfw-render-window-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_SwapBuffers)
	  (callback imgui-glfw-swap-buffers-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_SetWindowAlpha)
	  (callback imgui-glfw-set-window-alpha-callback)

	  (foreign-slot-value platform-io '(:struct ig::ImGuiPlatformIO)
			      'ig::Platform_CreateVkSurface)
	  (callback imgui-glfw-create-vk-surface-callback))

    #+windows
    (ig::install_Platform_SetImeInputPos_callback
     platform-io (callback imgui-glfw-set-ime-input-pos-callback-internal))
    
    (let ((main-viewport (ig::igGetMainViewport))
	  (data (alloc-platform-user-data))
	  (window (main-window imgui)))
	    
      (setf (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'Window)
	    (h window)

	    (foreign-slot-value data '(:struct ImGuiViewportDataGlfw) 'WindowOwned)
	    nil)

      (setf (foreign-slot-value main-viewport '(:struct ig::ImGuiViewport)
				'ig::PlatformUserData)
	    data

	    (foreign-slot-value main-viewport '(:struct ig::ImGuiViewport)
				'ig::PlatformHandle)
	    (h window)))

    (values)))

(defun get-viewport-window (viewport)
  (find-window
   (foreign-slot-value viewport '(:struct ig::ImGuiViewport) 'ig::PlatformHandle)))

(defun imgui-glfw-shutdown-platform-interface (imgui)
  (declare (ignore imgui))
  (values))
