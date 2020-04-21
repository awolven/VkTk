(in-package :igp)

(defcallback multiline-input-text-clear-callback :int ((p-data :pointer))
  (multiline-input-text-clear p-data))

(defun multiline-input-text-clear (p-data)
  (let ((flags (foreign-slot-value p-data '(:struct ig::ImGuiInputTextCallbackData) 'ig::EventFlag)))
    (when (not (zerop (logand flags ig::ImGuiInputTextFlags_CallbackCharFilter)))
      (print 'ischar)
      (when (eq (foreign-slot-value p-data '(:struct ig::ImGuiInputTextCallbackData) 'ig::EventChar)
		ig::ImGuiKey_ENTER)
	(print 'is-enter)
	(let ((p-buf (foreign-slot-value p-data '(:struct ig::ImGuiInputTextCallbackData) 'ig::UserData)))
	  (setf (mem-aref p-buf :char 0) 0)
	  (return-from multiline-input-text-clear 1)))
      (return-from multiline-input-text-clear 0))
    (return-from multiline-input-text-clear 0)))

;; the matrix below is used to emulate OpenGL for the 3d-matrices library.
(defparameter +clip-matrix+
  (mat 1 0 0 0
       0 -1 0 0
       0 0 1/2 0
       0 0 1/2 1))

(defvar *selection-mode* :face)

(defparameter *vertices-list*
  (list -0.5f0 -0.5f0 0.5f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 0.5f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 0.5f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 0.5f0 1.0f0 1.0f0 1.0f0

	 -0.5f0 -0.5f0 -0.5f0 1.0f0 0.0f0 0.0f0
	 0.5f0 -0.5f0 -0.5f0 0.0f0 1.0f0 0.0f0
	 0.5f0  0.5f0 -0.5f0 0.0f0 0.0f0 1.0f0
	 -0.5f0  0.5f0 -0.5f0 1.0f0 1.0f0 1.0f0
	))

(defparameter *vertex-data* (foreign-alloc :float :initial-contents *vertices-list*))
(defparameter *vertex-data-size* (* (foreign-type-size :float) (length *vertices-list*)))

(defparameter *indices*
  (list 0 1 2 2 3 0
	4 5 6 6 7 4
	0 1 4 4 5 1
	1 2 6 1 5 6
	0 4 7 0 3 7
	7 6 3 3 2 6))

(defparameter *index-data* (foreign-alloc :unsigned-short :initial-contents *indices*))
(defparameter *index-data-size* (* (foreign-type-size :unsigned-short) (length *indices*)))

(defvar *draw-index-args* nil)

(defparameter *axes-coord-list*
  (list 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
	1.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
	0.0f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0
	0.0f0 1.0f0 0.0f0 0.0f0 1.0f0 0.0f0
	0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 1.0f0
	0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 1.0f0))

(defparameter *axes-coord-data*
  (foreign-alloc :float :initial-contents *axes-coord-list*))

(defparameter *axes-coord-data-size*
  (* (foreign-type-size :float) (length *axes-coord-list*)))

(defparameter *axes-indices*
  (list 0 1 2 3 4 5))

(defparameter *axes-index-data* (foreign-alloc :unsigned-short :initial-contents *axes-indices*))

(defparameter *axes-index-data-size* (* (foreign-type-size :unsigned-short) (length *axes-indices*)))

(defclass application (vulkan-application-mixin)
  ((scene :initarg :scene)
   (editor :initform (make-instance 'editor) :reader editor)
   (pipeline-cache :initform +null-pipeline-cache+ :initarg :pipeline-cache :reader pipeline-cache)
   (allocation-callbacks :initform +null-allocator+ :initarg :allocator :reader allocator)
   (face-renderer :accessor face-renderer)
   (edge-renderer :accessor edge-renderer)
   (annotation-renderer :accessor annotation-renderer)
   (colored-line-strip-renderer :accessor colored-line-strip-renderer)
   (colored-point-vertex-renderer :accessor colored-point-vertex-renderer)))

(defgeneric process-ui (app))

(let ((last-time (get-internal-real-time))
      (frame-counter 0)
      (show-demo-window t)
      (show-another-window nil)
      (temp)
      (dt))

  (defmethod process-ui (app)

    (imgui-new-frame (imgui-module app))

    (let* ((scene (slot-value app 'scene))
	   (camera (slot-value scene 'camera))
	   (editor (editor app)))

      (ig:igBeginMainMenuBar)
      (when (ig:begin-menu "File")
	(ig:menu-item "New")
	(ig:menu-item "Open")
	(when (ig:menu-item "Exit")
	  (throw 'exit nil))
	(ig:end-menu))
      (when (ig:begin-menu "Edit")
	(ig:menu-item "Create")
	(ig:menu-item "Move")
	(ig:menu-item "Delete")
	(ig:end-menu))
      (when (ig:begin-menu "View")
	(when (ig:menu-item "Reset Camera")
	  (reset-camera camera))
	(ig:end-menu))	  
      (ig:igEndMainMenuBar)

      (incf frame-counter)

      (multiple-value-bind (width height) (get-window-size (main-window app))
	(progn
	  (ig:set-next-window-pos 0 19 :condition :first-use-ever)
	  (ig:set-next-window-size 140 (round (/ height 2)))
	  (ig:begin "selection stack")
	  (ig:push-item-width -1)
	  (let ((ss-count (length (selection-stack editor)))
		(ss-strings (mapcar (lambda (object)
				      (foreign-string-alloc (princ-to-string object)))
				    (selection-stack editor))))
	    (with-foreign-object (p :pointer ss-count)
	      (loop for i from 0 below ss-count
		 do (setf (cffi:mem-aref p :pointer i) (elt ss-strings i)))
	      (with-foreign-object (p-selected :int)
		(when (ig:igListBoxStr_Arr "" p-selected p ss-count 7)
		  (print (mem-aref p-selected :int))
		  (finish-output))))
	    (mapcar #'foreign-string-free ss-strings))
	  (ig:end))
	
	(ig:set-next-window-pos 0 0 :condition :always)
	(ig:set-next-window-size width height)
	(ig:begin "background"
		  :no-background t
		  :no-title-bar t
		  :no-resize t
		  :no-move t
		  :no-scrollbar t
		  :no-scroll-with-mouse t
		  :no-collapse t
		  :no-saved-settings t
		  :no-mouse-inputs t
		  :no-focus-on-appearing t
		  :no-bring-to-front-on-focus t
		  :no-nav-inputs t
		  :no-nav-focus t
		  :no-decoration t
		  :no-inputs t)

	(ig:set-cursor-screen-pos 170 50)
	(ig:text "TEST")
	(ig:end)

	(ig:set-next-window-pos (- width 120) 19 :condition :always)
	(ig:set-next-window-size 120 (- height 38))
	(ig:begin "right-docker"
		  :no-title-bar t
		  :no-resize t
		  :no-move t
		  :no-scrollbar t
		  :no-scroll-with-mouse t
		  :no-saved-settings t
		  :no-focus-on-appearing t
		  :no-bring-to-front-on-focus t
		  :no-nav-inputs t
		  :no-nav-focus t
		  :no-decoration t)
	(ig:text "selection mode")
	(when (ig:radio-button "compsolid" (eq *selection-mode* :compsolid))
	  (setq *selection-mode* :compsolid))
	(when (ig:radio-button "compound" (eq *selection-mode* :compound))
	  (setq *selection-mode* :compound))
	(when (ig:radio-button "solid" (eq *selection-mode* :solid))
	  (setq *selection-mode* :solid))
	(when (ig:radio-button "shell" (eq *selection-mode* :shell))
	  (setq *selection-mode* :shell))	
	(when (ig:radio-button "face" (eq *selection-mode* :face))
	  (setq *selection-mode* :face))
	(when (ig:radio-button "wire" (eq *selection-mode* :wire))
	  (setq *selection-mode* :wire))
	(when (ig:radio-button "edge" (eq *selection-mode* :edge))
	  (setq *selection-mode* :edge))
	(when (ig:radio-button "vertex" (eq *selection-mode* :vertex))
	  (setq *selection-mode* :vertex))

	(ig:new-line)
	(ig:text "view projection")
	(when (ig:radio-button "perspective" (perspective-p camera))
	  (setf (slot-value camera 'type) :perspective))
	(when (ig:radio-button "orthographic" (ortho-p camera))
	  (setf (slot-value camera 'type) :ortho))
	(update-projection-matrix camera)

	(ig:new-line)
	(with-foreign-object (p-f :float 3)
	  (let ((ap (slot-value camera 'aim-point)))
	    (setf (mem-aref p-f :float 0) (vx ap)
		  (mem-aref p-f :float 1) (vy ap)
		  (mem-aref p-f :float 2) (vz ap))
	    
	    (ig:igInputFloat3 "aim point" p-f "%f %f %f" 0)

	    (setf (slot-value camera 'aim-point)
		  (vec3 (mem-aref p-f :float 0) (mem-aref p-f :float 1) (mem-aref p-f :float 2)))
	    (update-view-matrix camera)))
	(ig:end)

	(ig:set-next-window-pos 0 (- height 20) :condition :always)
	(ig:set-next-window-size width 20)

	(ig:begin "bottom-docker"
		  :no-title-bar t
		  :no-resize t)
	(if (elt (camera-mode scene) 2)
	    (progn
	      (ig:set-cursor-screen-pos 5 (- height 15))
	      (ig:text "move to rotate camera")
	      (ig:set-cursor-screen-pos (- (/ width 2) 40) (- height 15))
	      (ig:text "M: accept")
	      (ig:set-cursor-screen-pos (- width 150) (- height 15))
	      (ig:text "[Ctrl] + move to pan"))
	    (progn
	      (ig:set-cursor-screen-pos 5 (- height 15))
	      (ig:text "L: select")
	      (ig:set-cursor-screen-pos (- (/ width 2) 40) (- height 15))
	      (ig:text "M: press: camera mode, scroll: zoom")))
	(ig:end))      
		 
      (ig:text "Hello, wworld!")

      (ig:text "camera up: ~S" (up-vector camera))
      
      (ig:color-edit "clear color" (clear-value app))
		 
      (ig:text "~S" (clear-value app))
		 
      (ig:text "camera position: ~S" (eye-point camera))

      (ig:text "camera right: ~S" (vc (v- (slot-value camera 'aim-point) (eye-point camera)) (up-vector camera)))
		 
      (let* ((ortho-p (ortho-p camera)))
	       
	(when (ig:button (if ortho-p "Set Perspective" "Set Orthographic") :dx 120.0f0 :dy 20.0f0)
	  (setf (slot-value camera 'type) (if ortho-p :perspective :ortho))
	  (update-projection-matrix camera)))

      (when (ig:button "Reset Camera" :dx 120.0f0 :dy 20.0f0)
	(reset-camera camera))

      (when (ig:button "Demo Window" :dx 100.0f0 :dy 20.0f0)
	(if show-demo-window
	    (setq show-demo-window nil)
	    (setq show-demo-window t)))
      
      (when (ig:button "Another Window" :dx 120.0f0 :dy 20.0f0)
	(if show-another-window
	    (setq show-another-window nil)
	    (setq show-another-window t)))
      
      (setq temp (get-internal-real-time)
	    dt (- temp last-time))
      
      (ig:text "~s ms/frame (~s FPS)" dt
	    (if (zerop dt)
		0
		(round (/ frame-counter (/ dt 1000.0f0)))))
      
      (setq frame-counter 0
	    last-time temp)
      
      (when show-another-window
	
	(multiple-value-bind (ignore open)
	    (ig:begin "Another Window" :open show-another-window)
	  (declare (ignore ignore))
	  
	  (setq show-another-window open)
	
	  (ig:text "Hello from another window!")
	
	  (ig:end)))
      
      (when show-demo-window
	(ig:set-next-window-pos 650.0f0 20.0f0 :condition :first-use-ever)
	
	(setq show-demo-window (ig:show-demo-window show-demo-window)))

      (maybe-move-camera camera))))

(defun main (app &rest args)
  (declare (ignore args))

  (let* ((device (default-logical-device app))
	 (main-window (main-window app))
	 (render-pass (render-pass main-window))
	 (index (queue-family-index (render-surface main-window)))
	 (queue (find-queue device index))
	 (command-pool (find-command-pool device index))
	 (command-buffer (elt (command-buffers command-pool) 0)))
    
    (reset-command-pool device command-pool)
  
    (begin-command-buffer command-buffer)
    
    (imgui-create-fonts-texture (imgui-module app) command-buffer)
    
    (end-command-buffer command-buffer)
    
    (queue-submit queue command-buffer)
    
    (device-wait-idle device)
    
    (imgui-invalidate-font-upload-objects (imgui-module app))
    
    (let ((current-frame 0)
	  (image-index)
	  (clear-value (clear-value app)))
      (catch 'exit
	(loop while (zerop (glfw:glfwWindowShouldClose (h main-window)))
	   do (glfw:glfwPollEvents)

	     (let* ((swapchain (swapchain main-window))
		    ;; fetch swapchain anew in case of recreate swapchain
		    (command-buffer (frame-command-buffer (elt (frame-resources swapchain) current-frame))))
	       (process-ui app)
		 
	       (setq image-index
		     (frame-begin swapchain render-pass current-frame clear-value))
		 
	       (render-graphics app main-window command-buffer queue current-frame)
		 
	       (frame-end swapchain queue current-frame)
		 
	       (frame-present swapchain queue current-frame image-index main-window)

	       (setq current-frame (mod (1+ current-frame) (number-of-images swapchain))))))
      (shutdown-application app))))

(defvar *app* nil)

(defun run-demo (&rest args &key (debug t) (width 1280) (height 720) &allow-other-keys)
  (declare (ignore width height))
  #+windows
  (sb-thread:make-thread
   (lambda ()
     (let ((*debug* debug))
       ;; app must be created in the thread that will run main
       (setq *app* (apply #'make-instance 'application args))
       #+NIL(igp::append-circle (slot-value *app* 'igp::editor) (3d-vectors::vec2 10 20) 40 (make-array 4 :element-type 'single-float :initial-element 0.5))
       (main *app*)))
   :name "graphics")

  #+linux
  (sb-thread:make-thread
   (lambda ()
  (sb-int:with-float-traps-masked
   (:invalid
    :inexact
    :overflow
    :underflow
    :divide-by-zero)
   (main (setq *app* (apply #'make-instance 'application args)))))
   :name "graphics")
  
  #+darwin
  (sb-thread:interrupt-thread
   (sb-thread:main-thread)
   (lambda ()
     (sb-int:with-float-traps-masked
	 (:invalid :inexact :overflow)
       (main (setq *app* (apply #'make-instance 'application args)))))))

(defclass renderer-mixin ()
  ((application :reader application :initarg :app)
   (window :accessor window :initarg :window)
   (scene :reader scene :initarg :scene)
   (allocator :accessor allocator :initarg :allocator)
   (device :accessor device :initarg :device)
   (render-pass :accessor render-pass :initarg :render-pass)
   (pipeline-cache :accessor pipeline-cache :initarg :pipeline-cache)
   (descriptor-pool :accessor descriptor-pool :initarg :descriptor-pool)
   (descriptor-set-layout :accessor descriptor-set-layout)
   (descriptor-set :accessor descriptor-set)
   (pipeline-layout :accessor pipeline-layout)
   (pipeline :accessor pipeline)
   (uniform-buffer-vs :accessor uniform-buffer-vs)))

(defmethod main-window ((renderer renderer-mixin))
  (main-window (slot-value renderer 'scene)))

(defclass face-renderer (renderer-mixin)
  ())

(defmethod pipeline-topology ((renderer face-renderer))
  vk:VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST)

(defmethod vertex-shader-file-name ((renderer face-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/vert.spv"))

(defmethod fragment-shader-file-name ((renderer face-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/frag.spv"))

(defclass edge-renderer (renderer-mixin)
  ())

(defmethod pipeline-topology ((renderer edge-renderer))
  vk:VK_PRIMITIVE_TOPOLOGY_LINE_STRIP)

(defmethod vertex-shader-file-name ((renderer edge-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.vert.spv"))

(defmethod fragment-shader-file-name ((renderer edge-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.frag.spv"))


(defclass annotation-renderer (renderer-mixin)
  ())

(defmethod pipeline-topology ((renderer annotation-renderer))
  vk:VK_PRIMITIVE_TOPOLOGY_LINE_LIST)

(defmethod vertex-shader-file-name ((renderer annotation-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.vert.spv"))

(defmethod fragment-shader-file-name ((renderer annotation-renderer))
  (asdf/system:system-relative-pathname :vktk "shaders/lines.frag.spv"))

(defmethod initialize-instance :after ((renderer renderer-mixin) &rest initargs
				       &key &allow-other-keys)
  (declare (ignore initargs))
  (create-device-objects renderer)
  (values))

(defmethod make-vertex-input-attribute-descriptions ((renderer renderer-mixin))
  (list (make-instance 'vertex-input-attribute-description
		       :location 0
		       :format vk:VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type renderer) 'position))
	(make-instance 'vertex-input-attribute-description
		       :location 1
		       :format vk:VK_FORMAT_R32G32B32_SFLOAT
		       :offset (foreign-slot-offset (pipeline-vertex-type renderer) 'color))))

(defmethod make-descriptor-set-layout-bindings ((renderer renderer-mixin))
  (list (make-instance 'uniform-buffer-for-vertex-shader-dsl-binding)))

(defmethod make-descriptor-buffer-info ((renderer renderer-mixin))
    (list (make-instance 'descriptor-uniform-buffer-info
			 :buffer (uniform-buffer-vs renderer)
			 :range (foreign-type-size (uniform-buffer-type renderer)))))

(defmethod make-push-constant-ranges ((renderer renderer-mixin))
  nil)

(defcstruct vec2
  (a :float)
  (b :float))

(defcstruct vec3
  (a :float)
  (b :float)
  (c :float))

(defcstruct 3DVertex
  (position (:struct vec3))
  (color (:struct vec3)))

(defmethod pipeline-vertex-type ((renderer renderer-mixin))
  '(:struct 3DVertex))

(defmethod pipeline-depth-test-enable? ((renderer renderer-mixin))
  t)

(defmethod pipeline-depth-write-enable? ((renderer renderer-mixin))
  t)

(defmethod pipeline-depth-compare-op ((renderer renderer-mixin))
  vk:VK_COMPARE_OP_LESS)

(defmethod pipeline-logic-op ((renderer renderer-mixin))
  vk:VK_LOGIC_OP_COPY)

(defmethod pipeline-blend-enable? ((renderer renderer-mixin))
  nil)
  
(defmethod pipeline-depth-clamp-enable? ((renderer renderer-mixin))
  nil)

(defmethod pipeline-src-alpha-blend-factor ((renderer renderer-mixin))
  vk:VK_BLEND_FACTOR_ONE)

(defmethod pipeline-min-sample-shading ((renderer renderer-mixin))
  1.0f0)
  
(defun create-standard-renderer-device-objects (renderer
						&key (bindings (make-descriptor-set-layout-bindings renderer))
						  (push-constant-ranges (make-push-constant-ranges renderer))
						  (line-width #+(or windows linux) 2.0f0 #+darwin 1.0f0)
						  (vertex-type (pipeline-vertex-type renderer))
						  (vertex-input-attribute-descriptions
						   (make-vertex-input-attribute-descriptions renderer))
						  (topology (pipeline-topology renderer))
						  (min-sample-shading (pipeline-min-sample-shading renderer))
						  (depth-test-enable (pipeline-depth-test-enable? renderer))
						  (depth-write-enable (pipeline-depth-write-enable? renderer))
						  (depth-compare-op (pipeline-depth-compare-op renderer))
						  (logic-op (pipeline-logic-op renderer))
						  (blend-enable (pipeline-blend-enable? renderer))
						  (depth-clamp-enable (pipeline-depth-clamp-enable? renderer))
						  (src-alpha-blend-factor (pipeline-src-alpha-blend-factor renderer)))

  (with-slots (device) renderer
    (multiple-value-bind (width height) (get-framebuffer-size (window renderer))
      (let ((vtx-shader (create-shader-module-from-file device (vertex-shader-file-name renderer)))
	    (frg-shader (create-shader-module-from-file device (fragment-shader-file-name renderer))))

	(setf (descriptor-set-layout renderer) (create-descriptor-set-layout device :bindings bindings))

	(setf (pipeline-layout renderer)
	      (create-pipeline-layout device (list (descriptor-set-layout renderer))
				      :push-constant-ranges push-constant-ranges)
	      (pipeline renderer)
	      (create-graphics-pipeline device (pipeline-cache renderer) (pipeline-layout renderer)
					(render-pass renderer) 1 width height vtx-shader frg-shader
					:line-width line-width
					:vertex-type vertex-type
					:vertex-input-attribute-descriptions vertex-input-attribute-descriptions
					:topology topology
					:min-sample-shading min-sample-shading
					:depth-test-enable (if depth-test-enable (if (eq depth-test-enable vk:VK_FALSE) vk:VK_FALSE vk:VK_TRUE) vk:VK_FALSE)
					:depth-write-enable (if depth-write-enable (if (eq depth-write-enable vk:VK_FALSE) vk:VK_FALSE vk:VK_TRUE) vk:VK_FALSE)
					:depth-compare-op depth-compare-op
					:logic-op logic-op
					:blend-enable (if blend-enable (if (eq blend-enable vk:VK_FALSE) vk:VK_FALSE vk:VK_TRUE) vk:VK_FALSE)
					:depth-clamp-enable (if depth-clamp-enable (if (eq depth-clamp-enable vk:VK_FALSE) vk:VK_FALSE vk:VK_TRUE) vk:VK_FALSE)
					:src-alpha-blend-factor src-alpha-blend-factor))

	(destroy-shader-module vtx-shader)
	(destroy-shader-module frg-shader)
	  
	(setf (uniform-buffer-vs renderer)
	      (create-uniform-buffer device (foreign-type-size (uniform-buffer-type renderer))))
	  
	(setf (descriptor-set renderer)
	      (create-descriptor-set
	       device
	       (list (descriptor-set-layout renderer))
	       (descriptor-pool renderer)
	       :descriptor-buffer-info
	       (make-descriptor-buffer-info renderer))))
      (values))))

(defmethod create-device-objects ((renderer renderer-mixin))
  (create-standard-renderer-device-objects renderer))

(defcstruct vec4
  (x :float)
  (y :float)
  (z :float)
  (w :float))

(defcstruct mat4
  (x0 :float)
  (y0 :float)
  (z0 :float)
  (w0 :float)
  (x1 :float)
  (y1 :float)
  (z1 :float)
  (w1 :float)
  (x2 :float)
  (y2 :float)
  (z2 :float)
  (w2 :float)
  (x3 :float)
  (y3 :float)
  (z3 :float)
  (w3 :float))

(defcstruct 3DDemoVSUBO
  (model (:struct mat4))
  (view (:struct mat4))
  (proj (:struct mat4))
  (clip (:struct mat4)))


(defun euclid (vec4)
  (vec3 (/ (vx vec4) (vw vec4))
	(/ (vy vec4) (vw vec4))
	(/ (vz vec4) (vw vec4))))

(defun compute-picking-ray (window camera view-matrix projection-matrix clip-matrix)
  (multiple-value-bind (viewport-width viewport-height) (get-window-size window)
      (let ((unclip (minv clip-matrix))
	    (unproj (minv projection-matrix))
	    (unview (minv view-matrix)))
	(let ((p (eye-point camera)))
	  (multiple-value-bind (mouse-x mouse-y) (get-cursor-pos window)
	    (let* ((mouse-clip (vec4 (- (/ (* 2.0f0 mouse-x) viewport-width) 1.0f0)
				     (- (/ (* 2.0f0 mouse-y) viewport-height) 1.0f0)
				     (if (perspective-p camera) 0.3254f0 0.0f0)
				     1.0f0))
		   (mouse-world (euclid (m* unview unproj unclip mouse-clip))))
	      ;;(ig:igText (format nil "mouse-world: ~S" mouse-world))
	      (let ((ray
		     (if (perspective-p camera)
			 (make-ray :origin p :direction (vunit (v- mouse-world p)))
			 (let ((dir (vunit (v- (slot-value camera 'aim-point) p))))
			   (make-ray :origin mouse-world :direction dir)))))
		;;(ig:igText (format nil "~S" ray))
		ray)))))))

(defmethod uniform-buffer-type ((renderer renderer-mixin))
  '(:struct 3DDemoVSUBO))

(defun update-vertex-shader-uniform-buffer (module model-matrix view-matrix proj-matrix clip-matrix)

  (with-foreign-object (p-ubo (uniform-buffer-type module))
	(let ((p-m (foreign-slot-pointer p-ubo (uniform-buffer-type module) 'model))
	      (p-v (foreign-slot-pointer p-ubo (uniform-buffer-type module) 'view))
	      (p-p (foreign-slot-pointer p-ubo (uniform-buffer-type module) 'proj))
	      (p-c (foreign-slot-pointer p-ubo (uniform-buffer-type module) 'clip)))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-m :float (+ i (* j 4))) (coerce (mcref model-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-v :float (+ i (* j 4))) (coerce (mcref view-matrix i j) 'single-float))))

	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-p :float (+ i (* j 4))) (coerce (mcref proj-matrix i j) 'single-float))))


	  (loop for i from 0 below 4
	     do (loop for j from 0 below 4
		   do (setf (mem-aref p-c :float (+ i (* j 4))) (coerce (mcref clip-matrix i j) 'single-float))))
	  
	  (copy-uniform-buffer-memory (device module)
				      p-ubo (allocated-memory (uniform-buffer-vs module))
				      (foreign-type-size (uniform-buffer-type module)))))
      (values))




(defun 3d-demo-select (queue renderer model-matrix view-matrix projection-matrix)
  (let ((scene (slot-value renderer 'scene)))
    
    (let* ((ray (compute-picking-ray (slot-value renderer 'window)
				     (slot-value (slot-value renderer 'scene) 'camera)
				     view-matrix projection-matrix +clip-matrix+))
	   (box-intersecting (ray-intersects-bvh ray (first (slot-value scene 'standins)) model-matrix)))
      (let* ((app (slot-value renderer 'application))
	     (editor (slot-value app 'editor))
	     (dynamic-database (dynamic-database editor)))
	(setf (line-strip-vertices-fill-pointer dynamic-database) 0)
	(setf (line-strip-indices-fill-pointer dynamic-database) 0)
	(setf (fill-pointer (line-strip-commands dynamic-database)) 0)

	(when (eq (editor-mode editor) :line-first-point)
	  (append-line (dynamic-database editor)
		       (first (selection-stack editor))
		       (intersect-ray-with-plane
			(compute-picking-ray (slot-value renderer 'window)
					     (slot-value (slot-value renderer 'scene) 'camera)
					     view-matrix projection-matrix +clip-matrix+)		 
			(make-plane :equation (vec4 0 0 1 0)))))
	
	(if (intersect-bezier-curve-and-ray (oc::make-bezier-curve-3d (list (gp:pnt 0.0 100.0 0.0)
									  (gp:pnt 50 90 0.0)
									  (gp:pnt 10.1 20.0 0.0)
									  (gp:pnt -050 -10.0 0.0))
								    (list 1 1 1 1))
					    ray dynamic-database)
	    (append-geom2d-curve dynamic-database
				 (oc::make-bezier-curve-2d (list (gp:pnt2d 0.0 100.0)
								 (gp:pnt2d 50 90)
								 (gp:pnt2d 10.1 20.0)
								 (gp:pnt2d -050 -10.0))
							   (list 1 1 1 1))
				 (make-array 4 :element-type 'single-float :initial-contents (list 1.0f0 0.0f0 0.0f0 1.0f0))))
	(append-geom2d-curve dynamic-database
			     (oc::make-bezier-curve-2d (list (gp:pnt2d 0.0 100.0)
							     (gp:pnt2d 50 90)
							     (gp:pnt2d 10.1 20.0)
							     (gp:pnt2d -050 -10.0))
						       (list 1 1 1 1))
			     (make-array 4 :element-type 'single-float :initial-contents (list 1.0f0 1.0f0 1.0f0 1.0f0))))
	
      
	    (when box-intersecting
	      (cond ((eq *selection-mode* :face)
		     (let* ((toplevel-standin (first (slot-value scene 'standins))))
		       (unhighlight-toplevel-standin queue toplevel-standin)
		       (let ((selected (caar
					(sort (remove-if #'null
							 (mapcar #'(lambda (face)
								     (let ((tmin (ray-intersects-face-p ray face model-matrix)))
								       (when tmin
									 (list face
									       tmin))))
								 box-intersecting))
					      #'< :key #'cadr))))
			 (when selected
			   (highlight-standin queue selected toplevel-standin)
			   (ig:igText (format nil "~S" (slot-value selected 'random-id))))))))
	      (ig:igText "SELECTED!")))))


(defun format-text-at-world-position (window position matrix
				      format-string &rest format-args)
  (multiple-value-bind (width height) (get-window-size window)
    (let* ((clip (euclid (m* matrix position)))
	   (ndcx (round (/ (* (+ 1.0 (vx clip)) width) 2.0)))
	   (ndcy (round (/ (* (+ 1.0 (vy clip)) height) 2.0))))
      (ig:set-next-window-pos 0 0 :condition :always)
      (ig:set-next-window-size width height)
      (ig:begin "background"
		:no-background t
		:no-title-bar t
		:no-resize t
		:no-move t
		:no-scrollbar t
		:no-scroll-with-mouse t
		:no-collapse t
		:no-saved-settings t
		:no-mouse-inputs t
		:no-focus-on-appearing t
		:no-bring-to-front-on-focus t
		:no-nav-inputs t
		:no-nav-focus t
		:no-decoration t
		:no-inputs t)
       
      (ig:set-cursor-screen-pos ndcx ndcy)
      (ig:text (apply #'format nil format-string format-args))
      (ig:end))))

(defmethod 3d-demo-render (queue (renderer face-renderer) command-buffer fb-width fb-height
			   &optional (model-matrix (meye 4)))

  (let* ((scene (slot-value renderer 'scene))
	 (camera (slot-value scene 'camera))
	 (view-matrix (slot-value camera 'view-matrix))
	 (projection-matrix (slot-value camera 'projection-matrix)))

    (let* ((current-time (get-internal-real-time))
	   (elapsed-time (- current-time (start-time (slot-value renderer 'scene)))))

      (setq model-matrix 
	    (m* (mrotation (vec3 0.0 0.0 1.0)
			   (rem (* (/ elapsed-time 1000.0d0) #.(/ pi 4.0d0)) #.(* pi 2.0d0)))
		(mtranslation (vec3 100.0 0.0 0.0)))))
#+NIL
    (format-text-at-world-position (window renderer)
				   (vec4 0 0 0 1)
				   (m* +clip-matrix+ projection-matrix view-matrix model-matrix)
				   "hello")

    (let* ((position (vec4 0 0 0 1))
	   (position-clip (euclid (m* +clip-matrix+ projection-matrix view-matrix model-matrix position)))
	   (ndc-x (round (/ (* (+ 1.0 (vx position-clip)) #+darwin 0.5 fb-width) 2.0)))
	   (ndc-y (round (/ (* (+ 1.0 (vy position-clip)) #+darwin 0.5 fb-height) 2.0)))
	   (hover-x (+ ndc-x 25))
	   (hover-y (+ ndc-y 25)))

      (ig:set-next-window-pos 0 0 :condition :always)
      (ig:set-next-window-size fb-width fb-height)
      (ig:begin "background"
		:no-background t
		:no-title-bar t
		:no-resize t
		:no-move t
		:no-scrollbar t
		:no-scroll-with-mouse t
		:no-collapse t
		:no-saved-settings t
		:no-mouse-inputs t
		:no-focus-on-appearing t
		:no-bring-to-front-on-focus t
		:no-nav-inputs t
		:no-nav-focus t
		:no-decoration t
		:no-inputs t)

      (ig:set-cursor-screen-pos hover-x hover-y)
      (ig:text "<~S, ~S>" hover-x hover-y)

      (let* ((app (slot-value renderer 'application))
	     (editor (slot-value app 'editor))
	     (command-stream (command-stream editor)))
	(with-foreign-object (p-c :float 4)
	  (setf (mem-aref p-c :float 0) 0.0f0
		(mem-aref p-c :float 1) 0.0f0
		(mem-aref p-c :float 2) 0.0f0
		(mem-aref p-c :float 3) 1.0f0)
	  
	  (let* ((history (slot-value command-stream 'history)))
	    (loop for i from (length history) downto 1
	       with y = (- (/ fb-height #+darwin 2 #+(or windows linux) 1) (* 7 (ig:iggettextlineheight)))
	       with prev-newline = (length history)
	       when (minusp y)
	       do (return)
	       when (char= (char history (1- i)) #\newline)
	       do (ig:set-cursor-screen-pos 20 y)
		 (ig::igtextcolored p-c (subseq (slot-value command-stream 'history) i prev-newline))
		 (setq prev-newline (1- i))
		 (decf y (ig:iggettextlineheight)))
	    
	    (let ((input-vertical-y (- (/ fb-height #+darwin 2 #+(or windows linux) 1) 70)))
	      (ig:set-cursor-screen-pos 4 input-vertical-y)
	      (setf (mem-aref p-c :float 0) 1.0f0)
	      (ig::igtextcolored p-c "->")
	      (ig:set-cursor-screen-pos 5 (1+ input-vertical-y))
	      (setf (mem-aref p-c :float 1) 1.0f0)
	      (setf (mem-aref p-c :float 2) 1.0f0)
	      (ig::igtextcolored p-c "->")
	      (ig:set-cursor-screen-pos 30 input-vertical-y)
	      (with-foreign-objects ((p-buf '(:array :char 1024))
				     (p-buf-size :int)
				     (p-size '(:struct ig::ImVec2)))
		(setf (mem-aref p-buf-size :int) 1024
		      (mem-aref (foreign-slot-pointer p-size '(:struct ig::ImVec2) 'ig::x) :float) 1120.0f0
		      (mem-aref (foreign-slot-pointer p-size '(:struct ig::ImVec2) 'ig::y) :float) (* 15.0f0 3))
		;;(let ((lock (slot-value command-stream 'input-lock)))
		;;(sb-thread:with-mutex (lock)

		  (when (IMGUI:IGINPUTTEXTMULTILINE "##command" p-buf 1024 p-size
						    (logior IMGUI:IMGUIINPUTTEXTFLAGS_CTRLENTERFORNEWLINE
							    IMGUI:IMGUIINPUTTEXTFLAGS_ENTERRETURNSTRUE
							    IMGUI:ImGuiInputTextFlags_CallbackAlways)
						    (callback multiline-input-text-clear-callback) p-buf)
		    (print 'im-here1!)
		    (finish-output)
		    (unless (imgui::igIsItemActive)
		      (print 'im-here2!)
		      (finish-output)
		      (let ((string (cffi:foreign-string-to-lisp p-buf)))
			(handler-case (with-input-from-string (stream string)
					(let ((cl:*read-eval* nil)) (read stream))
					(cond ((string-equal string "Line")
					       (setf (editor-mode editor) :line-zeroth-point)))
					      
					#+notyet
					(loop for char across string
					   do (vector-push-extend char (slot-value command-stream 'input-buffer))
					   finally (vector-push-extend #\newline (slot-value command-stream 'input-buffer)))
					(setf (mem-aref p-buf :char 0) 0)
					(ig:igsetwindowfocusstr "##command"))
			(cl:end-of-file ()))))))))));;))
      (ig:end))      

    (3d-demo-select queue renderer model-matrix view-matrix projection-matrix)

    (cmd-bind-pipeline command-buffer (pipeline renderer) :bind-point :graphics)

    (update-vertex-shader-uniform-buffer renderer model-matrix view-matrix projection-matrix +clip-matrix+)

    (cmd-set-viewport command-buffer :width fb-width :height fb-height)

    (cmd-set-scissor command-buffer :width fb-width :height fb-height)

    (loop for standin in (slot-value scene 'standins)
       do (with-slots (device) renderer
	    (unless (face-vertex-buffer standin)
	      (let ((vertex-array (face-vertex-array standin)))
		(setf (face-vertex-buffer standin)
		      (sb-sys:with-pinned-objects (vertex-array)
			(create-vertex-buffer device
						   (sb-sys:vector-sap vertex-array)
						   (* (length (face-vertex-array standin))
						      (load-time-value (foreign-type-size :float))))))))
	    (unless (face-index-buffer standin)
	      (let ((index-array (face-index-array standin)))
		(setf (face-index-buffer standin)
		      (sb-sys:with-pinned-objects (index-array)
			(create-index-buffer device
						  (sb-sys:vector-sap index-array)
						  (* (length (face-index-array standin))
						     (load-time-value (foreign-type-size :unsigned-short)))))))))

	 (cmd-bind-vertex-buffers command-buffer (list (face-vertex-buffer standin)))

	 (cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer) (list (descriptor-set renderer)))
	 (cmd-bind-index-buffer command-buffer (face-index-buffer standin))

	 (loop for cmd across (face-commands standin)
	    do (cmd-draw-indexed command-buffer cmd))))
  
  model-matrix)

(defmethod 3d-demo-render (queue (renderer edge-renderer) command-buffer fb-width fb-height
			   &optional (model-matrix (meye 4)))

  (let* ((scene (slot-value renderer 'scene))
	 (camera (slot-value scene 'camera))
	 (view-matrix (slot-value camera 'view-matrix))
	 (projection-matrix (slot-value camera 'projection-matrix)))
    
    (cmd-bind-pipeline command-buffer (pipeline renderer) :bind-point :graphics)
    
    (update-vertex-shader-uniform-buffer renderer model-matrix view-matrix projection-matrix +clip-matrix+)
	
    (cmd-set-viewport command-buffer :width fb-width :height fb-height)
      
    (cmd-set-scissor command-buffer :width fb-width :height fb-height)
    
    (loop for standin in (slot-value scene 'standins)
       do (with-slots (device) renderer
	    (unless (edge-vertex-buffer standin)
	      (let ((vertex-array (edge-vertex-array standin)))
		(setf (edge-vertex-buffer standin)
		      (sb-sys:with-pinned-objects (vertex-array)
			(create-vertex-buffer device (sb-sys:vector-sap vertex-array)
						   (* (length (edge-vertex-array standin))
						      (load-time-value (foreign-type-size :float))))))))
	    (unless (edge-index-buffer standin)
	      (let ((index-array (edge-index-array standin)))
		(setf (edge-index-buffer standin)
		      (sb-sys:with-pinned-objects (index-array)
			(create-index-buffer device (sb-sys:vector-sap index-array)
						  (* (length (edge-index-array standin))
						     (load-time-value (foreign-type-size :unsigned-short)))))))))

	 (cmd-bind-vertex-buffers command-buffer (list (edge-vertex-buffer standin)))
	 
	 (cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer) (list (descriptor-set renderer)))
	 
	 (cmd-bind-index-buffer command-buffer (edge-index-buffer standin))
	 
	 (loop for cmd across (edge-commands standin)
	    do (cmd-draw-indexed command-buffer cmd))))
    
    model-matrix)

(defmethod 3d-demo-render (queue (renderer annotation-renderer) command-buffer fb-width fb-height
			   &optional (model-matrix (meye 4)))

  (let* ((scene (slot-value renderer 'scene))
	 (camera (slot-value scene 'camera))
	 (view-matrix (slot-value camera 'view-matrix))
	 (projection-matrix (slot-value camera 'annotation-projection-matrix)))
	
    (with-slots (window) renderer
      (let ((camera-distance (slot-value camera 'distance)))
	    
	(nmscale model-matrix (vec3 camera-distance camera-distance camera-distance)))

      (cmd-bind-pipeline command-buffer (pipeline renderer) :bind-point :graphics)	  
	  
      (update-vertex-shader-uniform-buffer renderer model-matrix view-matrix projection-matrix +clip-matrix+))

    (cmd-set-viewport command-buffer :width fb-width :height fb-height)

    (cmd-set-scissor command-buffer  :width fb-width :height fb-height)

    (let* ((annotation (slot-value (slot-value renderer 'scene) 'annotation))
	   (index-array (line-index-array annotation))
	   (vertex-array (line-vertex-array annotation))
	   (device (device renderer)))
	
      (unless (vertex-buffer annotation)
		  
	(setf (vertex-buffer annotation)
	      (sb-sys:with-pinned-objects
		  (vertex-array)
		(create-vertex-buffer device (sb-sys:vector-sap vertex-array)
				      (* (length (line-vertex-array annotation))
					 (load-time-value (foreign-type-size :float)))))))
      (unless (index-buffer annotation)
	(setf (index-buffer annotation)
	      (sb-sys:with-pinned-objects
		  (index-array)
		(create-index-buffer device (sb-sys:vector-sap index-array)
				     (* (length (line-index-array annotation))
					(load-time-value (foreign-type-size :unsigned-short)))))))

      (cmd-bind-vertex-buffers command-buffer (list (vertex-buffer annotation)))
	
      (cmd-bind-descriptor-sets command-buffer (pipeline-layout renderer) (list (descriptor-set renderer)))
	
      (cmd-bind-index-buffer command-buffer (index-buffer annotation))
	
      (cmd-draw-indexed command-buffer (make-draw-indexed-cmd :index-count (length index-array)
							      :first-index 0
							      :vertex-offset 0))))
  model-matrix)

(defmethod render-graphics (app window command-buffer queue current-frame)
  (let ((model-matrix))
    (multiple-value-bind (w h) (get-framebuffer-size window)
    
      (3d-demo-render queue (annotation-renderer app) command-buffer w h)

      (setq model-matrix (3d-demo-render queue (face-renderer app) command-buffer w h))

      (let ((camera (slot-value (slot-value app 'scene) 'camera))
	    (editor (slot-value app 'editor)))
	(editor-render-draw-lists editor (colored-line-strip-renderer app) command-buffer current-frame w h (meye 4) (slot-value camera 'view-matrix) (slot-value camera 'projection-matrix))
	(editor-render-draw-lists editor (colored-point-vertex-renderer app) command-buffer current-frame w h (meye 4) (slot-value camera 'view-matrix) (slot-value camera 'projection-matrix)))

      (3d-demo-render queue (edge-renderer app) command-buffer w h model-matrix)

      (imgui-render (imgui-module app) command-buffer current-frame))))

(defclass camera ()
  ((type :initform :perspective :accessor camera-type :initarg :type)
   (window :initarg :window)
   (scene :initarg :scene)
   (aim-point :initform (vec3 0 0 30))
   (azimuth :initform -45.0f0)
   (elevation :initform 25.0f0)
   (distance :initform 300)
   (pan-x :initform 0.0f0)
   (pan-y :initform 0.0f0)
   
   (fov :initform 45.0f0)
   
   (hither :initform  10000.0f0)
   (yon :initform 0.1f0)

   (zoom :initform 1.0f0)
   
   (view-frustum)
   
   (updated :initform nil)

   (view-matrix)
   (projection-matrix)
   (annotation-projection-matrix)))

(defmethod initialize-instance :after ((instance camera) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (update-view-matrix instance)
  (update-projection-matrix instance))



(defclass annotation ()
  ((vertex-array :initarg :vertex-array :accessor line-vertex-array)
   (index-array :initarg :index-array :accessor line-index-array)
   
   (vertex-buffer :accessor vertex-buffer :initform nil)
   (index-buffer :accessor index-buffer :initform nil)))

(defclass scene ()
  ((app :initarg :app)
   (start-time :reader start-time :initform (get-internal-real-time))
   (camera-mode :initform (make-array 3 :initial-element nil) :accessor camera-mode)
   (camera :initarg :camera :accessor camera :initform nil)
   (standins :initform (list (create-standin (oc::make-bottle))))
   (annotation :initform (let ((vertices-list
				(list 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
				      1.0f0 0.0f0 0.0f0 1.0f0 0.0f0 0.0f0
				      0.0f0 0.0f0 0.0f0 0.0f0 1.0f0 0.0f0
				      0.0f0 1.0f0 0.0f0 0.0f0 1.0f0 0.0f0
				      0.0f0 0.0f0 0.0f0 0.0f0 0.0f0 1.0f0
				      0.0f0 0.0f0 1.0f0 0.0f0 0.0f0 1.0f0))
			       (indices (list 0 1 2 3 4 5)))
			   (make-instance 'annotation
					  :vertex-array
					  (make-array (length vertices-list)
						      :element-type 'single-float
						      :initial-contents vertices-list)
					  :index-array
					  (make-array (length indices)
						      :element-type '(unsigned-byte 16)
						      :initial-contents indices))))))

(defmethod destroy-device-objects ((app application))
  (let* ((window (main-window app))
	 (swapchain (swapchain window)))

    (when (depth-image-view swapchain)
      (destroy-image-view (depth-image-view swapchain))
      (setf (depth-image-view swapchain) nil))
    
    (when (depth-image swapchain)
      (destroy-image (depth-image swapchain))
      (setf (depth-image swapchain) nil))
    
    (loop for image-view across (color-image-views swapchain)
       do (destroy-image-view image-view)
       finally (setf (color-image-views swapchain) nil))
    
    (loop for framebuffer across (framebuffers swapchain)
       do (destroy-framebuffer framebuffer)
       finally (setf (framebuffers swapchain) nil))

    (destroy-device-objects (slot-value app 'scene))
    
    (values)))
  
(defmethod destroy-device-objects ((renderer renderer-mixin))
    
    (when (pipeline renderer)
      (destroy-pipeline (pipeline renderer))
      (setf (pipeline renderer) nil))
    
    (when (pipeline-layout renderer)
      (destroy-pipeline-layout (pipeline-layout renderer))
      (setf (pipeline-layout renderer) nil))

    
    (when (descriptor-set-layout renderer)
      (destroy-descriptor-set-layout (descriptor-set-layout renderer))
      (setf (descriptor-set-layout renderer) nil))

    (when (uniform-buffer-vs renderer)
      (destroy-buffer (uniform-buffer-vs renderer))
      (setf (uniform-buffer-vs renderer) nil))

    (values))

(defmethod destroy-device-objects ((renderer annotation-renderer))
  (call-next-method)
  (let ((annotation (slot-value (slot-value renderer 'scene) 'annotation)))
    (destroy-buffer (vertex-buffer annotation))
    (destroy-buffer (index-buffer annotation)))
  (values))

(defmethod destroy-device-objects ((scene scene))
  (loop for standin in (slot-value scene 'standins)
     do (destroy-buffer (face-vertex-buffer standin))
       (destroy-buffer (face-index-buffer standin))
       (destroy-buffer (edge-vertex-buffer standin))
       (destroy-buffer (edge-index-buffer standin))))  

(defmethod shutdown-application ((app application))
  (device-wait-idle (default-logical-device app))
  (destroy-device-objects app)
  (destroy-device-objects (annotation-renderer app))
  (destroy-device-objects (edge-renderer app))
  (destroy-device-objects (face-renderer app))
  (imgui-shutdown (imgui-module app))
  (call-next-method))

(defun reset-camera (camera)
  (with-slots (type
	       aim-point
	       azimuth
	       elevation
	       distance
	       pan-x pan-y
	       fov
	       hither
	       yon
	       zoom) camera
      (setf type :perspective
	    aim-point (vec3 0 0 30)
	    azimuth -45
	    elevation 25
	    distance 300
	    pan-x 0
	    pan-y 0
	    fov 45
	    hither 10000
	    yon 0.1
	    zoom 1.0f0)
      (update-view-matrix camera)
      (update-projection-matrix camera)))

(defmethod eye-point ((view-matrix mat4))
  (euclid (mcol (minv view-matrix) 3)))

(defmethod eye-point ((camera camera))
  (let ((view-matrix (slot-value camera 'view-matrix)))
    (eye-point view-matrix)))
;; dunno if these methods really work.
(defmethod right-vector ((view-matrix mat4))
  (v- (vec3 (mcref4 view-matrix 0 0) (mcref4 view-matrix 0 1) (mcref view-matrix 0 2))))

(defmethod right-vector ((camera camera))
  (let ((view-matrix (slot-value camera 'view-matrix)))
    (right-vector view-matrix)))

(defmethod up-vector ((view-matrix mat4))
  (vec3 (mcref4 view-matrix 1 0) (mcref4 view-matrix 1 1) (mcref4 view-matrix 1 2)))

(defmethod up-vector ((camera camera))
  (let ((view-matrix (slot-value camera 'view-matrix)))
    (up-vector view-matrix)))

(defmethod forward-vector ((view-matrix mat4))
  (v- (vec3 (mcref4 view-matrix 2 0) (mcref4 view-matrix 2 1) (mcref view-matrix 2 2))))

(defmethod forward-vector ((camera camera))
  (let ((view-matrix (slot-value camera 'view-matrix)))
    (forward-vector view-matrix)))

(defun perspective-p (camera)
  (eq (slot-value camera 'type) :perspective))

(defun ortho-p (camera)
  (eq (slot-value camera 'type) :ortho))

(defun update-view-matrix (camera)
  (with-slots (elevation
	       azimuth
	       distance
	       aim-point
	       view-matrix
	       pan-x
	       pan-y) camera
    (setf view-matrix
	  (let ((m (meye 4)))

	    (nmtranslate m (vec3 pan-x pan-y (- distance)))
	    (nmrotate m (vec3 1.0f0 0.0f0 0.0f0) (* pi (/ elevation 180.0f0))) ;; elevation
	    (nmrotate m (vec3 0.0f0 1.0f0 0.0f0) (* pi (/ azimuth 180.0f0)));;azimuth
	    
	    
	    ;; z->y, x->z
	    (nmrotate m (vec3 0.0f0 1.0f0 0.0f0) (* pi (/ 270 180.0f0)))
	    (nmrotate m (vec3 1.0f0 0.0f0 0.0f0) (* pi (/ 270 180.0f0)))
	    
	    (nmtranslate m (v- aim-point))
	    m))))

(defun update-projection-matrix (camera)
  (with-slots (window projection-matrix annotation-projection-matrix) camera
    (multiple-value-bind (width height) (get-framebuffer-size window)
      (let ((aspect (/ width height)))
	(ecase (slot-value camera 'type)
	  (:ortho (with-slots (zoom pan-x pan-y) camera
		    (let ((100/zoom (/ 100 zoom)))
		    
		      (setf projection-matrix
			    (mortho (+ (* (- 100/zoom) aspect) pan-x) (+ (* 100/zoom aspect) pan-x)
				    (+ (- 100/zoom) pan-y) (+ 100/zoom pan-y)
				    -10000.0 10000.0)
			    annotation-projection-matrix projection-matrix)
		      #+NIL
		      (setf annotation-projection-matrix
			    (mortho (+ (* (- 100) aspect) pan-x) (+ (* 100 aspect) pan-x)
				    (+ (- 100) pan-y) (+ 100 pan-y)
				    -10000.0 10000.0)))))
	  (:perspective (with-slots (fov yon hither) camera
			  (setf projection-matrix (mperspective fov aspect yon hither)
				annotation-projection-matrix projection-matrix))))))))


(defun cartesian-to-azimuth-elevation-distance (coordinate)
  (let* ((distance (sqrt (+ (expt (vx coordinate) 2) (expt (vy coordinate) 2) (expt (vz coordinate) 2))))
	 (azimuth (- (coerce (/ (* 180.0f0 (atan (vy coordinate) (vx coordinate))) pi) 'single-float)))
	 (elevation (coerce (/ (* 180.0f0 (asin (/ (vz coordinate) distance))) pi) 'single-float)))
    (values azimuth elevation distance)))

(defun maybe-move-camera (camera)
  (with-slots (window) camera
    (when (elt (camera-mode (slot-value camera 'scene)) 2)
      (destructuring-bind (start-mouse-x start-mouse-y start-elevation start-azimuth)
	  (elt (camera-mode (slot-value camera 'scene)) 2)
	(let ((shift
	       (or (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_LEFT_SHIFT) glfw:GLFW_PRESS)
		   (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_RIGHT_SHIFT) glfw:GLFW_PRESS)))
	      (control
	       (or (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_LEFT_CONTROL) glfw:GLFW_PRESS)
		   (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_RIGHT_CONTROL) glfw:GLFW_PRESS)))
	      (meta
	       (or (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_LEFT_ALT) glfw:GLFW_PRESS)
		   (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_RIGHT_ALT) glfw:GLFW_PRESS)))
	      (super
	       (or (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_LEFT_SUPER) glfw:GLFW_PRESS)
		   (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_RIGHT_SUPER) glfw:GLFW_PRESS)))
	      (left-arrow (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_LEFT) glfw:GLFW_PRESS))
	      (right-arrow (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_RIGHT) glfw:GLFW_PRESS))
	      (up-arrow (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_UP) glfw:GLFW_PRESS))
	      (down-arrow (eq (glfw:glfwGetKey (h window) glfw:GLFW_KEY_DOWN) glfw:GLFW_PRESS)))
	  (declare (ignore shift meta super))
	  (multiple-value-bind (mouse-x mouse-y) (get-cursor-pos window)
	    (multiple-value-bind (width height) (get-framebuffer-size window)
	      (let* ((delta-ndc (vec2 (/ (* 2.0f0 (- mouse-x start-mouse-x)) width)
				      (/ (* 2.0f0 (- mouse-y start-mouse-y)) height))))
		
		(with-slots (aim-point azimuth elevation distance pan-x pan-y) camera

		  (let* ((pan-speed 25)
			 (s (* (slot-value camera 'distance) 1/20 (- 101 pan-speed))))
		    
		    (when control
		      (setf pan-x (* s (vx delta-ndc))
			    pan-y (* s (- (vy delta-ndc)))))

		    (unless control
		      (cond (left-arrow (setf pan-x (- pan-x (* s 0.005))))
			    (right-arrow  (setf pan-x (+ pan-x (* s 0.005))))
			    (up-arrow  (setf pan-y (+ pan-y (* s 0.005))))
			    (down-arrow  (setf pan-y (- pan-y (* s 0.005)))))
		      
		      (setf elevation (+ start-elevation (* 180.0f0 (vy delta-ndc)))
			    azimuth (+ start-azimuth (* 180.0f0 (vx delta-ndc))))))))))
	    
	  (update-view-matrix camera)
	    

	  (ig:igText
	   (format nil "elevation: ~S, azimiuth: ~s, distance: ~S"
		   (slot-value camera 'elevation)
		   (slot-value camera 'azimuth)
		   (slot-value camera 'distance)))
	  camera)))))

(defun imgui-scroll-event (window yoffset)
  (let* ((imgui (imgui-module (application window)))
	 (3d-demo-module (face-renderer (application window)))
	 (camera (slot-value (slot-value 3d-demo-module 'scene) 'camera)))

    (when (perspective-p camera)
      (with-slots (distance) camera
	;;(ig:igText (format nil "mouse-wheel: ~S, distance: ~S" (mouse-wheel imgui) distance))
	(let* ((zoom-percent (/ yoffset 10))
	       (delta (* (max (abs distance) 0.2) zoom-percent)))
	  
	  (setf distance (min 1000000.0f0 ;; keep from divide by zero error
				   ;; in compute-picking-ray
				   (max 0.001f0
					(+ distance delta)))))
	#+NIL
	(when (< distance (slot-value (slot-value camera 'yon) camera 'yon))
	  (setf (slot-value camera 'yon) distance))
	#+NIL
	(when (> distance (slot-value camera 'hither))
	  (setf (slot-value camera 'hither) distance))
	(format *debug-io* "~&mouse-wheel: ~S, distance: ~S" (mouse-wheel imgui) distance)
	(finish-output *debug-io*)))

    
    
    (when (ortho-p camera)
      (with-slots (distance zoom) camera
	(let* ((zoom-percent (/ yoffset 10))
	       (delta (* (max (abs distance) 0.2) zoom-percent)))
	  
	  (setf distance (min 1000000.0f0 ;; keep from divide by zero error
			      ;; in compute-picking-ray
			      (max 0.001f0
				   (+ distance delta))))
	  (setf zoom (max 0.1 (+ zoom zoom-percent)))
	  (format *debug-io* "~&mouse-wheel: ~S, zoom: ~S" yoffset zoom))))

    (update-view-matrix camera)
    (update-projection-matrix camera)
    (setf (mouse-wheel imgui) (coerce (+ (mouse-wheel imgui) yoffset) 'single-float))))




(defmethod initialize-instance :after ((app application) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (let* ((scene (make-instance 'scene :app app)))
    (setf (slot-value app 'scene) scene)
    (let* ((device (default-logical-device app))
	   (main-window (main-window app))
	   (allocator (allocator app))
	   (render-pass (render-pass main-window))
	   (pipeline-cache (pipeline-cache app))
	   (descriptor-pool (first (descriptor-pools device)))
	   ;; todo: find appropriate descriptor pool
	   (swapchain (swapchain main-window)))
      ;; this is ugly
      (let ((imgui (make-instance 'imgui)))
	(imgui-init imgui (main-window app)
		    :allocator allocator
		    :device device
		    :render-pass render-pass
		    :pipeline-cache pipeline-cache
		    :descriptor-pool descriptor-pool
		    :frame-count (number-of-images swapchain))
      
	(let* ((standard-renderer-initargs
		(list :app app
		      :scene scene
		      :allocator allocator
		      :device device
		      :render-pass render-pass
		      :window main-window
		      :pipeline-cache pipeline-cache
		      :descriptor-pool descriptor-pool))
	       (face-renderer (apply #'make-instance 'face-renderer
				     standard-renderer-initargs))
	       (edge-renderer (apply #'make-instance 'edge-renderer
				     standard-renderer-initargs))
	       (annotation-renderer (apply #'make-instance 'annotation-renderer
					   :vertex-data *axes-coord-data*
					   :vertex-data-size *axes-coord-data-size*
					   :index-data *axes-index-data*
					   :index-data-size *axes-index-data-size*
					   :index-count (length *axes-indices*)
					   standard-renderer-initargs))
	       (cls-renderer (apply #'make-instance 'colored-line-strip-renderer
				    :frame-count (number-of-images swapchain) standard-renderer-initargs))
	       (cpv-renderer (apply #'make-instance 'colored-point-vertex-renderer
				    :frame-count (number-of-images swapchain) standard-renderer-initargs)))

	  ;; if we pass frame-count directly to the render function we can then move these two renderers to the initform of application. todo.
	
	  (setf (imgui-module app) imgui)
	  (setf (face-renderer app) face-renderer)
	  (setf (edge-renderer app) edge-renderer)
	  (setf (annotation-renderer app) annotation-renderer)
	  (setf (colored-line-strip-renderer app) cls-renderer)
	  (setf (colored-point-vertex-renderer app) cpv-renderer)
	  ))))
  (values))
      
	
      
(defmethod initialize-instance :after ((instance scene) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value instance 'camera) (make-instance 'camera :scene instance :window (main-window (slot-value instance 'app))))
  (values))

(defconstant +infinity+ most-positive-single-float)
(defconstant +epsilon+ 1.0e-06)

(defclass bounding-volume ()
  ())

(defstruct bounding-box
  (min)
  (max))

(defstruct bounding-sphere
  (center)
  (radius))

(defclass transform ()
  ((matrix :initform :identity :reader transform-matrix)
   (inverse :initform :identity :reader transform-inverse)))

(defstruct ray
  (origin)
  (direction))

(defun make-adjustable-array ()
  (make-array 8192 :adjustable t :fill-pointer 0))

(deftype plane-equation () `vec4)

(defstruct plane
  (equation))

(defun plane-normal (plane)
  (let ((equation (plane-equation plane)))
    (vec3 (vx equation) (vy equation) (vz equation))))

(defun plane-point (plane)
  (let ((equation (plane-equation plane)))
    (let ((a^2+b^2+c^2 (+ (expt (vx equation) 2)
			  (expt (vy equation) 2)
			  (expt (vz equation) 2))))
      (vec3 (/ (* (vx equation) (vw equation)) a^2+b^2+c^2)
	    (/ (* (vy equation) (vw equation)) a^2+b^2+c^2)
	    (/ (* (vz equation) (vw equation)) a^2+b^2+c^2)))))

(defmethod distance ((point vec3) (plane plane))
  (distance-point-to-plane plane point))

(defun distance-point-to-plane (plane point)
  (let ((equation (plane-equation plane)))
    (/ (+ (* (vx equation) (vx point))
	  (* (vy equation) (vy point))
	  (* (vz equation) (vz point))
	  (vw equation))
       (sqrt (+ (expt (vx equation) 2)
		(expt (vy equation) 2)
		(expt (vz equation) 2))))))


(defstruct frustum
  (near-plane)
  (far-plane)
  (left-plane)
  (right-plane)
  (bottom-plane)
  (top-plane))

(defun zero-vector ()
  (vec3 0.0f0 0.0f0 0.0f0))

(defmethod zero-vector-p ((v vec3))
  (and (= 0.0f0 (vx v)) (= 0.0f0 (vy v)) (= 0.0f0 (vz v))))

(defmethod expand-matrix ((matrix (eql :identity)))
  (meye 4))

(defmethod expand-matrix ((matrix mat4))
  matrix)

(defparameter +bb-min+
  (make-bounding-box
   :max (vec3 +infinity+ +infinity+ +infinity+)
   :min (vec3 (- +infinity+) (- +infinity+) (- +infinity+))))

(defun bounding-box (expand &rest points)
  (unless expand (setq expand 0.0f0))
  (if (null points)
      (error "No points specified for bounding-box.")
      (let* ((1st-point (car points))
	     (x (vx 1st-point))
	     (y (vy 1st-point))
	     (z (vz 1st-point)))
	(multiple-value-bind (min max) (bounding-box-1 points x x y y z z)
	  (make-bounding-box
	   :min (vec3 (- (vx min) expand) (- (vy min) expand) (- (vz min) expand))
	   :max (vec3 (+ (vx max) expand) (+ (vy max) expand) (+ (vz max) expand)))))))
	   
(defun bounding-box-1 (points &optional (x0 (vx (car points))) (x1 (vx (car points))) (y0 (vy (car points))) (y1 (vy (car points))) (z0 (vz (car points))) (z1 (vz (car points))))
  (if (null points)
      (values (vec3 x0 y0 z0) (vec3 x1 y1 z1))
      (cond
	((< (vx (car points)) x0) (bounding-box-1 points (vx (car points)) x1 y0 y1 z0 z1))
	((> (vx (car points)) x1) (bounding-box-1 points x0 (vx (car points)) y0 y1 z0 z1))
	((< (vy (car points)) y0) (bounding-box-1 points x0 x1 (vy (car points)) y1 z0 z1))
	((> (vy (car points)) y1) (bounding-box-1 points x0 x1 y0 (vy (car points)) z0 z1))
	((< (vz (car points)) z0) (bounding-box-1 points x0 x1 y0 y1 (vz (car points)) z1))
	((> (vz (car points)) z1) (bounding-box-1 points x0 x1 y0 y1 z0 (vz (car points))))
	(t (bounding-box-1 (cdr points) x0 x1 y0 y1 z0 z1)))))

(defun frustum-from-matrix (pvmat)
  (make-frustum
   :left-plane (let ((a (+ (mcref4 pvmat 0 3) (mcref pvmat 0 0)))
		     (b (+ (mcref4 pvmat 1 3) (mcref pvmat 1 0)))
		     (c (+ (mcref4 pvmat 2 3) (mcref pvmat 2 0)))
		     (d (+ (mcref4 pvmat 3 3) (mcref pvmat 3 0))))
		 (make-plane :equation (vec4 a b c d)))
   :right-plane (let ((a (- (mcref4 pvmat 0 3) (mcref pvmat 0 0)))
		      (b (- (mcref4 pvmat 1 3) (mcref pvmat 1 0)))
		      (c (- (mcref4 pvmat 2 3) (mcref pvmat 2 0)))
		      (d (- (mcref4 pvmat 3 3) (mcref pvmat 3 0))))
		  (make-plane :equation (vec4 a b c d)))
   :bottom-plane (make-plane :equation
			     (vec4 (+ (mcref4 pvmat 0 3) (mcref pvmat 0 1))
				   (+ (mcref4 pvmat 1 3) (mcref pvmat 1 1))
				   (+ (mcref4 pvmat 2 3) (mcref pvmat 2 1))
				   (+ (mcref4 pvmat 3 3) (mcref pvmat 3 1))))
   :top-plane (make-plane :equation
			  (vec4 (- (mcref4 pvmat 0 3) (mcref pvmat 0 1))
				(- (mcref4 pvmat 1 3) (mcref pvmat 1 1))
				(- (mcref4 pvmat 2 3) (mcref pvmat 2 1))
				(- (mcref4 pvmat 3 3) (mcref pvmat 3 1))))
   :near-plane (make-plane :equation (vec4 (+ (mcref4 pvmat 0 3) (mcref4 pvmat 0 2))
					   (+ (mcref4 pvmat 1 3) (mcref pvmat 1 2))
					   (+ (mcref4 pvmat 2 3) (mcref pvmat 2 2))
					   (+ (mcref4 pvmat 3 3) (mcref pvmat 3 2))))
   :far-plane (make-plane :equation (vec4 (- (mcref4 pvmat 0 3) (mcref4 pvmat 0 2))
					  (- (mcref4 pvmat 1 3) (mcref pvmat 1 2))
					  (- (mcref4 pvmat 2 3) (mcref pvmat 2 2))
					  (- (mcref4 pvmat 3 3) (mcref pvmat 3 2))))))



#+NIL
(defun frustum-from-ray (scene ray tolerance)
  (let* ((origin (ray-origin ray))
	 (pmat (m* (meye 4) ;; placeholder for view-matrix
		   (mortho (- origin tolerance)
			   (+ origin tolerance)
			   (- origin tolerance)
			   (+ origin tolerance)
			   -100 100))))
    (frustum-from-matrix pmat)))



(defun transform-bounding-box (box m)
  (multiple-value-bind (min max)
      (let* ((corners (multiple-value-list (bounding-box-corners box)))
	     (corners-xformed
	      (mapcar #'(lambda (vec3)
			  (euclid (m* m (vec4 (vx vec3) (vy vec3) (vz vec3) 1.0f0))))
		      corners)))
	(bounding-box-1 corners-xformed))
    (make-bounding-box :min min :max max)))

(defun unhighlight-toplevel-standin (queue standin &optional (toplevel-standin standin))
  (loop for child in (slot-value standin 'child-standins)
     do (when (highlighted-p child)
	  (unhighlight-standin queue child toplevel-standin))
       (unhighlight-toplevel-standin queue child toplevel-standin)))

(defun unhighlight-standin (queue standin toplevel-standin)
  (when (typep standin 'face-standin)
    (let ((nvtx (slot-value standin 'num-verts))
	  (offset (draw-indexed-cmd-vertex-offset (slot-value standin 'draw-cmd)))
	  (vertex-array (face-vertex-array toplevel-standin)))
      (loop for v from offset repeat nvtx
	 do (setf (aref vertex-array (+ (* 6 v) 3)) 1.0f0)
	   (setf (aref vertex-array (+ (* 6 v) 4)) 0.0f0)
	   (setf (aref vertex-array (+ (* 6 v) 5)) 0.0f0)
	 finally (when (face-vertex-buffer toplevel-standin)
		   (queue-wait-idle queue)
		   (destroy-buffer (face-vertex-buffer toplevel-standin)))
	   (setf (face-vertex-buffer toplevel-standin) nil))))
  (setf (highlighted-p standin) nil)
  t)

(defun highlight-standin (queue standin toplevel-standin)
  (let ((nvtx (slot-value standin 'num-verts))
	(offset (draw-indexed-cmd-vertex-offset (slot-value standin 'draw-cmd)))
	(vertex-array (face-vertex-array toplevel-standin)))
    (loop for v from offset repeat nvtx
       do (setf (aref vertex-array (+ (* 6 v) 3)) 0.0f0)
	 (setf (aref vertex-array (+ (* 6 v) 4)) 0.0f0)
	 (setf (aref vertex-array (+ (* 6 v) 5)) 1.0f0)
       finally (when (face-vertex-buffer toplevel-standin)
		 (queue-wait-idle queue)
		 (destroy-buffer (face-vertex-buffer toplevel-standin)))
	 (setf (face-vertex-buffer toplevel-standin) nil)
	 (setf (highlighted-p standin) t)))
  t)
  

(defun ray-intersects-bvh (ray standin matrix &optional (leaf-type :face))
  (let ((bbox (slot-value standin 'bounding-box)))
    (when (ray-intersects-box-p ray (transform-bounding-box bbox matrix))
      (flet ((recur ()
	       (apply #'append
		      (mapcar #'(lambda (child) (ray-intersects-bvh ray child matrix leaf-type))
			      (slot-value standin 'child-standins)))))
	(ecase leaf-type
	  (:face (typecase standin
		   (shell-standin
		    (remove-if #'null
			       (mapcar #'(lambda (face-standin) (when (ray-intersects-box-p ray (transform-bounding-box (slot-value face-standin 'bounding-box) matrix))
								   face-standin))
				       (slot-value standin 'child-standins))))
		   (t (recur))))
	  (:edge (typecase standin
		   (wire-standin
		    (remove-if #'null
			       (mapcar #'(lambda (edge-standin) (when (ray-intersects-box-p ray (transform-bounding-box (slot-value edge-standin 'bounding-box) matrix))
								   edge-standin))
				       (slot-value standin 'child-standins))))
		   (t (recur)))))))))


(defun ray-intersects-box-p (ray bbox)
  (let* ((min (bounding-box-min bbox))
	 (max (bounding-box-max bbox))
	 (tmin)
	 (tmax)
	 (tymin)
	 (tymax)
	 (tzmin)
	 (tzmax)
	 (dir (ray-direction ray))
	 (origin (ray-origin ray)))

    (if (= (vx dir) 0.0f0)
	(setq tmin most-negative-single-float
	      tmax most-positive-single-float)
	(if (> (vx dir) 0.0f0)
	    (setq tmin (/ (- (vx min) (vx origin)) (vx dir))
		  tmax (/ (- (vx max) (vx origin)) (vx dir)))
	    (setq tmin (/ (- (vx max) (vx origin)) (vx dir))
		  tmax (/ (- (vx min) (vx origin)) (vx dir)))))

    (if (= (vy dir) 0.0f0)
	(setq tymin most-negative-single-float
	      tymax most-positive-single-float)
	(if (> (vy dir) 0.0f0)
	    (setq tymin (/ (- (vy min) (vy origin)) (vy dir))
		  tymax (/ (- (vy max) (vy origin)) (vy dir)))
	    (setq tymin (/ (- (vy max) (vy origin)) (vy dir))
		  tymax (/ (- (vy min) (vy origin)) (vy dir)))))

    (when (or (> tmin tymax) (> tymin tmax))
      (return-from ray-intersects-box-p nil))

    (when (> tymin tmin)
      (setq tmin tymin))

    (when (> tymax tmax)
      (setq tmax tymax))

    (if (= (vz dir) 0.0f0)
	(setq tzmin most-negative-single-float
	      tzmax most-positive-single-float)
	(if (> (vz dir) 0.0f0)
	    (setq tzmin (/ (- (vz min) (vz origin)) (vz dir))
		  tzmax (/ (- (vz max) (vz origin)) (vz dir)))
	    (setq tzmin (/ (- (vz max) (vz origin)) (vz dir))
		  tzmax (/ (- (vz min) (vz origin)) (vz dir)))))

    (when (or (> tmin tzmax) (> tzmin tmax))
      (return-from ray-intersects-box-p nil))

    (when (> tzmin tmin)
      (setq tmin tzmin))

    (when (> tzmax tmax)
      (setq tmax tzmax))

    (return-from ray-intersects-box-p (values t tmin tmax))))
    
    

(defun bounding-box-corners (box)
  (let ((min (bounding-box-min box))
	(max (bounding-box-max box)))
    (values (vec3 (vx min) (vy min) (vz min))
	    (vec3 (vx min) (vy min) (vz max))
	    (vec3 (vx min) (vy max) (vz min))
	    (vec3 (vx min) (vy max) (vz max))
	    (vec3 (vx max) (vy min) (vz min))
	    (vec3 (vx max) (vy min) (vz max))
	    (vec3 (vx max) (vy max) (vz min))
	    (vec3 (vx max) (vy max) (vz max)))))

(defun frustum-intersects-box-p (frustum box)
  (let ((corners (multiple-value-list
		  (bounding-box-corners box)))
	(result :inside))
    (labels (
	     (test-plane (plane)
	       (let ((out 0)
		     (in 0))
		 (loop for vtx in corners while (or (zerop in) (zerop out))
		    do (if (< (distance vtx plane) 0)
			   (incf out)
			   (incf in)))
		 (if (zerop in)
		     (return-from frustum-intersects-box-p nil)
		     (unless (zerop out)
		       (setq result :intersect))))))
      (test-plane (frustum-left-plane frustum))
      (test-plane (frustum-right-plane frustum))
      (test-plane (frustum-near-plane frustum))
      (test-plane (frustum-far-plane frustum))
      (test-plane (frustum-top-plane frustum))
      (test-plane (frustum-bottom-plane frustum))
      (return-from frustum-intersects-box-p result))))

(defmethod frustum-contains-point-p ((frustum frustum) (point gp:pnt))
  (frustum-contains-point-p frustum (vec3 (oc:x point) (oc:y point) (oc:z point))))

(defmethod frustum-contains-point-p ((frustum frustum) (point vec3))
  (flet ((test-plane (plane)
	   (< (distance point plane) 0)))
    (and (test-plane (frustum-left-plane frustum))
	 (test-plane (frustum-right-plane frustum))
	 (test-plane (frustum-near-plane frustum))
	 (test-plane (frustum-far-plane frustum))
	 (test-plane (frustum-top-plane frustum))
	 (test-plane (frustum-bottom-plane frustum)))))

(defmethod ray-intersects-triangle-p ((ray ray) (p1 gp:pnt) (p2 gp:pnt) (p3 gp:pnt))
  (ray-intersects-triangle-p ray
			     (vec3 (oc:x p1) (oc:y p1) (oc:z p1))
			     (vec3 (oc:x p2) (oc:y p2) (oc:z p2))
			     (vec3 (oc:x p3) (oc:y p3) (oc:z p3))))

;; Möller–Trumbore intersection algorithm
(defmethod ray-intersects-triangle-p ((ray ray) (p1 vec3) (p2 vec3) (p3 vec3))
  (block intersects
    (let* ((origin (ray-origin ray))
	   (direction (ray-direction ray))
	   (edge1 (v- p2 p1))
	   (edge2 (v- p3 p1))
	   (h (vc direction edge2))
	   (a (v. edge1 h)))
      
      (when (< (abs a) +epsilon+)
	(return-from intersects nil)) ;; ray is parallel to triangle

      (let* ((f (/ 1.0f0 a))
	     (s (v- origin p1))
	     (u (* f (v. s h))))
	
	(when (or (< u 0.0f0) (> u 1.0f0))
	  (return-from intersects nil))

	(let* ((q (vc s edge1))
	       (v (* f (v. direction q))))
	  
	  (when (or (< v 0.0f0) (> (+ u v) 1.0f0))
	    (return-from intersects nil))

	  (let ((tt (* f (v. edge2 q))))
	    (when (> tt +epsilon+)
	      (return-from intersects tt))))))
    (return-from intersects nil)))

(defun ray-intersects-face-p (ray face-standin model-matrix)
  (let* ((vertices (slot-value face-standin 'poly-vertices))
	 (indices (slot-value face-standin 'poly-indices)))
    (loop for i from 0 below (length indices) by 3
       do (let ((p1 (aref vertices (aref indices i)))
		(p2 (aref vertices (aref indices (1+ i))))
		(p3 (aref vertices (aref indices (+ i 2)))))
	    (let ((point1 (euclid (m* model-matrix (vec4 (vx p1) (vy p1) (vz p1) 1.0f0))))
		  (point2 (euclid (m* model-matrix (vec4 (vx p2) (vy p2) (vz p2) 1.0f0))))
		  (point3 (euclid (m* model-matrix (vec4 (vx p3) (vy p3) (vz p3) 1.0f0)))))
	      (let ((tmin (ray-intersects-triangle-p ray point1 point2 point3)))
		(when tmin
		  (return-from ray-intersects-face-p tmin)))))
       finally (return-from ray-intersects-face-p nil))))

(defun test-transform (matrix vertex-array)
  (let* ((count (length vertex-array))
	 (output-array (make-array count :element-type 'single-float)))
    (loop for i from 0 below count by 3
       do (let ((x (aref vertex-array i))
		(y (aref vertex-array (1+ i)))
		(z (aref vertex-array (+ i 2))))
	    (let ((transformed-point (euclid (m* matrix (vec4 x y z 1.0f0)))))
	      (setf (aref output-array i) (vx transformed-point)
		    (aref output-array (1+ i)) (vy transformed-point)
		    (aref output-array (+ i 2)) (vz transformed-point))))
      finally (return-from test-transform output-array))))
  

(defclass toplevel-standin ()
  ((child-standins :initform nil) ;; for the toplevel shape standin)
   (bounding-box)
   
   (poly-nodes :reader polyhedron-nodes :initform (make-adjustable-array))
   (face-indices :reader face-indices :initform (make-adjustable-array))
   (edge-indices :reader edge-indices :initform (make-adjustable-array))
   (face-commands :reader face-commands :initform (make-adjustable-array))
   (edge-commands :reader edge-commands :initform (make-adjustable-array))
		 
   (face-vertex-array :accessor face-vertex-array)
   (face-index-array :accessor face-index-array)
   (edge-vertex-array :accessor edge-vertex-array)
   (edge-index-array :accessor edge-index-array)
   (face-vertex-buffer :accessor face-vertex-buffer :initform nil)
   (face-index-buffer :accessor face-index-buffer :initform nil)
   (edge-vertex-buffer :accessor edge-vertex-buffer :initform nil)
   (edge-index-buffer :accessor edge-index-buffer :initform nil)))

(defun get-face-standins (arg)
  (when arg
    (if (listp arg)
	(append (get-face-standins (car arg))
		(get-face-standins (cdr arg)))
	(if (typep arg 'shell-standin)
	    (slot-value arg 'child-standins) ;; these are the face standins.
	    (let ((children (slot-value arg 'child-standins)))
	      (append (get-face-standins (car children))
		      (get-face-standins (cdr children))))))))

(defclass shape-standin-mixin ()
  ((topods-shape :initarg :source-shape)
   (child-standins :initform nil)
   (toplevel)
   (bounding-box)
   (highlighted-p :accessor highlighted-p :initform nil)))

(defclass vertices-mixin ()
  ((poly-vertices :initform (make-adjustable-array))
   (vertex-normals)))

(defclass indices-mixin ()
  ((poly-indices :initform (make-adjustable-array))
   (draw-cmd)))

(defclass compsolid-standin (shape-standin-mixin) ())

(defclass compound-standin (shape-standin-mixin) ())

(defclass solid-standin (shape-standin-mixin) ())

(defclass shell-standin (shape-standin-mixin) ())

(defclass face-standin (vertices-mixin indices-mixin shape-standin-mixin)
  ((vertex-offset :accessor vertex-offset)
   (num-verts)
   (random-id :initform (random 100000))
   (triangulation)))

(defclass wire-standin (shape-standin-mixin) ())

(defclass edge-standin (indices-mixin shape-standin-mixin)
  ((num-verts)
   (my-face :reader my-face :initarg :corresponding-face)))  

(defclass vertex-standin (shape-standin-mixin) ())

(defun pnt->vec3 (pnt)
  (vec3 (coerce (oc:x pnt) 'single-float)
	(coerce (oc:y pnt) 'single-float)
	(coerce (oc:z pnt) 'single-float)))

(defun create-standin (topods-shape)
  (make-instance 'oc::BRep-Mesh-Incremental-Mesh :S topods-shape :D 7.0d-3 :Relative t)
  (let ((toplevel (make-instance 'toplevel-standin)))
    (setf (slot-value toplevel 'child-standins)
	  (list (create-standin-1 topods-shape nil nil toplevel)))

    (setf (slot-value toplevel 'bounding-box)
	  (when (slot-value toplevel 'child-standins)
	    (apply #'bounding-box nil (append (mapcar #'(lambda (child) (bounding-box-min (slot-value child 'bounding-box))) (slot-value toplevel 'child-standins))
					      (mapcar #'(lambda (child) (bounding-box-max (slot-value child 'bounding-box))) (slot-value toplevel 'child-standins))))))
    
    (populate-vertex-arrays toplevel)
    toplevel))



(defmethod standin-class-for-topods-shape ((shape oc::TopoDS-CompSolid))
  'compsolid-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS-Compound))
  'compound-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS-Solid))
  'solid-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS-Shell))
  'shell-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS-Wire))
  'wire-standin)


(defmethod create-standin-1 ((shape oc::TopoDS-Shape) parent-standin grandparent-standin toplevel-standin)
  (declare (ignore grandparent-standin))
  (let ((standin (make-instance (standin-class-for-topods-shape shape) :source-shape shape))
	(iterator (make-instance 'oc::TopoDS-Iterator :S shape)))
									 
    (setf (slot-value standin 'toplevel) toplevel-standin)
    (setf (slot-value standin 'child-standins)
	  (remove-if
	   #'null
	   (loop while (oc::more-p iterator)
	      collect (prog1
			  (create-standin-1 (oc::value iterator) standin parent-standin
					    toplevel-standin)
			(oc::next iterator)))))
    
    (setf (slot-value standin 'bounding-box)
	  (when (slot-value standin 'child-standins)
	    (apply #'bounding-box nil (append (mapcar #'(lambda (child) (bounding-box-min (slot-value child 'bounding-box))) (slot-value standin 'child-standins))
					      (mapcar #'(lambda (child) (bounding-box-max (slot-value child 'bounding-box))) (slot-value standin 'child-standins))))))
    standin))

(defmethod create-standin-1 ((vertex oc::TopoDS-Vertex) parent-standin
			     grandparent-standin toplevel-standin)
  (declare (ignore grandparent-standin))
  (let ((standin (make-instance 'vertex-standin :source-shape vertex)))
    (setf (slot-value standin 'toplevel) toplevel-standin)
    (setf (slot-value standin 'bounding-box)
	  (bounding-box nil (pnt->vec3 (gp::make-pnt :ptr (oc::_wrap_BRep_Tool_Pnt (oc::ff-pointer vertex))))))
    standin))

(defmethod create-standin-1 ((face oc::TopoDS-Face) parent-standin
			     grandparent-standin toplevel-standin)
  (declare (ignore grandparent-standin))
  (let* ((standin (make-instance 'face-standin :source-shape face))
	 (triangulation (allocate-instance (find-class 'oc::Poly-Triangulation)))
	 (commands (face-commands toplevel-standin))
	 (vertex-array (polyhedron-nodes toplevel-standin))
	 (index-array (face-indices toplevel-standin))
	 (first-index (fill-pointer index-array))
	 (vertex-offset (fill-pointer vertex-array)))
    (setf (slot-value standin 'toplevel) toplevel-standin)

    (setf (slot-value standin 'vertex-offset) vertex-offset)
    
    (setf (oc::ff-pointer triangulation)
	  (oc::_wrap_BRep_Tool_Triangulation
	   (oc::ff-pointer face) (oc::_wrap_new_TopLoc_Location__SWIG_0))
	  (slot-value standin 'triangulation) triangulation)
    
    (let* ((vertices (oc::get-nodes triangulation))
	   (vertex-lower (oc::get-lower vertices))
	   (vertex-upper (oc::get-upper vertices))
	   (triangles (oc::get-triangles triangulation))
	   (triangles-lower (oc::get-lower triangles))
	   (triangles-upper (oc::get-upper triangles))
	   (num-verts (1+ (- vertex-upper vertex-lower))))

      (setf (slot-value standin 'num-verts) num-verts)

      (vector-push-extend (setf (slot-value standin 'draw-cmd)
				(make-draw-indexed-cmd :index-count (* 3 (1+ (- triangles-upper triangles-lower)))
						       :first-index first-index
						       :vertex-offset vertex-offset))
			  commands)
	  
      (loop for i from vertex-lower to vertex-upper
	 do (let ((pnt (oc::get-value vertices i)))
	      (vector-push-extend (pnt->vec3 pnt) vertex-array)))

      (setf (slot-value standin 'poly-vertices) (make-array (1+ (- vertex-upper vertex-lower))
							    :displaced-to vertex-array
							    :displaced-index-offset vertex-offset))
	  
      (loop for i from triangles-lower to triangles-upper
	 do (let ((triangle (oc::get-value triangles i)))
	      (vector-push-extend (- (oc::get-value triangle 1) vertex-lower) index-array)
	      (vector-push-extend (- (oc::get-value triangle 2) vertex-lower) index-array)
	      (vector-push-extend (- (oc::get-value triangle 3) vertex-lower) index-array)))

      (setf (slot-value standin 'poly-indices)  (make-array (* 3 (1+ (- triangles-upper triangles-lower)))
							    :displaced-to index-array
							    :displaced-index-offset first-index))

      (setf (slot-value standin 'bounding-box)
	    (apply #'bounding-box nil
		   (loop for i from vertex-offset to (+ vertex-offset (- vertex-upper vertex-lower))
			collect (aref vertex-array i))))

      (setf (slot-value standin 'child-standins)
	    (remove-if #'null
		       (let ((iterator (make-instance 'oc::TopoDS-Iterator :S face)))
			 (loop while (oc::more-p iterator)
			    collect (prog1 (create-standin-1
					    (oc::value iterator) standin parent-standin toplevel-standin)
				      (oc::next iterator)))))))
    standin))

(defmethod on-mouse-button (window button action)
  (print "on mouse button clicked")
  (finish-output)
  (let ((imgui (imgui-module (application window))))
    (when (and (eq action glfw:GLFW_PRESS)
	       (>= button 0)
	       (< button 3))
      (setf (elt (mouse-pressed imgui) button) t)
      (let* ((scene (slot-value (application window) 'scene))
	     (editor (slot-value (application window) 'editor))
	     (camera (slot-value scene 'camera)))

	(cond ((eq (editor-mode editor) :line-zeroth-point)
	       (push (intersect-ray-with-plane
		      (compute-picking-ray window camera (slot-value camera 'view-matrix)
					   (slot-value camera 'projection-matrix) +clip-matrix+)		 
		      (make-plane :equation (vec4 0 0 1 0)))
		     (selection-stack editor))
	       (setf (editor-mode editor) :line-first-point))
	      
	      ((eq (editor-mode editor) :line-first-point)
	       (push (intersect-ray-with-plane
		      (compute-picking-ray window camera (slot-value camera 'view-matrix)
					   (slot-value camera 'projection-matrix) +clip-matrix+)		 
		      (make-plane :equation (vec4 0 0 1 0)))
		     (selection-stack editor))
	       (add-entity editor (make-instance 'line-segment :point1 (first (selection-stack editor))
						 :point2 (second (selection-stack editor))))
	       (setf (selection-stack editor) nil)
	       (setf (editor-mode editor) nil)))

	  
	(setf (elt (camera-mode scene) button)
	      (if (elt (camera-mode scene) button)
		  nil
		  (append (multiple-value-list (get-cursor-pos window))
			  (list (slot-value camera 'elevation)
				(slot-value camera 'azimuth)))))
	(print "on mouse button done")
	(finish-output)))))

(defun populate-vertex-arrays (toplevel)
  (let* ((step (/ (cffi:foreign-type-size '(:struct 3DVertex)) (cffi:foreign-type-size :float)))
	 (vertex-array-float-size (* step (fill-pointer (polyhedron-nodes toplevel))))
	 (face-vertex-array (make-array vertex-array-float-size :element-type 'single-float))
	 (edge-vertex-array (make-array vertex-array-float-size :element-type 'single-float))
	 (face-index-array (make-array (fill-pointer (face-indices toplevel)) :element-type '(unsigned-byte 16)))
	 (edge-index-array (make-array (fill-pointer (edge-indices toplevel)) :element-type '(unsigned-byte 16))))

    (flet ((copy-vertex-arrays ()
	     (loop for i from 0 for point across (polyhedron-nodes toplevel)

		do (let ((float-offset (* i step)))
	    
		     (setf (aref face-vertex-array (+ float-offset 0)) (vx point)
			   (aref face-vertex-array (+ float-offset 1)) (vy point)
			   (aref face-vertex-array (+ float-offset 2)) (vz point)
			   (aref face-vertex-array (+ float-offset 3)) 1.0f0
			   (aref face-vertex-array (+ float-offset 4)) 0.0f0
			   (aref face-vertex-array (+ float-offset 5)) 0.0f0)
	 
		     (setf (aref edge-vertex-array (+ float-offset 0)) (vx point)
			   (aref edge-vertex-array (+ float-offset 1)) (vy point)
			   (aref edge-vertex-array (+ float-offset 2)) (vz point)
			   (aref edge-vertex-array (+ float-offset 3)) 0.0f0
			   (aref edge-vertex-array (+ float-offset 4)) 0.0f0
			   (aref edge-vertex-array (+ float-offset 5)) 0.0f0)))
	     (setf (slot-value toplevel 'face-vertex-array) face-vertex-array
		   (slot-value toplevel 'edge-vertex-array) edge-vertex-array)
	     (values))

	   (copy-index-arrays ()
	     (loop for i from 0 for index across (face-indices toplevel)
		do (setf (aref face-index-array i) index))
	     (loop for i from 0 for index across (edge-indices toplevel)
		do (setf (aref edge-index-array i) index))
	     (setf (slot-value toplevel 'face-index-array) face-index-array
		   (slot-value toplevel 'edge-index-array) edge-index-array)
	     (values)))

      (copy-vertex-arrays)
      (copy-index-arrays)
      (values))))

(defmethod create-standin-1 ((edge oc::TopoDS-Edge) parent-standin
			     (grandparent-standin face-standin) toplevel-standin)
  (let ((standin (make-instance 'edge-standin :source-shape edge
				:corresponding-face grandparent-standin)))
    (setf (slot-value standin 'toplevel) toplevel-standin)
    (let* ((triangulation (slot-value grandparent-standin 'triangulation))
	   (index-array (edge-indices toplevel-standin))
	   (commands (edge-commands toplevel-standin))
	   (first-index (fill-pointer index-array))
	   (L (oc::_wrap_new_TopLoc_Location__SWIG_0)))

      (let* ((poly-polygon-on-triangulation
	      (oc::get-polygon-on-triangulation edge triangulation L))
	     (nodes (oc::get-nodes poly-polygon-on-triangulation))
	     (nodes-lower (oc::get-lower nodes))
	     (nodes-upper (oc::get-upper nodes))
	     (index-count (1+ (- nodes-upper nodes-lower)))
	     (vertex-array (polyhedron-nodes toplevel-standin))
	     (face-vertex-offset (slot-value grandparent-standin 'vertex-offset)))
	
	(vector-push-extend (make-draw-indexed-cmd :index-count index-count
						   :first-index first-index
						   :vertex-offset face-vertex-offset)
			    commands)

	(setf (slot-value standin 'bounding-box)
	      (apply #'bounding-box nil
		     (let ((node))
		       (loop for i from nodes-lower to nodes-upper
			  do (setq node (oc::get-value nodes i))
			    (vector-push-extend (1- node) index-array)
			  collect (aref vertex-array (+ (1- node) face-vertex-offset))))))

	(setf (slot-value standin 'child-standins)
	      (let ((iterator (make-instance 'oc::TopoDS-Iterator :S edge)))
		(loop while (oc::more-p iterator)
		   collect (prog1 (create-standin-1 (oc::value iterator)
						    standin parent-standin toplevel-standin)
			     (oc::next iterator)))))
	    
	standin))))


      
					  
