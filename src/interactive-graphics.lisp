(in-package #:igp)

(defconstant bnd-precision-infinite 1.0d100)
(defconstant -bnd-precision-infinite (- bnd-precision-infinite))

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
   
   (camera-type :initform :lookat)
   (rotation-speed :initform 1.0f0)
   (movement-speed :initform 1.0f0)
   
   (view-frustum)

   
   
   (updated :initform nil)

   (view-matrix)
   (projection-matrix)
   (annotation-projection-matrix)
   ))

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
  (vktk::euclid (mcol (minv view-matrix) 3)))

(defmethod eye-point ((camera camera))
  (let ((view-matrix (slot-value camera 'view-matrix)))
    (eye-point view-matrix)))

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

(defun vktk::imgui-scroll-event (window yoffset)
  (let* ((imgui (vktk::imgui-module (application window)))
	 (3d-demo-module (vktk::face-renderer (application window)))
	 (camera (slot-value (slot-value 3d-demo-module 'vktk::scene) 'igp::camera)))

    (when (perspective-p camera)
      (with-slots (igp::distance) camera
	;;(ig:igText (format nil "mouse-wheel: ~S, distance: ~S" (mouse-wheel imgui) igp::distance))
	(let* ((zoom-percent (/ yoffset 10))
	       (delta (* (max (abs distance) 0.2) zoom-percent)))
	  
	  (setf igp::distance (min 1000000.0f0 ;; keep from divide by zero error
				   ;; in compute-picking-ray
				   (max 0.001f0
					(+ distance delta)))))
	#+NIL
	(when (< distance (slot-value (slot-value camera 'yon) camera 'yon))
	  (setf (slot-value camera 'yon) distance))
	#+NIL
	(when (> distance (slot-value camera 'hither))
	  (setf (slot-value camera 'hither) distance))
	(format *debug-io* "~&mouse-wheel: ~S, distance: ~S" (vktk::mouse-wheel imgui) igp::distance)
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

    (igp::update-view-matrix camera)
    (igp::update-projection-matrix camera)
    (setf (vktk::mouse-wheel imgui) (coerce (+ (vktk::mouse-wheel imgui) yoffset) 'single-float))))

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
   
   (camera)
   (viewport)
   (standins :initform (list (create-standin (oc::oc-body (oc::make-bottle)))))
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

(defmethod initialize-instance :after ((instance scene) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (slot-value instance 'camera) (make-instance 'camera :scene instance :window (vktk::main-window (slot-value instance 'app))))
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

#+OLD
(defclass bounding-sphere (bounding-volume)
  ((c :accessor bounding-sphere-center)
   (r :accessor bounding-sphere-radius)))

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

;; minv
;; mtranslation
;; mscaling
;; mrotation
;; mlookat

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


(defun frustum-from-ray (scene ray tolerance)
  (let* ((origin (ray-origin ray))
	 (pmat (m* (meye 4) ;; placeholder for view-matrix
		   (mortho (- origin tolerance)
			   (+ origin tolerance)
			   (- origin tolerance)
			   (+ origin tolerance)
			   -100 100))))
    (frustum-from-matrix pmat)))


(defmethod compute-selection (scene ray projection-matrix (sensitivity (eql :vertices)) &key (tolerance 1e-3))
  (let ((shapes (top-level-shapes scene))
	(frustum (frustum-from-ray projection-matrix ray tolerance))
	(vertices ()))
    (loop for shape in shapes
       do
	 (let ((explorer (make-instance 'oc::TopoDS_Explorer :S shape :ToFind oc::TopAbs_VERTEX)))
	   (loop while (oc::more-p explorer)
	      do (let* ((vertex (oc::current explorer))
			(pnt (gp::make-pnt :ptr (oc::_wrap_BRep_Tool_Pnt vertex))))
		   (when (frustum-contains-point-p frustum pnt)
		     (push vertex vertices)))
		(oc::next explorer))))
    (first vertices)))
		       
(defmethod compute-selection (scene ray projection-matrix (sensitivity (eql :edges))  &key (tolerance 1e-3)))
(defmethod compute-selection (scene ray projection-matrix (sensitivity (eql :wires))  &key (tolerance 1e-3)))
(defmethod compute-selection (scene ray projection-matrix (sensitivity (eql :faces))  &key (tolerance 1e-3)))
(defmethod compute-selection (scene ray projection-matrix (sensitivity (eql :shells)) &key (tolerance 1e-3)))
(defmethod compute-selection (scene ray projection-matrix (sensitivity (eql :solids)) &key (tolerance 1e-3)))

(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :vertices)) (inclusion (eql :overlap)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :edges)) (inclusion (eql :overlap)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :wires)) (inclusion (eql :overlap)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :faces)) (inclusion (eql :overlap)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :shells)) (inclusion (eql :overlap)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :solids)) (inclusion (eql :overlap)) &key (tolerance 1e-3)))

(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :vertices)) (inclusion (eql :surround)) &key (tolerance 1e-3))
  ;; super slow concept testing version
  (let ((shapes (top-level-shapes scene))
	(vertices ()))
    (loop for shape in shapes
       do
	 (let ((explorer (make-instance 'oc::TopoDS_Explorer :S shape :ToFind oc::TopAbs_VERTEX)))
	   (loop while (oc::more-p explorer)
	      do (let* ((vertex (oc::current explorer))
			(pnt (gp::make-pnt :ptr (oc::_wrap_BRep_Tool_Pnt vertex))))
		   (when (frustum-contains-point-p pick-geometry pnt)
		     (push vertex vertices))
		   (oc::next explorer)))))
    vertices))		

(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :edges)) (inclusion (eql :surround)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :wires)) (inclusion (eql :surround)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :faces)) (inclusion (eql :surround)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :shells)) (inclusion (eql :surround)) &key (tolerance 1e-3)))
(defmethod compute-selection-set (scene (pick-geometry frustum) (sensitivity (eql :solids)) (inclusion (eql :surround)) &key (tolerance 1e-3)))

(defun transform-bounding-box (box m)
  (multiple-value-bind (min max)
      (let* ((corners (multiple-value-list (bounding-box-corners box)))
	     (corners-xformed
	      (mapcar #'(lambda (vec3)
			  (vktk::euclid (m* m (vec4 (vx vec3) (vy vec3) (vz vec3) 1.0f0))))
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
	 finally (when (igp::face-vertex-buffer toplevel-standin)
		   (vk:vkQueueWaitIdle (h queue))
		   (vktk::destroy-buffer (igp::face-vertex-buffer toplevel-standin)))
	   (setf (igp::face-vertex-buffer toplevel-standin) nil))))
  (setf (highlighted-p standin) nil)
  t)

(defun highlight-standin (queue standin toplevel-standin)
  (let ((nvtx (slot-value standin 'num-verts))
	(offset (draw-indexed-cmd-vertex-offset (slot-value standin 'igp::draw-cmd)))
	(vertex-array (igp::face-vertex-array toplevel-standin)))
    (loop for v from offset repeat nvtx
       do (setf (aref vertex-array (+ (* 6 v) 3)) 0.0f0)
	 (setf (aref vertex-array (+ (* 6 v) 4)) 0.0f0)
	 (setf (aref vertex-array (+ (* 6 v) 5)) 1.0f0)
       finally (when (igp::face-vertex-buffer toplevel-standin)
		 (vk:vkQueueWaitIdle (h queue))
		 (vktk::destroy-buffer (igp::face-vertex-buffer toplevel-standin)))
	 (setf (igp::face-vertex-buffer toplevel-standin) nil)
	 (setf (highlighted-p standin) t)))
  t)
  

(defun ray-intersects-bvh (ray standin matrix &optional (leaf-type :face))
  (when (ray-intersects-box-p ray standin matrix)
    (flet ((recur ()
	     (apply #'append
		    (mapcar #'(lambda (child) (ray-intersects-bvh ray child matrix leaf-type))
			    (slot-value standin 'child-standins)))))
      (ecase leaf-type
	(:face (typecase standin
		 (shell-standin
		  (remove-if #'null
			     (mapcar #'(lambda (face-standin) (ray-intersects-box-p ray face-standin matrix))
				     (slot-value standin 'child-standins))))
		 (t (recur))))
	(:edge (typecase standin
		 (wire-standin
		  (remove-if #'null
			     (mapcar #'(lambda (edge-standin) (ray-intersects-box-p ray edge-standin matrix))
				     (slot-value standin 'child-standins))))
		 (t (recur))))))))


(defun ray-intersects-box-p (ray standin &optional (matrix (meye 4)))
  (let* ((box (transform-bounding-box (slot-value standin 'bounding-box) matrix))
	 (min (bounding-box-min box))
	 (max (bounding-box-max box))
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

    (return-from ray-intersects-box-p (values standin tmin tmax))))
    
    

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

(defmethod frustum-contains-point-p ((frustum frustum) (point oc::pnt))
  (frustum-contains-point-p frustum (vec3 (oc::x point) (oc::y point) (oc::z point))))

(defmethod frustum-contains-point-p ((frustum frustum) (point vec3))
  (flet ((test-plane (plane)
	   (< (distance point plane) 0)))
    (and (test-plane (frustum-left-plane frustum))
	 (test-plane (frustum-right-plane frustum))
	 (test-plane (frustum-near-plane frustum))
	 (test-plane (frustum-far-plane frustum))
	 (test-plane (frustum-top-plane frustum))
	 (test-plane (frustum-bottom-plane frustum)))))

(defmethod ray-intersects-triangle-p ((ray ray) (p1 oc::pnt) (p2 oc::pnt) (p3 oc::pnt))
  (ray-intersects-triangle-p ray
			     (vec3 (x p1) (y p1) (z p1))
			     (vec3 (x p2) (y p2) (z p2))
			     (vec3 (x p3) (y p3) (z p3))))

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
	    (let ((point1 (vktk::euclid (m* model-matrix (vec4 (vx p1) (vy p1) (vz p1) 1.0f0))))
		  (point2 (vktk::euclid (m* model-matrix (vec4 (vx p2) (vy p2) (vz p2) 1.0f0))))
		  (point3 (vktk::euclid (m* model-matrix (vec4 (vx p3) (vy p3) (vz p3) 1.0f0)))))
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
	    (let ((transformed-point (vktk::euclid (m* matrix (vec4 x y z 1.0f0)))))
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
  (vec3 (coerce (gp::x pnt) 'single-float)
	(coerce (gp::y pnt) 'single-float)
	(coerce (gp::z pnt) 'single-float)))

(defun create-standin (topods-shape)
  (make-instance 'oc::BRepMesh_IncrementalMesh :S topods-shape :D 7.0d-3 :Relative t)
  (let ((toplevel (make-instance 'toplevel-standin)))
    (setf (slot-value toplevel 'child-standins)
	  (list (create-standin-1 topods-shape nil nil toplevel)))

    (setf (slot-value toplevel 'bounding-box)
	  (when (slot-value toplevel 'child-standins)
	    (apply #'bounding-box nil (append (mapcar #'(lambda (child) (bounding-box-min (slot-value child 'bounding-box))) (slot-value toplevel 'child-standins))
					      (mapcar #'(lambda (child) (bounding-box-max (slot-value child 'bounding-box))) (slot-value toplevel 'child-standins))))))
    
    (populate-vertex-arrays toplevel)
    toplevel))

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS_CompSolid))
  'compsolid-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS_Compound))
  'compound-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS_Solid))
  'solid-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS_Shell))
  'shell-standin)

(defmethod standin-class-for-topods-shape ((shape oc::TopoDS_Wire))
  'wire-standin)


(defmethod create-standin-1 ((shape oc::TopoDS_Shape) parent-standin grandparent-standin toplevel-standin)
  (declare (ignore grandparent-standin))
  (let ((standin (make-instance (standin-class-for-topods-shape shape) :source-shape shape))
	(iterator (make-instance 'oc::TopoDS_Iterator :S shape)))
									 
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

(defmethod create-standin-1 ((vertex oc::TopoDS_Vertex) parent-standin
			     grandparent-standin toplevel-standin)
  (declare (ignore grandparent-standin))
  (let ((standin (make-instance 'vertex-standin :source-shape vertex)))
    (setf (slot-value standin 'toplevel) toplevel-standin)
    (setf (slot-value standin 'bounding-box)
	  (bounding-box nil (pnt->vec3 (gp::make-pnt :ptr (oc::_wrap_BRep_Tool_Pnt (oc::ff-pointer vertex))))))
    standin))

(defmethod create-standin-1 ((face oc::TopoDS_Face) parent-standin
			     grandparent-standin toplevel-standin)
  (declare (ignore grandparent-standin))
  (let* ((standin (make-instance 'face-standin :source-shape face))
	 (triangulation (allocate-instance (find-class 'oc::Poly_Triangulation)))
	 (commands (face-commands toplevel-standin))
	 (vertex-array (polyhedron-nodes toplevel-standin))
	 (index-array (face-indices toplevel-standin))
	 (first-index (fill-pointer index-array))
	 (vertex-offset (fill-pointer vertex-array)))
    (setf (slot-value standin 'toplevel) toplevel-standin)

    (setf (slot-value standin 'vertex-offset) vertex-offset)
    
    (setf (ff-pointer triangulation)
	  (oc::_wrap_BRep_Tool_Triangulation
	   (ff-pointer face) (oc::_wrap_new_TopLoc_Location__SWIG_0))
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
		       (let ((iterator (make-instance 'oc::TopoDS_Iterator :S face)))
			 (loop while (oc::more-p iterator)
			    collect (prog1 (create-standin-1
					    (oc::value iterator) standin parent-standin toplevel-standin)
				      (oc::next iterator)))))))
    standin))

(defmethod standin-number-of-vertices ((standin vertices-mixin))
  (break "!")
  (fill-pointer (slot-value standin 'poly-vertices)))

(defmethod standin-number-of-indices ((standin indices-mixin))
  (break "$")
  (fill-pointer (slot-value standin 'poly-indices)))

(defmethod compute-face-vertex-array-float-size (standin)
  ;; we know only vertices-mixin standins have vertex-array-size
  (loop for child in (slot-value standin 'child-standins)
     sum (compute-face-vertex-array-float-size child)))

(defmethod compute-face-vertex-array-float-size ((standin face-standin))
  (* (/ (cffi:foreign-type-size '(:struct vktk::3DVertex)) (cffi:foreign-type-size :float)) (standin-number-of-vertices standin)))

(defmethod compute-face-index-array-size (standin)
  (loop for child in (slot-value standin 'child-standins)
     sum (compute-face-index-array-size child)))

(defmethod compute-face-index-array-size ((standin face-standin))
  (standin-number-of-indices standin))

(defmethod compute-edge-vertex-array-float-size (standin)
  ;; we know only vertices-mixin standins have vertex-array-size
  (loop for child in (slot-value standin 'child-standins)
     sum (compute-edge-vertex-array-float-size child)))

(defmethod compute-edge-vertex-array-float-size ((standin face-standin))
  (* (/ (cffi:foreign-type-size '(:struct vktk::3DVertex)) (cffi:foreign-type-size :float)) (standin-number-of-vertices standin)))

(defmethod compute-edge-index-array-size (standin)
  (loop for child in (slot-value standin 'child-standins)
     sum (compute-edge-index-array-size child)))

(defmethod compute-edge-index-array-size ((standin edge-standin))
  (standin-number-of-indices standin))

#+NIL
(defmethod populate-vertex-arrays-1 (standin toplevel voffset ioffset-face ioffset-edge)
  (loop for child in (slot-value standin 'child-standins)
     do (multiple-value-setq (voffset ioffset-face ioffset-edge)
	  (populate-vertex-arrays-1 child toplevel voffset ioffset-face ioffset-edge)))
  (values voffset ioffset-face ioffset-edge))

(defstruct draw-indexed-cmd
  (index-count)
  (first-index)
  (vertex-offset))

#+NIL
(defmethod populate-vertex-arrays-1 ((standin face-standin) toplevel voffset ioffset-face ioffset-edge)
  (let ((step (/ (cffi:foreign-type-size '(:struct vktk::3DVertex)) (cffi:foreign-type-size :float)))
	(indices (slot-value standin 'poly-indices))
	(face-vertex-array (face-vertex-array toplevel))
	(edge-vertex-array (edge-vertex-array toplevel)))

    (setf (slot-value standin 'vertex-offset) voffset)

    (setf (slot-value standin 'draw-cmd)
	  (make-draw-indexed-cmd :index-count (fill-pointer indices)
				 :first-index ioffset-face
				 :vertex-offset voffset))
    
    (loop for i from voffset for point across (slot-value standin 'poly-vertices)

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
		  (aref edge-vertex-array (+ float-offset 5)) 0.0f0))
	 
       finally (setq voffset i))
    
    (let ((face-index-array (face-index-array toplevel)))
      (loop for i from ioffset-face for index across indices 
	 do (setf (aref face-index-array i) index)
	 finally (setq ioffset-face i)))

    (let ((ignore1)
	  (ignore2))
      (loop for child in (slot-value standin 'child-standins)
	 do (multiple-value-setq (ignore1 ignore2 ioffset-edge)
	      (populate-vertex-arrays-1 child toplevel nil nil ioffset-edge))))
    
    (values voffset ioffset-face ioffset-edge)))

#+NIL
(defmethod populate-vertex-arrays-1 ((edge edge-standin) toplevel ignore1 ignore2 ioffset-edge)
  (declare (ignore ignore1 ignore2))
  (let ((indices (slot-value edge 'poly-indices))
	(edge-index-array (edge-index-array toplevel)))

    (setf (slot-value edge 'draw-cmd)
	  (make-draw-indexed-cmd :index-count (fill-pointer indices)
				 :first-index ioffset-edge
				 :vertex-offset (slot-value (my-face edge) 'vertex-offset)))
    
    (loop for i from ioffset-edge for index across indices
       do (setf (aref edge-index-array i) index)
       finally (setq ioffset-edge i)))
  (values nil nil ioffset-edge))

(defun populate-vertex-arrays (toplevel)
  (let* ((step (/ (cffi:foreign-type-size '(:struct vktk::3DVertex)) (cffi:foreign-type-size :float)))
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

(defmethod create-standin-1 ((edge oc::TopoDS_Edge) parent-standin
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
	      (let ((iterator (make-instance 'oc::TopoDS_Iterator :S edge)))
		(loop while (oc::more-p iterator)
		   collect (prog1 (create-standin-1 (oc::value iterator)
						    standin parent-standin toplevel-standin)
			     (oc::next iterator)))))
	    
	standin))))
