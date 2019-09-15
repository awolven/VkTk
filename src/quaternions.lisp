(in-package :igp)

(defun reverse-quaternion (q)
  (vec4 (- (vx q)) (- (vy q)) (- (vz q)) (vw q)))

(defun scale-quaternion (q scale-factor)
  (v* scale-factor q))

(defun quaternion-square-norm (q)
  (+ (expt (vx q) 2) (expt (vy q) 2) (expt (vz q) 2) (expt (vw q) 2)))

(defun quaternion-norm (q)
  (sqrt (quaternion-square-norm q)))

(defun normalize-quaternion (q)
  (let ((magnitude (quaternion-norm q)))
    (when (< magnitude single-float-epsilon)
      (setq q (stabilize-quaternion-length q)
	    magnitude (quaternion-norm q)))
    (scale-quaternion q (/ 1.0f0 magnitude))))

(defun stabilize-quaternion-length (q)
  (let ((cs (+ (abs (vx q)) (abs (vy q)) (abs (vz q)) (abs (vw q)))))
    (if (> cs 0.0f0)
	(vec4 (/ (vx q) cs) (/ (vy q) cs) (/ (vz q) cs) (/ (vw q) cs))
	(vec4 0 0 0 1))))

(defun invert-quaternion (q)
  (let ((in (/ 1.0f0 (quaternion-square-norm q))))
    (vec4 (* in (- (vx q)))
	  (* in (- (vy q)))
	  (* in (- (vz q)))
	  (* in (vw q)))))

(defun add-quaternions (q1 q2)
  (v+ q1 q2))

(defun subtract-quaternions (q1 q2)
  (v- q1 q2))

(defun multiply-quaternions (q1 q2)
  (vec4 (+ (* (vw q1) (vx q2))
	   (* (vx q1) (vw q2))
	   (* (vy q1) (vz q2))
	   (- (* (vz q1) (vy q2))))
	(+ (* (vw q1) (vy q2))
	   (* (vy q1) (vw q2))
	   (* (vz q1) (vx q2))
	   (- (* (vx q1) (vz q2))))
	(+ (* (vw q1) (vz q2))
	   (* (vz q1) (vw q2))
	   (* (vx q1) (vy q2))
	   (- (* (vy q1) (vx q2))))
	(- (* (vw q1) (vw q2))
	   (* (vx q1) (vx q2))
	   (* (vy q1) (vy q2))
	   (* (vz q1) (vz q2)))))

(defun dot-quaternions (q1 q2)
  (v. q1 q2))

(defun rotated-quaternion (from to)
  (let* ((cross (vc from to))
	 (q (normalize-quaternion
	     (vec4 (vx cross) (vy cross) (vz cross) (v. from to)))))
    
    (setf (vw q) (1+ (vw q)))
    
    (normalize-quaternion
     (if (< (vw q) single-float-epsilon)
	 (if (> (expt (vz from) 2) (expt (vx from) 2))
	     (vec4 0.0f0 (vz from) (- (vy from)) (vw q))
	     (vec4 (vy from) (- (vx from)) 0.0f0 (vw q)))
	 q))))

(defun quaternion-from-vector-and-angle (axis angle)
  (setq axis (vunit axis))
  (let* ((angle-half (* 0.5f0 angle))
	 (sin-a (sin angle-half)))
    (vec4 (* sin-a (vx axis))
	  (* sin-a (vy axis))
	  (* sin-a (vz axis))
	  (cos angle-half))))

(defun vector-and-angle-from-quaternion (q)
  (let ((vl (sqrt (+ (* (vx q) (vx q))
		     (* (vy q) (vy q))
		     (* (vz q) (vz q))))))
    (if (> vl single-float-epsilon)
	(let* ((ivl (/ 1.0f0 vl))
	       (axis (vec3 (* ivl (vx q)) (* ivl (vy q)) (* ivl (vz q)))))
	  (if (< (vw q) 0.0f0)
	      (let ((angle (* 2.0f0 (atan (- vl) (- (vw q))))))
		(values axis angle))
	      (let ((angle (* 2.0f0 (atan vl (vw q)))))
		(values axis angle))))
	(values (vec3 0.0f0 0.0f0 1.0f0) 0.0f0))))

(defun quaternion-from-matrix (mat)
  (let ((trace (+ (mcref mat 0 0) (mcref mat 1 1) (mcref4 mat 2 2))))
    (if (> trace 0.0f0)
	(let ((w (+ trace 1.0f0)))
	  (scale-quaternion
	   (vec4 (- (mcref4 mat 2 1) (mcref4 mat 1 2))
		 (- (mcref4 mat 0 2) (mcref4 mat 2 0))
		 (- (mcref4 mat 1 0) (mcref4 mat 0 1))
		 w)
	   (/ 0.5f0 (sqrt w))))
	(if (and (> (mcref4 mat 0 0) (mcref4 mat 1 1))
		 (> (mcref4 mat 1 1) (mcref4 mat 2 2)))
	    (let ((x (- (+ 1.0f0 (mcref4 mat 0 0))
			(mcref4 mat 1 1)
			(mcref4 mat 2 2))))
	      (scale-quaternion
	       (vec4 x
		     (+ (mcref4 mat 0 1) (mcref4 mat 1 0))
		     (+ (mcref4 mat 0 2) (mcref4 mat 2 0))
		     (+ (mcref4 mat 2 1) (mcref4 mat 1 2)))
	       (/ 0.5f0 (sqrt x))))
	    (if (> (mcref4 mat 1 1) (mcref4 mat 2 2))
		(let ((y (- (+ 1.0f0 (mcref4 mat 1 1))
			    (mcref4 mat 0 0)
			    (mcref4 mat 2 2))))
		  (scale-quaternion
		   (vec4 (+ (mcref4 mat 0 1) (mcref4 mat 1 0))
			 y
			 (+ (mcref4 mat 1 2) (mcref4 mat 2 1))
			 (+ (mcref4 mat 0 2) (mcref4 mat 2 0)))
		   (/ 0.5f0 (sqrt y))))
		(let ((z (- (+ 1.0f0 (mcref4 mat 2 2))
			    (mcref4 mat 0 0)
			    (mcref4 mat 1 1))))
		  (scale-quaternion
		   (vec4 (+ (mcref4 mat 0 2) (mcref4 mat 2 0))
			 (+ (mcref4 mat 1 2) (mcref4 mat 2 1))
			 z
			 (- (mcref4 mat 1 0) (mcref4 mat 1 2)))
		   (/ 0.5f0 (sqrt z)))))))))

(defun matrix-from-quaternion (q)
  (let ((x (vx q))
	(y (vy q))
	(z (vz q))
	(w (vw q)))	  
  (let (wx wy wz xx yy yz xy xz zz x2 y2 z2)
    (let ((s (/ 2.0f0 (quaternion-square-norm q))))
      (setq x2 (* x s)  y2 (* y s)  z2 (* z s)
	    xx (* x x2) xy (* x y2) xz (* x z2)
	    yy (* y y2) yz (* y z2) zz (* z z2)
	    wx (* w x2) wy (* w y2) wz (* w z2))
      
      (mat (- 1.0f0 yy zz) (- xy wz) (+ xz wy) 0.0f0
	   (+ xy wz) (- 1.0f0 xx zz) (- yz wx) 0.0f0
	   (- xz wy) (+ yz wx) (- 1.0f0 xx yy) 0.0f0
	   0.0f0 0.0f0 0.0f0 1.0f0)))))
	   
		  
    
	   
