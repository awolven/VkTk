(in-package :vkapp)

;; invert-matrix inverts the in-matrix and places the result
;; in out-matrix and then returns the determinant.  The matrices
;; must be square and of the same size.
;;
;; Parameters:
;;    in-matrix  - matrix to invert
;;    out-matrix - matrix to return result in
;;
(defun invert-4x4-matrix (matrix)
  (let* ((in-matrix (marr4 matrix))
	 (out-mat4 (mat4))
	 (out-matrix (marr4 out-mat4)))
  
    (let ((dim 4) ;; dimension of matrix
          (det 1) ;; determinant of matrix
          (l nil) ;; permutation vector
          (m nil) ;; permutation vector
          (temp 0))       

      ;;(if (not (equal dim (array-dimension in-matrix 1)))
	;;  (error "invert-matrix () - matrix not square")
	  ;;)

      ;;(if (not (equal (array-dimensions in-matrix) 
	;;	      (array-dimensions out-matrix)))
	  ;;(error "invert-matrix () - matrices not of the same size")
	  ;;)

      ;; copy in-matrix to out-matrix if they are not the same
      (when (not (equal in-matrix out-matrix))
	(do ((i 0 (1+ i)))
	    ((>= i dim))    
	  (do ((j 0 (1+ j)))
	      ((>= j dim)) 
	    (setf (aref out-matrix (+ i (* j 4))) (aref in-matrix (+ i (* j 4))))
	    )
	  )
        )

      ;; allocate permutation vectors for l and m, with the 
      ;; same origin as the matrix
      (setf l (make-array `(,dim)))
      (setf m (make-array `(,dim)))

      (do ((k 0 (1+ k))
	   (biga 0)
	   (recip-biga 0))
	  ((>= k dim))

	(setf (aref l k) k)
	(setf (aref m k) k)
	(setf biga (aref out-matrix (+ k (* 4 k))))

	;; find the biggest element in the submatrix
	(do ((i k (1+ i)))
	    ((>= i dim))    
	  (do ((j k (1+ j)))
	      ((>= j dim)) 
	    (when (> (abs (aref out-matrix (+ i (* 4 j)))) (abs biga))
	      (setf biga (aref out-matrix (+ i (* 4 j))))
	      (setf (aref l k) i)
	      (setf (aref m k) j)
	      )
	    )
	  )

	;; interchange rows
	(if (> (aref l k) k)
	    (do ((j 0 (1+ j))
		 (i (aref l k)))
		((>= j dim)) 
	      (setf temp (- (aref out-matrix (+ k (* j 4)))))
	      (setf (aref out-matrix (+ k (* j 4))) (aref out-matrix (+ i (* j 4))))
	      (setf (aref out-matrix (+ i (* j 4))) temp)
	      )
            )

	;; interchange columns 
	(if (> (aref m k) k)
	    (do ((i 0 (1+ i))
		 (j (aref m k)))
		((>= i dim)) 
	      (setf temp (- (aref out-matrix (+ i (* k 4)))))
	      (setf (aref out-matrix (+ i (* k 4))) (aref out-matrix (+ i (* j 4))))
	      (setf (aref out-matrix (+ i (* j 4))) temp)
	      )
            )

	;; divide column by minus pivot (value of pivot 
	;; element is in biga)
	(if (equalp biga 0) 
	    (return-from invert-4x4-matrix 0)
            )
	(setf recip-biga (/ 1 biga))
	(do ((i 0 (1+ i)))
	    ((>= i dim)) 
	  (if (not (equal i k))
	      (setf (aref out-matrix (+ i (* k 4)))
		    (* (aref out-matrix (+ i (* k 4))) (- recip-biga)))
	      )
	  )

	;; reduce matrix
	(do ((i 0 (1+ i)))
	    ((>= i dim)) 
	  (when (not (equal i k))
	    (setf temp (aref out-matrix (+ i (* k 4))))
	    (do ((j 0 (1+ j)))
		((>= j dim)) 
	      (if (not (equal j k))
		  (incf (aref out-matrix (+ i (* j 4)))
			(* temp (aref out-matrix (+ k (* j 4)))))
		  )
	      )
	    )
	  )

	;; divide row by pivot
	(do ((j 0 (1+ j)))
	    ((>= j dim)) 
	  (if (not (equal j k))
	      (setf (aref out-matrix (+ k (* j 4)))
		    (* (aref out-matrix (+ k (* j 4))) recip-biga))
	      )
	  )

	(setf det (* det biga))	;; product of pivots
	(setf (aref out-matrix (+ k (* k 4))) recip-biga)
        ) ;; k loop

      ;; final row & column interchanges
      (do ((k (1- dim) (1- k)))
	  ((< k 0))
	(if (> (aref l k) k)
	    (do ((j 0 (1+ j))
		 (i (aref l k)))
		((>= j dim))
	      (setf temp (aref out-matrix (+ j (* k 4))))
	      (setf (aref out-matrix (+ j (* k 4)))
		    (- (aref out-matrix (+ j (* i 4)))))
	      (setf (aref out-matrix (+ j (* i 4))) temp)
	      )
            )
	(if (> (aref m k) k)
	    (do ((i 0 (1+ i))
		 (j (aref m k)))
		((>= i dim))
	      (setf temp (aref out-matrix (+ k (* i 4))))
	      (setf (aref out-matrix (+ k (* i 4)))
		    (- (aref out-matrix (+ j (* i 4)))))
	      (setf (aref out-matrix (+ j (* i 4))) temp)
	      )
            )
        )
      (values out-mat4 det) ;; return determinant
      )
    )
  )

#|

(defun dot-product-3d (a b)
  (+ (* (aref a 0) (aref b 0))
     (* (aref a 1) (aref b 1))
     (* (aref a 2) (aref b 2))))

(defun vector-3d-magnitude (v)
  (sqrt (+ (expt (aref v 0) 2)
	   (expt (aref v 1) 2)
	   (expt (aref v 2) 2))))

(defun allocate-4-tuple ()
  (make-array 4 :element-type 'double-float))

(defun allocate-3-tuple ()
  (make-array 3 :element-type 'double-float))

(defun allocate-2-tuple ()
  (make-array 2 :element-type 'double-float))

(defun make-vector-3d (a b c)
  (let ((v (allocate-3-tuple)))
    (setf (aref v 0) (coerce a 'double-float)
	  (aref v 1) (coerce b 'double-float)
	  (aref v 2) (coerce c 'double-float))
    v))

(defun cross-product (u v)
  (let ((result (allocate-3-tuple)))
    (setf (aref result 0) (- (* (aref u 1) (aref v 2))
			     (* (aref u 2) (aref v 1)))
	  (aref result 1) (- (* (aref u 2) (aref v 0))
			     (* (aref u 0) (aref v 2)))
	  (aref result 2) (- (* (aref u 0) (aref v 1))
			     (* (aref u 1) (aref v 0))))
    result))

(defun normalize-vector-3d (v)
  (let ((a (aref v 0))
	(b (aref v 1))
	(c (aref v 2))
	(n (allocate-3-tuple)))
    (let ((mag (sqrt (+ (* a a) (* b b) (* c c)))))
      (setf (aref n 0) (/ a mag)
	    (aref n 1) (/ b mag)
	    (aref n 2) (/ c mag))
      n)))

(defconstant +tol+ double-float-epsilon)

(defun subtract-vectors-3d (a b)
  (let ((result (allocate-3-tuple)))
    (setf (aref result 0) (- (aref a 0) (aref b 0))
	  (aref result 1) (- (aref a 1) (aref b 1))
	  (aref result 2) (- (aref a 2) (aref b 2)))
    result))

(defun negate-vector (v)
  (let ((result (allocate-3-tuple)))
    (setf (aref result 0) (- (aref v 0))
	  (aref result 1) (- (aref v 1))
	  (aref result 2) (- (aref v 2)))
    result))

(defun multiply-scalar-vector-3d (mag v)
  (let ((result (allocate-3-tuple)))
    (setf (aref result 0) (* mag (aref v 0))
	  (aref result 1) (* mag (aref v 1))
	  (aref result 2) (* mag (aref v 2)))
    result))

(defun homogeneous-3d-matrix-vector-product (m v)
  (declare (type (simple-array double-float (4 4)) m))
  (let ((result (allocate-4-tuple)))
    (setf (aref result 0) (+ (* (aref m 0 0) (aref v 0))
			     (* (aref m 1 0) (aref v 1))
			     (* (aref m 2 0) (aref v 2))
			     (* (aref m 3 0) (aref v 3)))
	  (aref result 1) (+ (* (aref m 0 1) (aref v 0))
			     (* (aref m 1 1) (aref v 1))
			     (* (aref m 2 1) (aref v 2))
			     (* (aref m 3 1) (aref v 3)))
	  (aref result 2) (+ (* (aref m 0 2) (aref v 0))
			     (* (aref m 1 2) (aref v 1))
			     (* (aref m 2 2) (aref v 2))
			     (* (aref m 3 2) (aref v 3)))
	  (aref result 3) (+ (* (aref m 0 3) (aref v 0))
			     (* (aref m 1 3) (aref v 1))
			     (* (aref m 2 3) (aref v 2))
			     (* (aref m 3 3) (aref v 3))))
			     
    result))

(defun allocate-4x4-matrix ()
  (make-array '(4 4) :element-type 'double-float))

(defun transpose-4x4-matrix (m)
  (let ((mt (allocate-4x4-matrix)))
    (setf (aref mt 0 0) (aref m 0 0)
	  (aref mt 0 1) (aref m 1 0)
	  (aref mt 0 2) (aref m 2 0)
	  (aref mt 0 3) (aref m 3 0)
	  (aref mt 1 0) (aref m 0 1)
	  (aref mt 1 1) (aref m 1 1)
	  (aref mt 1 2) (aref m 2 1)
	  (aref mt 1 3) (aref m 3 1)
	  (aref mt 2 0) (aref m 0 2)
	  (aref mt 2 1) (aref m 1 2)
	  (aref mt 2 2) (aref m 2 2)
	  (aref mt 2 3) (aref m 3 2)
	  (aref mt 3 0) (aref m 0 3)
	  (aref mt 3 1) (aref m 1 3)
	  (aref mt 3 2) (aref m 2 3)
	  (aref mt 3 3) (aref m 3 3))
    mt))
	
(defun multiply-homogeneous-transformation-matrices (A B)
  (let ((result (allocate-4x4-matrix)))
    (setf (aref result 0 0) (+ (* (aref A 0 0) (aref B 0 0))
			       (* (aref A 0 1) (aref B 1 0))
			       (* (aref A 0 2) (aref B 2 0))
			       (* (aref A 0 3) (aref B 3 0)))
	  (aref result 0 1) (+ (* (aref A 0 0) (aref B 0 1))
			       (* (aref A 0 1) (aref B 1 1))
			       (* (aref A 0 2) (aref B 2 1))
			       (* (aref A 0 3) (aref B 3 1)))
	  (aref result 0 2) (+ (* (aref A 0 0) (aref B 0 2))
			       (* (aref A 0 1) (aref B 1 2))
			       (* (aref A 0 2) (aref B 2 2))
			       (* (aref A 0 3) (aref B 3 2)))
	  (aref result 0 3) (+ (* (aref A 0 0) (aref B 0 3))
			       (* (aref A 0 1) (aref B 1 3))
			       (* (aref A 0 2) (aref B 2 3))
			       (* (aref A 0 3) (aref B 3 3)))

	  (aref result 1 0) (+ (* (aref A 1 0) (aref B 0 0))
			       (* (aref A 1 1) (aref B 1 0))
			       (* (aref A 1 2) (aref B 2 0))
			       (* (aref A 1 3) (aref B 3 0)))
	  (aref result 1 1) (+ (* (aref A 1 0) (aref B 0 1))
			       (* (aref A 1 1) (aref B 1 1))
			       (* (aref A 1 2) (aref B 2 1))
			       (* (aref A 1 3) (aref B 3 1)))
	  (aref result 1 2) (+ (* (aref A 1 0) (aref B 0 2))
			       (* (aref A 1 1) (aref B 1 2))
			       (* (aref A 1 2) (aref B 2 2))
			       (* (aref A 1 3) (aref B 3 2)))
	  (aref result 1 3) (+ (* (aref A 1 0) (aref B 0 3))
			       (* (aref A 1 1) (aref B 1 3))
			       (* (aref A 1 2) (aref B 2 3))
			       (* (aref A 1 3) (aref B 3 3)))

	  (aref result 2 0) (+ (* (aref A 2 0) (aref B 0 0))
			       (* (aref A 2 1) (aref B 1 0))
			       (* (aref A 2 2) (aref B 2 0))
			       (* (aref A 2 3) (aref B 3 0)))
	  (aref result 2 1) (+ (* (aref A 2 0) (aref B 0 1))
			       (* (aref A 2 1) (aref B 1 1))
			       (* (aref A 2 2) (aref B 2 1))
			       (* (aref A 2 3) (aref B 3 1)))
	  (aref result 2 2) (+ (* (aref A 2 0) (aref B 0 2))
			       (* (aref A 2 1) (aref B 1 2))
			       (* (aref A 2 2) (aref B 2 2))
			       (* (aref A 2 3) (aref B 3 2)))
	  (aref result 2 3) (+ (* (aref A 2 0) (aref B 0 3))
			       (* (aref A 2 1) (aref B 1 3))
			       (* (aref A 2 2) (aref B 2 3))
			       (* (aref A 2 3) (aref B 3 3)))

	  (aref result 3 0) (+ (* (aref A 3 0) (aref B 0 0))
			       (* (aref A 3 1) (aref B 1 0))
			       (* (aref A 3 2) (aref B 2 0))
			       (* (aref A 3 3) (aref B 3 0)))
	  (aref result 3 1) (+ (* (aref A 3 0) (aref B 0 1))
			       (* (aref A 3 1) (aref B 1 1))
			       (* (aref A 3 2) (aref B 2 1))
			       (* (aref A 3 3) (aref B 3 1)))
	  (aref result 3 2) (+ (* (aref A 3 0) (aref B 0 2))
			       (* (aref A 3 1) (aref B 1 2))
			       (* (aref A 3 2) (aref B 2 2))
			       (* (aref A 3 3) (aref B 3 2)))
	  (aref result 3 3) (+ (* (aref A 3 0) (aref B 0 3))
			       (* (aref A 3 1) (aref B 1 3))
			       (* (aref A 3 2) (aref B 2 3))
			       (* (aref A 3 3) (aref B 3 3))))
    result))


(defun allocate-4x4-identity-matrix ()
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) 1.0d0
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) 0.0d0
	  (aref m 0 3) 0.0d0

	  (aref m 1 0) 0.0d0
	  (aref m 1 1) 1.0d0
	  (aref m 1 2) 0.0d0
	  (aref m 1 3) 0.0d0

	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) 1.0d0
	  (aref m 2 3) 0.0d0

	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) 0.0d0
	  (aref m 3 3) 1.0d0)
    m))


(defun rotation-matrix (axis angle)
  (let* ((m (allocate-4x4-matrix))
	 (u (normalize-vector-3d axis))
	 (C (cos angle))
	 (S (sin angle))
	 (1-C (1- C))
	 (ux (aref u 0))
	 (uy (aref u 1))
	 (uz (aref u 2)))

    (setf (aref m 0 0) (+ (* 1-C ux ux) C)
	  (aref m 0 1) (- (* 1-C ux uy) (* S uz))
	  (aref m 0 2) (+ (* 1-C ux uz) (* S uy))
	  (aref m 0 3) 0.0d0

	  (aref m 1 0) (+ (* 1-C ux uy) (* S uz))
	  (aref m 1 1) (+ (* 1-C uy uy) C)
	  (aref m 1 2) (- (* 1-C uy uz) (* S ux))
	  (aref m 1 3) 0.0d0

	  (aref m 2 0) (- (* 1-C ux uz) (* S uy))
	  (aref m 2 1) (+ (* 1-C uy uz) (* S ux))
	  (aref m 2 2) (+ (* 1-C uz uz) C)
	  (aref m 2 3) 0.0d0

	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) 0.0d0
	  (aref m 3 3) 1.0d0)
    m))
	
(defun look-at (eye-position-3d center-3d up-vector3d)
  (let ((forward (allocate-3-tuple))
	(side)
	(up)
	(matrix (allocate-4x4-identity-matrix))
	(matrix2 (allocate-4x4-matrix))
	(result-matrix))

    (setf (aref forward 0) (- (aref center-3d 0) (aref eye-position-3d 0))
	  (aref forward 1) (- (aref center-3d 1) (aref eye-position-3d 1))
	  (aref forward 2) (- (aref center-3d 2) (aref eye-position-3d 2)))

    (setq forward (normalize-vector-3d forward))

    (setq side (cross-product forward up-vector3d))

    (setq side (normalize-vector-3d side))

    (setq up (cross-product side forward))

    (setf (aref up 0) (- (aref up 0))
	  (aref up 1) (- (aref up 1))
	  (aref up 2) (- (aref up 2)))

    (setf (aref matrix2 0 0) (aref side 0)
	  (aref matrix2 0 1) (aref side 1)
	  (aref matrix2 0 2) (aref side 2)
	  (aref matrix2 0 3) 0.0d0

	  (aref matrix2 1 0) (aref up 0)
	  (aref matrix2 1 1) (aref up 1)
	  (aref matrix2 1 2) (aref up 2)
	  (aref matrix2 1 3) 0.0d0

	  (aref matrix2 2 0) (- (aref forward 0))
	  (aref matrix2 2 1) (- (aref forward 1))
	  (aref matrix2 2 2) (- (aref forward 2))
	  (aref matrix2 2 3) 0.0d0

	  (aref matrix2 3 0) 0.0d0
	  (aref matrix2 3 1) 0.0d0
	  (aref matrix2 3 2) 0.0d0
	  (aref matrix2 3 3) 1.0d0)

    (setq result-matrix
	  (multiply-homogeneous-transformation-matrices matrix matrix2))

    (translate-matrix result-matrix
		      (- (aref eye-position-3d 0))
		      (- (aref eye-position-3d 1))
		      (- (aref eye-position-3d 2)))

    result-matrix))

(defun look-at2 (eye target up-dir)
  (let* ((forward (normalize-vector-3d (subtract-vectors-3d eye target)))
	 (left (normalize-vector-3d (cross-product up-dir forward)))
	 (up (cross-product forward left))
	 (view-matrix (allocate-4x4-matrix)))
    ;; column-major
    (setf (aref view-matrix 0 0) (aref left 0)
	  (aref view-matrix 0 1) (aref left 1)
	  (aref view-matrix 0 2) (aref left 2)
	  (aref view-matrix 0 3) (- (dot-product-3d left eye))
	  (aref view-matrix 1 0) (aref up 0)
	  (aref view-matrix 1 1) (aref up 1)
	  (aref view-matrix 1 2) (aref up 2)
	  (aref view-matrix 1 3) (- (dot-product-3d up eye))
	  (aref view-matrix 2 0) (aref forward 0)
	  (aref view-matrix 2 1) (aref forward 1)
	  (aref view-matrix 2 2) (aref forward 2)
	  (aref view-matrix 2 3) (- (dot-product-3d forward eye))
	  (aref view-matrix 3 0) 0.0d0
	  (aref view-matrix 3 1) 0.0d0
	  (aref view-matrix 3 2) 0.0d0
	  (aref view-matrix 3 3) 1.0d0)
    view-matrix))

#|      matrix[0][0] =      rcos + u*u*(1-rcos);
	matrix[1][0] =  w * rsin + v*u*(1-rcos);
	matrix[2][0] = -v * rsin + w*u*(1-rcos);
	matrix[0][1] = -w * rsin + u*v*(1-rcos);
	matrix[1][1] =      rcos + v*v*(1-rcos);
	matrix[2][1] =  u * rsin + w*v*(1-rcos);
	matrix[0][2] =  v * rsin + u*w*(1-rcos);
	matrix[1][2] = -u * rsin + v*w*(1-rcos);
	matrix[2][2] =      rcos + w*w*(1-rcos);
|#
(defun rotation-matrix2 (axis angle)
  (setq axis (normalize-vector-3d axis))
  (let* ((u (aref axis 0))
	 (v (aref axis 1))
	 (w (aref axis 2))
	 (-u (- u))
	 (-v (- v))
	 (-w (- w))	   
	(rcos (cos angle))
	(rsin (sin angle))
	(1-rcos (1- rcos))
	(m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (+       rcos  (* u u 1-rcos))
	  (aref m 0 1) (+ (*  w rsin) (* v u 1-rcos))
	  (aref m 0 2) (+ (* -v rsin) (* w u 1-rcos))
	  (aref m 0 3) 0.0d0
	  (aref m 1 0) (+ (* -w rsin) (* u v 1-rcos))
	  (aref m 1 1) (+       rcos  (* v v 1-rcos))
	  (aref m 1 2) (+ (*  u rsin) (* w v 1-rcos))
	  (aref m 1 3) 0.0d0
	  (aref m 2 0) (+ (*  v rsin) (* u w 1-rcos))
	  (aref m 2 1) (+ (* -u rsin) (* v w 1-rcos))
	  (aref m 2 2) 1.0d0;;(+       rcos  (* w w 1-rcos))
	  (aref m 2 3) 0.0d0
	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) 0.0d0
	  (aref m 3 3) 1.0d0)
    m))
			  
	
    


(defun perspective-projection-matrix (fovy aspect z-near z-far)
  (let ((m (allocate-4x4-matrix))
	(f (/ 1.0d0 (tan (/ fovy 2.0d0)))))
    (setf (aref m 0 0) (/ f aspect)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) 0.0d0
	  (aref m 0 3) 0.0d0
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (- f)
	  (aref m 1 2) 0.0d0
	  (aref m 1 3) 0.0d0
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (/ (+ z-near z-far) (- z-near z-far))
	  (aref m 2 3) (/ (* 2.0d0 z-near z-far) (- z-near z-far))
	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) -1.0d0
	  (aref m 3 3) 1.0d0)
    m))


;[ 2n/(r-l)   0   -(r+l)/(r-l)   0     ]
;[ 0      -2n/(t-b) (t+b)/(t-b)  0     ]  
;[ 0          0     -f/(n-f)  fn/(n-f) ]
;[ 0          0        1         0     ]

(defun perspective-projection-matrix-2 (left right bottom top near far)
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (coerce (/ (* -2 near) (- right left)) 'double-float)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) (coerce (- (/ (+ right left) (- right left))) 'double-float)
	  (aref m 0 3) 0.0d0
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (coerce (/ (* 2 near) (- top bottom)) 'double-float)
	  (aref m 1 2) (coerce (/ (+ top bottom) (- top bottom)) 'double-float)
	  (aref m 1 3) 0.0d0
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (coerce (- (/ far (- far near))) 'double-float)
	  (aref m 2 3) (coerce (- (/ (* far near) (- far near))) 'double-float)
	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) -1.0d0
	  (aref m 3 3) 0.0d0)
    m))
  ;;(coerce (* -2 (/ (* far near) (- far near))) 'double-float)


(defun perspective-projection-matrix-3 (left right bottom top near far)
  (declare (ignore far))
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (coerce (/ (* 2.0d0 near) (- right left)) 'double-float)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) (coerce (- (/ (+ right left) (- right left))) 'double-float)
	  (aref m 0 3) 0.0d0
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (coerce (/ (* 2.0d0 near) (- top bottom)) 'double-float)
	  (aref m 1 2) (coerce (/ (+ top bottom) (- top bottom)) 'double-float)
	  (aref m 1 3) 0.0d0
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (coerce (- (/ 1.0d0 near)) 'double-float)
	  (aref m 2 3) 1.0d0
	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) 1.0d0
	  (aref m 3 3) 0.0d0)
    m))

(defun orthographic-projection-matrix (left right bottom top near far)
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (coerce (/ 2.0d0 (- right left)) 'double-float)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) 0.0d0
	  (aref m 0 3) 0.0d0
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (coerce (/ 2.0d0 (- top bottom)) 'double-float)
	  (aref m 1 2) 0.0d0
	  (aref m 1 3) 0.0d0
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (coerce (/ -2.0d0 (- far near)) 'double-float)
	  (aref m 2 3) 0.0d0
	  (aref m 3 0) (coerce (- (/ (+ right left) (- right left))) 'double-float)
	  (aref m 3 1) (coerce (- (/ (+ top bottom) (- top bottom))) 'double-float)
	  (aref m 3 2) (coerce (- (/ (+ far near) (- far near))) 'double-float)
	  (aref m 3 3) 1.0d0)
    m))

(defun orthographic-projection-matrix2 (left right bottom top near far)
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (coerce (/ 2.0d0 (- right left)) 'double-float)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) 0.0d0
	  (aref m 0 3) 0.0d0
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (coerce (/ 2.0d0 (- top bottom)) 'double-float)
	  (aref m 1 2) 0.0d0
	  (aref m 1 3) 0.0d0
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (coerce (/ 1.0d0 (- far near)) 'double-float)
	  (aref m 2 3) 0.0d0
	  (aref m 3 0) 0.0d0;;(coerce (- (/ (+ right left) (- right left))) 'double-float)
	  (aref m 3 1) 0.0d0;;(coerce (- (/ (+ top bottom) (- top bottom))) 'double-float)
	  (aref m 3 2) (coerce (- (/ near (- far near))) 'double-float)
	  (aref m 3 3) 1.0d0)
    m))

(defun orthographic-projection-matrix3 (left right bottom top near far)
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (coerce (/ 2.0d0 (- right left)) 'double-float)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) 0.0d0
	  (aref m 0 3) (coerce (- (/ (+ right left) (- right left))) 'double-float)
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (coerce (/ 2.0d0 (- top bottom)) 'double-float)
	  (aref m 1 2) 0.0d0
	  (aref m 1 3) (coerce (- (/ (+ top bottom) (- top bottom))) 'double-float)
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (coerce (/ -2.0d0 (- far near)) 'double-float)
	  (aref m 2 3) (coerce (- (/ (+ far near) (- far near))) 'double-float)
	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) 0.0d0
	  (aref m 3 3) 1.0d0)
    m))

#+NIL
(defun orthographic-projection-matrix-2 (left right bottom top near far)
  (let ((m (allocate-4x4-matrix)))
    (setf (aref m 0 0) (coerce (/ 2.0d0 (- right left)) 'double-float)
	  (aref m 0 1) 0.0d0
	  (aref m 0 2) 0.0d0
	  (aref m 0 3) (coerce (- (/ (+ right left) (- right left))) 'double-float)
	  (aref m 1 0) 0.0d0
	  (aref m 1 1) (coerce (/ -2.0d0 (- top bottom)) 'double-float)
	  (aref m 1 2) 0.0d0
	  (aref m 1 3) (coerce (/ (+ top bottom) (- top bottom)) 'double-float)
	  (aref m 2 0) 0.0d0
	  (aref m 2 1) 0.0d0
	  (aref m 2 2) (coerce (/ 2.0d0 (- far near)) 'double-float)
	  (aref m 2 3) (coerce (- (/ (+ far near) (- far near))) 'double-float)
	  (aref m 3 0) 0.0d0
	  (aref m 3 1) 0.0d0
	  (aref m 3 2) 0.0d0
	  (aref m 3 3) 1.0d0)
    m))
  
	  
(defun translate-matrix (matrix dx dy dz)
  (setf (aref matrix 3 0) (coerce dx 'double-float)
	(aref matrix 3 1) (coerce dy 'double-float)
	(aref matrix 3 2) (coerce dz 'double-float))
  (values))

(defun copy-matrix (source dest)
  (setf (aref dest 0 0) (aref source 0 0)
	(aref dest 0 1) (aref source 0 1)
	(aref dest 0 2) (aref source 0 2)
	(aref dest 0 3) (aref source 0 3)
	(aref dest 1 0) (aref source 1 0)
	(aref dest 1 1) (aref source 1 1)
	(aref dest 1 2) (aref source 1 2)
	(aref dest 1 3) (aref source 1 3)
	(aref dest 2 0) (aref source 2 0)
	(aref dest 2 1) (aref source 2 1)
	(aref dest 2 2) (aref source 2 2)
	(aref dest 2 3) (aref source 2 3)
	(aref dest 3 0) (aref source 3 0)
	(aref dest 3 1) (aref source 3 1)
	(aref dest 3 2) (aref source 3 2)
	(aref dest 3 3) (aref source 3 3))
  (values))
  
;;(defun column-major-mxm (m1 m2)
  
|#
