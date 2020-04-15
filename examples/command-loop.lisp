(in-package :igp)

(defclass command-stream (sb-gray:fundamental-character-input-stream
			  sb-gray:fundamental-character-output-stream)
  ((history :initform (make-array (* 1024 1024) :adjustable t :fill-pointer 0 :element-type 'character))
   (input-pointer :initform 0 :accessor input-pointer)
   (input-buffer :initform (make-array 1024 :adjustable t :fill-pointer 0 :element-type 'character))
   (input-available-sem :initform (sb-thread:make-semaphore :name "Input Available Semaphore" :count 0)
			:reader input-available-sem)))

(defvar test "abcd1234
5678

wxyz!")

(defun setup-test ()
  (let* ((stream (make-instance 'command-stream))
	 (buffer (slot-value stream 'input-buffer)))
    (loop for char across test
       do (vector-push-extend char buffer))
    stream))

(defmethod cl:stream-element-type ((stream command-stream))
  'character)

(defmethod close ((stream command-stream) &key abort)
  (declare (ignore abort))
  (setf (slot-value stream 'sb-gray::open-p) nil)
  t)

(defmethod sb-gray:stream-clear-input ((stream command-stream))
  (setf (input-pointer stream) 0
	(fill-pointer (slot-value stream 'input-buffer)) 0)
  t)

(defmethod sb-gray:stream-read-sequence ((stream command-stream) seq &optional start end)
  (unless start
    (setq start 0))
  (unless end
    (setq end (length seq)))
  (let* ((buffer (slot-value stream 'input-buffer)))
    (loop while (eq (input-pointer stream) (length buffer))
       do (sleep 0.001))  
    (let ((amount-to-read (- end start))
	  (amount-available (- (length buffer) (input-pointer stream))))
      (if (> amount-available amount-to-read)
	  (loop for j from (input-pointer stream) for i from start below end 
	     do (setf (elt seq i) (char buffer j))
	     finally (setf (input-pointer stream) j)
	       (return end))
	  (loop for i from start for j from (input-pointer stream) below (length buffer)
	     do (setf (elt seq i) (char buffer j))
	     finally (setf (input-pointer stream) j)
	       (return i))))))

(defmethod sb-gray:stream-write-sequence ((stream command-stream) seq &optional start end)
  (unless start
    (setq start 0))
  (unless end
    (setq end (length seq)))
  (let ((output (slot-value stream 'history)))
  (loop for i from start below end
     do (vector-push-extend (elt seq i) output))
  t))

(defmethod sb-gray:stream-clear-output ((stream command-stream))
  nil)

(defmethod sb-gray:stream-finish-output ((stream command-stream))
  nil)

(defmethod sb-gray:stream-force-output ((stream command-stream))
  nil)

(defmethod sb-gray:stream-peek-char ((stream command-stream))
  (let ((buffer (slot-value stream 'input-buffer)))
    (loop
       (if (eq (input-pointer stream) (length buffer))
	   (sleep 0.01)
	   (return)))
    (char buffer (input-pointer stream))))

(defmethod sb-gray:stream-read-char-no-hang ((stream command-stream))
  (let ((buffer (slot-value stream 'input-buffer)))
    (when (< (input-pointer stream) (length (slot-value stream 'input-buffer)))
      (prog1 (char buffer (input-pointer stream))
	(incf (input-pointer stream))))))

(defmethod sb-gray:stream-read-char ((stream command-stream))
  (let ((buffer (slot-value stream 'input-buffer)))
    (loop
       (if (eq (input-pointer stream) (length buffer))
	   (sleep 0.01)
	   (return)))
    (prog1 (char buffer (input-pointer stream))
      (incf (input-pointer stream)))))

(defmethod sb-gray:stream-read-line ((stream command-stream))
  (let ((buffer (slot-value stream 'input-buffer)))
    (loop
       (if (eq (input-pointer stream) (length (slot-value stream 'input-buffer)))
	   (sleep 0.01)
	   (return)))
    (let ((line (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop for i from (input-pointer stream) below (length buffer)
	 do (let ((char (char buffer i)))
	      (if (char= char #\newline)
		  (progn (setf (input-pointer stream) (1+ i))
			 (return (copy-seq line)))
		  (vector-push-extend char line)))
	 finally (progn (setf (input-pointer stream) i) (return (copy-seq line)))))))

(defmethod sb-gray:stream-listen ((stream command-stream))
  (< (input-pointer stream) (length (slot-value stream 'input-buffer))))

(defmethod sb-gray:stream-unread-char ((stream command-stream) character)
  (when (> (input-pointer stream) 0)
    (setf (char (slot-value stream 'input-buffer) (decf (input-pointer stream)))
	  character)))

(defmethod sb-gray:stream-fresh-line ((stream command-stream))
  (unless (zerop (length (slot-value stream 'history)))
    (let ((newline-position (position #\newline (slot-value stream 'history) :from-end t)))
      (unless (eq newline-position (1- (length (slot-value stream 'history))))
	(vector-push-extend #\newline (slot-value stream 'history))))))

(defmethod sb-gray:stream-line-column ((stream command-stream))
  (let ((newline-position (position #\newline (slot-value stream 'history) :from-end t)))
    (unless newline-position (setq newline-position 0))
    (- (length (slot-value stream 'history)) newline-position)))

(defmethod sb-gray:stream-line-length ((stream command-stream))
  nil)

(defmethod sb-gray:stream-write-char ((stream command-stream) character)
  (vector-push-extend character (slot-value stream 'history)))

(defmethod sb-gray:stream-write-string ((stream command-stream) string &optional start end)
  (unless start
    (setq start 0))
  (unless end
    (setq end (length string)))
  (let ((buffer (slot-value stream 'history)))
    (loop for i from start below end
       do (vector-push-extend (char string i) buffer)))
  string)

(defmethod interactive-stream-p ((stream command-stream))
  t)

(defvar igp::*app*)

(defun start-repl-thread ()
  (sb-thread:make-thread
   (lambda ()
     (let* ((app igp::*app*)
	    (editor (slot-value app 'editor))
	    (stream (slot-value editor 'command-stream))
	    (*standard-output* stream)
	    (*standard-input* stream)
	    (*terminal-io* stream)
	    (*debug-io* stream)
	    (cl:*debugger-hook* #'(lambda (condition me-or-my-encapsulation)
				    (let ((cl:*query-io* stream)
					  (cl:*debug-io* stream))
				      (my-debugger condition me-or-my-encapsulation)))))
       (loop with form with result
	  do (setq form (read stream))
	    (setq result (eval form))
	    (print result))))
   :name "igp::repl thread"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-aclrepl))

(defun start-aclrepl-thread ()
  (sb-thread:make-thread
   (lambda ()
     (let* ((app igp::*app*)
	    (editor (slot-value app 'editor))
	    (stream (slot-value editor 'command-stream))
	    (*standard-output* stream)
	    (*standard-input* stream)
	    (*terminal-io* stream)
	    (*query-io* stream)
	    (*debug-io* stream))
       (sb-aclrepl::repl)
       #+NIL
       (sb-aclrepl::debug-loop)))
   :name "igp::repl thread"))

(defun terminate-repl ()
  (let ((thread (find "igp::repl thread" (sb-thread:list-all-threads) :test #'string= :key #'sb-thread:thread-name)))
    (when thread
      (sb-thread::terminate-thread thread)
      t)))

(defun interrupt-repl ()
  (let ((thread (find "igp::repl thread" (sb-thread:list-all-threads) :test #'string= :key #'sb-thread:thread-name)))
    (when thread
      (sb-thread:interrupt-thread thread #'break))))

(defun one-of (choices &optional (prompt "Choice"))
  (let ((n (length choices))
	(i))
    (do ((c choices (cdr c))  (i 1 (+ i 1)))
	((null c))
      (format t "~&[~D] ~A~%" i (car c)))
    (loop until (typep i `(integer 1 ,n))
       do (format t "~&~A: " prompt)
	 (setq i (read *debug-io*))
	 (fresh-line *debug-io*))
    (nth (- i 1) choices)))

(defun my-debugger (condition me-or-my-encapsulation)
  (format *debug-io* "~&Uh oh: ~A" condition)
  (let ((restart (one-of (compute-restarts))))
    (unless restart (error "My debugger got an error."))
    (let ((cl:*debugger-hook* me-or-my-encapsulation))
      (invoke-restart-interactively restart))))










