(cl:in-package #:vulkan)

(cl:defmacro defvkinstextfun ((cname lname) result-type cl:&body args)
  (cl:let ((instance-arg (cl:gensym "INSTANCE")))
    `(cl:defun ,lname (,instance-arg ,@(cl:mapcar 'cl:car args))
       (cffi:foreign-funcall-pointer
	(cffi:with-foreign-string (p-native ,cname)
	  (VkGetInstanceProcAddr ,instance-arg p-native))
        cl:nil
        ,@(cl:loop for arg in args
		   collect (cl:second arg) collect (cl:first arg))
        ,result-type))))

(cl:defmacro defvkdevextfun ((cname lname) result-type cl:&body args)
  (cl:let ((device-arg (cl:gensym "DEVICE")))
    `(cl:defun ,lname (,device-arg ,@(cl:mapcar 'cl:car args))
       (cffi:foreign-funcall-pointer
	(cffi:with-foreign-string (p-native ,cname)
	  (VkGetDeviceProcAddr ,device-arg p-native))
        cl:nil
        ,@(cl:loop for arg in args
		   collect (cl:second arg) collect (cl:first arg))
        ,result-type))))

(cl:defun zero-struct (p struct-typespec)
  (cl:loop for i from 0 below (cffi:foreign-type-size struct-typespec)
     do (cl:setf (cffi:mem-aref p :unsigned-char i) 0))
  (cl:values))

(cl:defmacro with-vk-struct ((p-info struct-type) cl:&body body)
  `(cffi:with-foreign-object (,p-info '(:struct ,struct-type))
     (zero-struct ,p-info '(:struct ,struct-type))
     ,@(cl:when (cl:gethash struct-type *s-type-table*)
	 `((cl:setf (cffi:foreign-slot-value ,p-info '(:struct ,struct-type) 'vk::sType)
		 ,(cl:gethash struct-type *s-type-table*))))
     ,@body))
