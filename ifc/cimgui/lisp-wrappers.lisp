(in-package :imgui)

(defun show-demo-window (&optional (open nil))
  (with-foreign-object (p-open :bool)
    (setf (mem-aref p-open :bool) open)
    (igShowDemoWindow p-open)
    (mem-aref p-open :bool)))

(defun begin (name
	      &key
		open
		(p-open t)
		no-title-bar
		no-resize
		no-move
		no-scrollbar
		no-scroll-with-mouse
		no-collapse
		always-auto-resize
		no-background
		no-saved-settings
		no-mouse-inputs
		menu-bar
		horizontal-scrollbar
		no-focus-on-appearing
		no-bring-to-front-on-focus
		always-vertical-scrollbar
		always-horizontal-scrollbar
		always-use-window-padding
		no-nav-inputs
		no-nav-focus
		unsaved-document
		no-nav
		no-decoration
		no-inputs
		nav-flattened
		child-window
		tooltip
		popup
		modal
		child-menu)
  (let ((flags (logior (if no-title-bar ImGuiWindowFlags_NoTitleBar 0)
		       (if no-resize ImGuiWindowFlags_NoResize 0)
		       (if no-move ImGuiWindowFlags_NoMove 0)
		       (if no-scrollbar ImGuiWindowFlags_NoScrollbar 0)
		       (if no-scroll-with-mouse ImGuiWindowFlags_NoScrollWithMouse 0)
		       (if no-collapse ImGuiWindowFlags_NoCollapse 0)
		       (if always-auto-resize ImGuiWindowFlags_AlwaysAutoResize 0)
		       (if no-background ImGuiWindowFlags_NoBackground 0)
		       (if no-saved-settings ImGuiWindowFlags_NoSavedSettings 0)
		       (if no-mouse-inputs ImGuiWindowFlags_NoMouseInputs 0)
		       (if menu-bar ImGuiWindowFlags_MenuBar 0)
		       (if horizontal-scrollbar ImGuiWindowFlags_HorizontalScrollbar 0)
		       (if no-focus-on-appearing ImGuiWindowFlags_NoFocusOnAppearing 0)
		       (if no-bring-to-front-on-focus ImGuiWindowFlags_NoBringToFrontOnFocus 0)
		       (if always-vertical-scrollbar ImGuiWindowFlags_AlwaysVerticalScrollbar 0)
		       (if always-horizontal-scrollbar ImGuiWindowFlags_AlwaysHorizontalScrollbar 0)
		       (if always-use-window-padding ImGuiWindowFlags_AlwaysUseWindowPadding 0)
		       (if no-nav-inputs ImGuiWindowFlags_NoNavInputs 0)
		       (if no-nav-focus ImGuiWindowFlags_NoNavFocus 0)
		       (if unsaved-document ImGuiWindowFlags_UnsavedDocument 0)
		       (if no-nav ImGuiWindowFlags_NoNav 0)
		       (if no-decoration ImGuiWindowFlags_NoDecoration 0)
		       (if no-inputs ImGuiWindowFlags_NoInputs 0)
		       (if nav-flattened ImGuiWindowFlags_NavFlattened 0)
		       (if child-window ImGuiWindowFlags_ChildWindow 0)
		       (if tooltip ImGuiWindowFlags_Tooltip 0)
		       (if popup ImGuiWindowFlags_Popup 0)
		       (if modal ImGuiWindowFlags_Modal 0)
		       (if child-menu ImGuiWindowFlags_ChildMenu 0))))
    (if p-open 
	(with-foreign-object (p-open :bool)
	  (setf (mem-aref p-open :bool) open)
	  (values (igBegin name p-open flags)
		  (mem-aref p-open :bool)))
	(igBegin name (sb-sys:int-sap 0) flags))))
	
			   
(defun end ()
  (igEnd))



(defun set-next-window-pos (x y &key (condition nil)
				  (pivot-x 0.0f0)
				  (pivot-y 0.0f0))
  
  (with-foreign-objects ((p-pos '(:struct ImVec2_Simple))
			 (p-pivot '(:struct ImVec2_Simple)))
    (setf (foreign-slot-value p-pos '(:struct ImVec2_Simple) 'ig::x) (coerce x 'single-float)
	  (foreign-slot-value p-pos '(:struct ImVec2_Simple) 'ig::y) (coerce y 'single-float)
	  (foreign-slot-value p-pivot '(:struct ImVec2_Simple) 'ig::x) (coerce pivot-x 'single-float)
	  (foreign-slot-value p-pivot '(:struct ImVec2_Simple) 'ig::y) (coerce pivot-y 'single-float))

    (igSetNextWindowPos p-pos
			(case condition
			  (:always ImGuiCond_Always)
			  (:once ImGuiCond_Once)
			  (:first-use-ever ImGuiCond_FirstUseEver)
			  (:appearing ImGuiCond_Appearing)
			  (t 0))
			p-pivot)))

(defun set-next-window-size (dx dy &key (condition nil))
  (with-foreign-object (p-size '(:struct ImVec2_Simple))
    (setf (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::x) (coerce dx 'single-float)
	  (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::y) (coerce dy 'single-float))
    (let ((condition (case condition
		       (:always ImGuiCond_Always)
		       (:once ImGuiCond_Once)
		       (:first-use-ever ImGuiCond_FirstUseEver)
		       (:appearing ImGuiCond_Appearing)
		       (t 0))))
      
      (igSetNextWindowSize p-size condition))))

(defun set-cursor-screen-pos (x y)
  (with-foreign-object (p-pos '(:struct ImVec2_Simple))
    (setf (foreign-slot-value p-pos '(:struct ImVec2_Simple) 'ig::x) (coerce x 'single-float)
	  (foreign-slot-value p-pos '(:struct ImVec2_Simple) 'ig::y) (coerce y 'single-float))
    (igSetCursorScreenPos p-pos)))

(defun text (string &rest format)
  (igText (apply #'format nil string format)))

(defun bullet-text (string &rest format)
  (igBulletText (apply #'format nil string format)))

(defun push-item-width (item-width)
  (igPushItemWidth (coerce item-width 'single-float)))

(defun pop-item-width ()
  (igPopItemWidth))

(defun calc-item-width ()
  (igCalcItemWidth))

(defun push-text-wrap-pos (&optional (wrap-local-pos-x 0.0f0))
  (igPushTextWrapPos (coerce wrap-local-pos-x 'single-float)))

(defun pop-text-wrap-pos ()
  (igPopTextWrapPos))

(defun push-allow-keyboard-focus (allow-keyboard-focus-p)
  (igPushAllowKeyboardFocus allow-keyboard-focus-p))

(defun pop-allow-keyboard-focus ()
  (igPopAllowKeyboardFocus))

(defun push-button-repeat (repeat-p)
  (igPushButtonRepeat repeat-p))

(defun pop-button-repeat ()
  (igPopButtonRepeat))

(defun separator ()
  (igSeparator))

(defun same-line (&optional (local-pos-x 0.0f0)
		    (spacing-w -1.0f0))
  (igSameLine (coerce local-pos-x 'single-float) (coerce spacing-w 'single-float)))

(defun new-line ()
  (igNewLine))

(defun spacing ()
  (igSpacing))

(defun dummy (dx dy)
  (with-foreign-object (p-size '(:struct ImVec2_Simple))
    (setf (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::x) (coerce dx 'single-float)
	  (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::y) (coerce dy 'single-float))

    (igDummy p-size)))
  
(defun indent (width)
  (assert (typep width 'number))
  (setq width (coerce width 'single-float))
  (igIndent width))

(defun unindent (width)
  (assert (typep width 'number))
  (setq width (coerce width 'single-float))
  (igUnindent width))

(defun begin-group ()
  (igBeginGroup))

(defun end-group ()
  (igEndGroup))

(defun imgui-get-cursor-pos ()
  (let ((p-pos (igGetCursorPos)))
    (prog1 (values (foreign-slot-value p-pos '(:struct ImVec2_Simple) 'ig::x)
		   (foreign-slot-value p-pos '(:struct ImVec2_Simple) 'ig::y))
      (foreign-free p-pos))))

(defun get-cursor-pos-x ()
  (igGetCursorPosX))

(defun get-cursor-pos-y ()
  (igGetCursorPosY))

(defun begin-menu-bar ()
  (igBeginMenuBar))

(defun get-font-size ()
  (igGetFontSize))

(defun begin-menu (label &optional (enabled-p t))
  (igBeginMenu label enabled-p))

(defun end-menu ()
  (igEndMenu))

(defun menu-item (label &optional (shortcut nil) (selected-p nil) (enabled-p t))
  (igMenuItemBool label (if shortcut shortcut (sb-sys:int-sap 0)) selected-p enabled-p))

(defun collapsing-header (label &key selected
				  framed
				  allow-item-overlap
				  no-tree-push-on-open
				  no-auto-open-on-log
				  default-open
				  open-on-double-click
				  open-on-arrow
				  leaf
				  bullet
				  frame-padding
				  nav-left-jumps-back-here
				  collapsing-header)
  (let ((flags
	 (logior
	  (if selected ImGuiTreeNodeFlags_Selected 0)
	  (if framed ImGuiTreeNodeFlags_Framed 0)
	  (if allow-item-overlap ImGuiTreeNodeFlags_AllowItemOverlap 0)
	  (if no-tree-push-on-open ImGuiTreeNodeFlags_NoTreePushOnOpen 0)
	  (if no-auto-open-on-log ImGuiTreeNodeFlags_NoAutoOpenOnLog 0)
	  (if default-open ImGuiTreeNodeFlags_DefaultOpen 0)
	  (if open-on-double-click ImGuiTreeNodeFlags_OpenOnDoubleClick 0)
	  (if open-on-arrow ImGuiTreeNodeFlags_OpenOnArrow 0)
	  (if leaf ImGuiTreeNodeFlags_Leaf 0)
	  (if bullet ImGuiTreeNodeFlags_Bullet 0)
	  (if frame-padding ImGuiTreeNodeFlags_FramePadding 0)
	  (if nav-left-jumps-back-here ImGuiTreeNodeFlags_NavLeftJumpsBackHere 0)
	  (if collapsing-header ImGuiTreeNodeFlags_CollapsingHeader 0))))

    (igCollapsingHeader label flags)))

(defun key-pressed-p (imgui-key &optional (repeat-p t))
  (let ((index (igGetKeyIndex imgui-key)))
    (igIsKeyPressed index repeat-p)))

(defun tree-pop ()
  (igTreePop))

(defun text-wrapped (format &rest format-args)
  (igTextWrapped (apply #'format nil format format-args)))

(defun log-to-clipboard (&optional (max-depth nil))
  (igLogToClipboard (if max-depth max-depth -1)))

(defun log-text (format &rest format-args)
  (igLogText (apply #'format nil format format-args)))

(defun log-finish ()
  (igLogFinish))

(defstruct bool-receptor
  (ptr (foreign-alloc :bool)))

(defmethod get-receptor-value ((receptor bool-receptor))
  (mem-aref (bool-receptor-ptr receptor) :bool))

(defstruct flags-receptor
  (ptr (foreign-alloc :unsigned-int)))

(defmethod get-receptor-value ((receptor flags-receptor))
  (mem-aref (flags-receptor-ptr receptor) :unsigned-int))

(defun checkbox (label receptor)
  (igCheckbox label (bool-receptor-ptr receptor)))

(defun checkbox-flags (label receptor flags-value)
  (igCheckboxFlags label receptor flags-value))

(defun radio-button (label active-p)
  (igRadioButtonBool label active-p))

(defun begin-main-menu-bar ()
  (igBeginMainMenuBar))

(defun end-main-menu-bar ()
  (igEndMainMenuBar))

(defun begin-child (id &key
			 (dx 0.0f0)
			 (dy 0.0f0)
			 (border-p nil)
			 no-title-bar
			 no-resize
			 no-move
			 no-scrollbar
			 no-scroll-with-mouse
			 no-collapse
			 always-auto-resize
			 no-background
			 no-saved-settings
			 no-mouse-inputs
			 menu-bar
			 horizontal-scrollbar
			 no-focus-on-appearing
			 no-bring-to-front-on-focus
			 always-vertical-scrollbar
			 always-horizontal-scrollbar
			 always-use-window-padding
			 no-nav-inputs
			 no-nav-focus
			 unsaved-document
			 no-nav
			 no-decoration
			 no-inputs
			 nav-flattened
			 child-window
			 tooltip
			 popup
			 modal
			 child-menu)
  
  (let ((flags (logior (if no-title-bar ImGuiWindowFlags_NoTitleBar 0)
		       (if no-resize ImGuiWindowFlags_NoResize 0)
		       (if no-move ImGuiWindowFlags_NoMove 0)
		       (if no-scrollbar ImGuiWindowFlags_NoScrollbar 0)
		       (if no-scroll-with-mouse ImGuiWindowFlags_NoScrollWithMouse 0)
		       (if no-collapse ImGuiWindowFlags_NoCollapse 0)
		       (if always-auto-resize ImGuiWindowFlags_AlwaysAutoResize 0)
		       (if no-background ImGuiWindowFlags_NoBackground 0)
		       (if no-saved-settings ImGuiWindowFlags_NoSavedSettings 0)
		       (if no-mouse-inputs ImGuiWindowFlags_NoMouseInputs 0)
		       (if menu-bar ImGuiWindowFlags_MenuBar 0)
		       (if horizontal-scrollbar ImGuiWindowFlags_HorizontalScrollbar 0)
		       (if no-focus-on-appearing ImGuiWindowFlags_NoFocusOnAppearing 0)
		       (if no-bring-to-front-on-focus ImGuiWindowFlags_NoBringToFrontOnFocus 0)
		       (if always-vertical-scrollbar ImGuiWindowFlags_AlwaysVerticalScrollbar 0)
		       (if always-horizontal-scrollbar ImGuiWindowFlags_AlwaysHorizontalScrollbar 0)
		       (if always-use-window-padding ImGuiWindowFlags_AlwaysUseWindowPadding 0)
		       (if no-nav-inputs ImGuiWindowFlags_NoNavInputs 0)
		       (if no-nav-focus ImGuiWindowFlags_NoNavFocus 0)
		       (if unsaved-document ImGuiWindowFlags_UnsavedDocument 0)
		       (if no-nav ImGuiWindowFlags_NoNav 0)
		       (if no-decoration ImGuiWindowFlags_NoDecoration 0)
		       (if no-inputs ImGuiWindowFlags_NoInputs 0)
		       (if nav-flattened ImGuiWindowFlags_NavFlattened 0)
		       (if child-window ImGuiWindowFlags_ChildWindow 0)
		       (if tooltip ImGuiWindowFlags_Tooltip 0)
		       (if popup ImGuiWindowFlags_Popup 0)
		       (if modal ImGuiWindowFlags_Modal 0)
		       (if child-menu ImGuiWindowFlags_ChildMenu 0))))
    
  (with-foreign-object (p-size '(:struct ImVec2_Simple))
    (setf (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::x) (coerce dx 'single-float)
	  (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::y) (coerce dy 'single-float))

    (etypecase id
      (string (igBeginChild id p-size border-p flags))
      (integer (igBeginChildID id p-size border-p flags)))))) 

(defun push-id (id)
  (etypecase id
    (string (igPushIDStr id))
    (integer (igPushIDInt id))
    (sb-sys:system-area-pointer (igPushIdPtr id))))

(defun pop-id ()
  (igPopID))

(defun color-edit (label color
		   &key no-alpha
		     no-picker
		     no-options
		     no-small-preview
		     no-inputs
		     no-tooltip
		     no-label
		     no-side-preview
		     no-drag-drop
		     alpha
		     alpha-preview
		     alpha-preview-half
		     HDR
		     RGB
		     HSV
		     HEX
		     uint8
		     float
		     picker-hue-bar
		     picker-hue-wheel
		     inputs-mask
		     data-type-mask
		     picker-mask
		     options-default)
  (unless color
    (setq color (make-array 4 :element-type 'single-float
			    :initial-element 1.0f0)))
  (assert (typep color '(array single-float)))
  (sb-sys:with-pinned-objects (color)
    (let ((fn (if (eq 3 (length color))
		  #'igColorEdit3
		  (if (> (length color) 3)
		      #'igColorEdit4
		      (error "invalid color array size: ~S" (length color))))))
      (funcall fn label (sb-sys:vector-sap color)
	       (logior (if no-alpha ImGuiColorEditFlags_NoAlpha 0)
		       (if no-picker ImGuiColorEditFlags_NoPicker 0)
		       (if no-options ImGuiColorEditFlags_NoOptions 0)
		       (if no-small-preview ImGuiColorEditFlags_NoSmallPreview 0)
		       (if no-inputs ImGuiColorEditFlags_NoInputs 0)
		       (if no-tooltip ImGuiColorEditFlags_NoTooltip 0)
		       (if no-label ImGuiColorEditFlags_NoLabel 0)
		       (if no-side-preview ImGuiColorEditFlags_NoSidePreview 0)
		       (if no-drag-drop ImGuiColorEditFlags_NoDragDrop 0)
		       (if alpha ImGuiColorEditFlags_AlphaBar 0)
		       (if alpha-preview ImGuiColorEditFlags_AlphaPreview 0)
		       (if alpha-preview-half ImGuiColorEditFlags_AlphaPreviewHalf 0)
		       (if HDR ImGuiColorEditFlags_HDR 0)
		       (if RGB ImGuiColorEditFlags_RGB 0)
		       (if HSV ImGuiColorEditFlags_HSV 0)
		       (if HEX ImGuiColorEditFlags_HEX 0)
		       (if uint8 ImGuiColorEditFlags_Uint8 0)
		       (if float ImGuiColorEditFlags_Float 0)
		       (if picker-hue-bar ImGuiColorEditFlags_PickerHueBar 0)
		       (if picker-hue-wheel ImGuiColorEditFlags_PickerHueWheel 0)
		       (if inputs-mask (logior ImGuiColorEditFlags_RGB
					       ImGuiColorEditFlags_HSV
					       ImGuiColorEditFlags_HEX) 0)
		       (if data-type-mask (logior ImGuiColorEditFlags_Uint8
						  ImGuiColorEditFlags_Float) 0)
		       (if picker-mask (logior ImGuiColorEditFlags_PickerHueWheel
					       ImGuiColorEditFlags_PickerHueBar) 0)
		       (if options-default (logior ImGuiColorEditFlags_Uint8
						   ImGuiColorEditFlags_RGB
						   ImGuiColorEditFlags_PickerHueBar) 0)))))
  color)
  
(defun button (label &key (dx 100.0f0) (dy 20.0f0))
  (with-foreign-object (p-size '(:struct ImVec2_Simple))
    (setf (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::x) (coerce dx 'single-float)
	  (foreign-slot-value p-size '(:struct ImVec2_Simple) 'ig::y) (coerce dy 'single-float))
    (igButton label p-size)))

(defclass imgui-font (vk::handle-mixin)
  ((pathname :initarg :pathname :reader font-pathname)
   (size :initarg :size :reader font-size)
   (plist :initarg :plist :accessor font-plist)))

(defun get-font ()
  (make-instance 'imgui-font :handle (igGetFont)))

(defmethod print-object ((object imgui-font) stream)
  (print-unreadable-object (object stream :identity nil)
    (princ "FONT " stream)
    (princ (ImFont_GetDebugName (vk:h object)) stream)))

(defun render-text (text &key
			   (font
			    (eval
			     (read-from-string
			      "(rgn::find-font rgn::*app* :pathname \"ProggyClean\")")))
			   (x 0.0f0) (y 0.0f0)
			   (color #(1 1 1 1))
			   (num-bytes nil)
			   (size (slot-value font 'size))
			   (start 0)
			   (end (length text)))
  (let ((r (elt color 0))
	(g (elt color 1))
	(b (elt color 2))
	(a (elt color 3)))
    
    (with-foreign-string (p-text text)
      (igRenderText (slot-value font 'vk::handle)
		    (coerce size 'single-float)
		    (coerce x 'single-float)
		    (coerce y 'single-float)
		    (coerce r 'single-float)
		    (coerce g 'single-float)
		    (coerce b 'single-float)
		    (coerce a 'single-float)
		    p-text
		    (or num-bytes
			(cffi:inc-pointer
			 p-text (foreign-funcall "strlen" :pointer p-text :int)))
		    (round start)
		    (round end)))))

(defun add-rect (p-draw-list p-min-x p-min-y p-max-x p-max-y col rounding rounding-flags thickness)
  (ImDrawList_AddRect p-draw-list
		      (coerce p-min-x 'single-float) (coerce p-min-y 'single-float)
		      (coerce p-max-x 'single-float) (coerce p-max-y 'single-float)
		      (igGetColorU32Vec4
		       (coerce (elt col 0) 'single-float)
		       (coerce (elt col 1) 'single-float)
		       (coerce (elt col 2) 'single-float)
		       (coerce (elt col 3) 'single-float))
		      (coerce rounding 'single-float) rounding-flags
		      (coerce thickness 'single-float)))

(defun middle-mouse-down? (app)
  (declare (ignore app))
  (igIsMouseDown 2))

(defun left-mouse-down? (app)
  (declare (ignore app))
  (igIsMouseDown 0))

(defun right-mouse-down? (app)
  (declare (ignore app))
  (igIsMouseDown 1))

(defun middle-mouse-released? (app)
  (declare (ignore app))
  (igIsMouseReleased 2))

(defun left-mouse-released? (app)
  (declare (ignore app))
  (igIsMouseReleased 0))

(defun right-mouse-released? (app)
  (declare (ignore app))
  (igIsMouseReleased 1))

(defun middle-mouse-clicked? (app &optional (repeat nil))
  (declare (ignore app))
  (igIsMouseClicked 2 repeat))

(defun left-mouse-clicked? (app &optional (repeat nil))
  (declare (ignore app))
  (igIsMouseClicked 0 repeat))

(defun right-mouse-clicked? (app &optional (repeat nil))
  (declare (ignore app))
  (igIsMouseClicked 1 repeat))

(defun middle-mouse-double-clicked? (app)
  (declare (ignore app))
  (igIsMouseDoubleClicked 2))

(defun left-mouse-double-clicked? (app)
  (declare (ignore app))
  (igIsMouseDoubleClicked 0))

(defun right-mouse-double-clicked? (app)
  (declare (ignore app))
  (igIsMouseDoubleClicked 1))

				 
