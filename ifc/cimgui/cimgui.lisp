;; Copyright 2019 Andrew Kenneth Wolven <awolven@gmail.com>
;; 
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(cl:in-package :ig)

(cffi:defctype ImGuiContext :pointer)

(cffi:defctype ImTextureID :pointer)

(cffi:defctype ImGuiID :unsigned-int)

(cffi:defctype ImWchar :unsigned-short)

(cffi:defctype ImGuiCol :int)

(cffi:defctype ImGuiCond :int)

(cffi:defctype ImGuiDataType :int)

(cffi:defctype ImGuiDir :int)

(cffi:defctype ImGuiKey :int)

(cffi:defctype ImGuiNavInput :int)

(cffi:defctype ImGuiMouseCursor :int)

(cffi:defctype ImGuiStyleVar :int)

(cffi:defctype ImDrawCornerFlags :int)

(cffi:defctype ImDrawListFlags :int)

(cffi:defctype ImFontAtlasFlags :int)

(cffi:defctype ImGuiBackendFlags :int)

(cffi:defctype ImGuiColorEditFlags :int)

(cffi:defctype ImGuiColumnsFlags :int)

(cffi:defctype ImGuiConfigFlags :int)

(cffi:defctype ImGuiComboFlags :int)

(cffi:defctype ImGuiDragDropFlags :int)

(cffi:defctype ImGuiFocusedFlags :int)

(cffi:defctype ImGuiHoveredFlags :int)

(cffi:defctype ImGuiInputTextFlags :int)

(cffi:defctype ImGuiSelectableFlags :int)

(cffi:defctype ImGuiTabBarFlags :int)

(cffi:defctype ImGuiTabItemFlags :int)

(cffi:defctype ImGuiTreeNodeFlags :int)

(cffi:defctype ImGuiWindowFlags :int)

(cffi:defctype ImGuiInputTextCallback :pointer)

(cffi:defctype ImGuiSizeCallback :pointer)

(cffi:defctype ImS32 :int)

(cffi:defctype ImU32 :unsigned-int)

(cffi:defctype ImS64 :int64)

(cffi:defctype ImU64 :uint64)

(cffi:defctype ImDrawCallback :pointer)

(cffi:defctype ImDrawIdx :unsigned-short)


(cffi:defcenum ImGuiWindowFlags_
	(ImGuiWindowFlags_NoTitleBar #.(cl:ash 1 0))
	(ImGuiWindowFlags_NoResize #.(cl:ash 1 1))
	(ImGuiWindowFlags_NoMove #.(cl:ash 1 2))
	(ImGuiWindowFlags_NoScrollbar #.(cl:ash 1 3))
	(ImGuiWindowFlags_NoScrollWithMouse #.(cl:ash 1 4))
	(ImGuiWindowFlags_NoCollapse #.(cl:ash 1 5))
	(ImGuiWindowFlags_AlwaysAutoResize #.(cl:ash 1 6))
	(ImGuiWindowFlags_NoBackground #.(cl:ash 1 7))
	(ImGuiWindowFlags_NoSavedSettings #.(cl:ash 1 8))
	(ImGuiWindowFlags_NoMouseInputs #.(cl:ash 1 9))
	(ImGuiWindowFlags_MenuBar #.(cl:ash 1 10))
	(ImGuiWindowFlags_HorizontalScrollbar #.(cl:ash 1 11))
	(ImGuiWindowFlags_NoFocusOnAppearing #.(cl:ash 1 12))
	(ImGuiWindowFlags_NoBringToFrontOnFocus #.(cl:ash 1 13))
	(ImGuiWindowFlags_AlwaysVerticalScrollbar #.(cl:ash 1 14))
	(ImGuiWindowFlags_AlwaysHorizontalScrollbar #.(cl:ash 1 15))
	(ImGuiWindowFlags_AlwaysUseWindowPadding #.(cl:ash 1 16))
	;;(ImGuiWindowFlags_ResizeFromAnySide #.(cl:ash 1 17))
	(ImGuiWindowFlags_NoNavInputs #.(cl:ash 1 18))
	(ImGuiWindowFlags_NoNavFocus #.(cl:ash 1 19))
	(ImGuiWindowFlags_UnsavedDocument #.(cl:ash 1 20))
	(ImGuiWindowFlags_NoNav #.(cl:logior (cl:ash 1 18)
					     (cl:ash 1 19)))
	(ImGuiWindowFlags_NoDecoration #.(cl:logior (cl:ash 1 0)
						    (cl:ash 1 1)
						    (cl:ash 1 3)
						    (cl:ash 1 5)))
	(ImGuiWindowFlags_NoInputs #.(cl:logior (cl:ash 1 9)
						(cl:ash 1 18)
						(cl:ash 1 19)))
	(ImGuiWindowFlags_NavFlattened #.(cl:ash 1 23))
	(ImGuiWindowFlags_ChildWindow #.(cl:ash 1 24))
	(ImGuiWindowFlags_Tooltip #.(cl:ash 1 25))
	(ImGuiWindowFlags_Popup #.(cl:ash 1 26))
	(ImGuiWindowFlags_Modal #.(cl:ash 1 27))
	(ImGuiWindowFlags_ChildMenu #.(cl:ash 1 28)))
	
						
(cffi:defcenum ImGuiInputTextFlags_
	(ImGuiInputTextFlags_CharsDecimal #.(cl:ash 1 0))
	(ImGuiInputTextFlags_CharsHexadecimal #.(cl:ash 1 1))
	(ImGuiInputTextFlags_CharsUppercase #.(cl:ash 1 2))
	(ImGuiInputTextFlags_CharsNoBlank #.(cl:ash 1 3))
	(ImGuiInputTextFlags_AutoSelectAll #.(cl:ash 1 4))
	(ImGuiInputTextFlags_EnterReturnsTrue #.(cl:ash 1 5))
	(ImGuiInputTextFlags_CallbackCompletion #.(cl:ash 1 6))
	(ImGuiInputTextFlags_CallbackHistory #.(cl:ash 1 7))
	(ImGuiInputTextFlags_CallbackAlways #.(cl:ash 1 8))
	(ImGuiInputTextFlags_CallbackCharFilter #.(cl:ash 1 9))
	(ImGuiInputTextFlags_AllowTabInput #.(cl:ash 1 10))
	(ImGuiInputTextFlags_CtrlEnterForNewLine #.(cl:ash 1 11))
	(ImGuiInputTextFlags_NoHorizontalScroll #.(cl:ash 1 12))
	(ImGuiInputTextFlags_AlwaysInsertMode #.(cl:ash 1 13))
	(ImGuiInputTextFlags_ReadOnly #.(cl:ash 1 14))
	(ImGuiInputTextFlags_Password #.(cl:ash 1 15))
	(ImGuiInputTextFlags_NoUndoRedo #.(cl:ash 1 16)))

(cffi:defcenum ImGuiTreeNodeFlags_
	(ImGuiTreeNodeFlags_Selected #.(cl:ash 1 0))
	(ImGuiTreeNodeFlags_Framed #.(cl:ash 1 1))
	(ImGuiTreeNodeFlags_AllowItemOverlap #.(cl:ash 1 2))
	(ImGuiTreeNodeFlags_NoTreePushOnOpen #.(cl:ash 1 3))
	(ImGuiTreeNodeFlags_NoAutoOpenOnLog #.(cl:ash 1 4))
	(ImGuiTreeNodeFlags_DefaultOpen #.(cl:ash 1 5))
	(ImGuiTreeNodeFlags_OpenOnDoubleClick #.(cl:ash 1 6))
	(ImGuiTreeNodeFlags_OpenOnArrow #.(cl:ash 1 7))
	(ImGuiTreeNodeFlags_Leaf #.(cl:ash 1 8))
	(ImGuiTreeNodeFlags_Bullet #.(cl:ash 1 9))
	(ImGuiTreeNodeFlags_FramePadding #.(cl:ash 1 10))
	(ImGuiTreeNodeFlags_NavLeftJumpsBackHere #.(cl:ash 1 13))
	(ImGuiTreeNodeFlags_CollapsingHeader #.(cl:logior (cl:ash 1 1) (cl:ash 1 4))))

(cffi:defcenum ImGuiSelectableFlags_
	(ImGuiSelectableFlags_DontClosePopups #.(cl:ash 1 0))
	(ImGuiSelectableFlags_SpanAllColumns #.(cl:ash 1 1))
	(ImGuiSelectableFlags_AllowDoubleClick #.(cl:ash 1 2)))

(cffi:defcenum ImGuiComboFlags_
	(ImGuiComboFlags_PopupAlignLeft #.(cl:ash 1 0))
	(ImGuiComboFlags_HeightSmall #.(cl:ash 1 1))
	(ImGuiComboFlags_HeightRegular #.(cl:ash 1 2))
	(ImGuiComboFlags_HeightLarge #.(cl:ash 1 3))
	(ImGuiComboFlags_HeightLargest #.(cl:ash 1 4))
	(ImGuiComboFlags_HeightMask_ #.(cl:logior (cl:ash 1 1) (cl:ash 1 2) (cl:ash 1 3) (cl:ash 1 4))))

(cffi:defcenum ImGuiFocusedFlags_
	(ImGuiFocusedFlags_ChildWindows #.(cl:ash 1 0))
	(ImGuiFocusedFlags_RootWindow #.(cl:ash 1 1))
	(ImGuiFocusedFlags_RootAndChildWindows #.(cl:logior (cl:ash 1 1) (cl:ash 1 0))))

(cffi:defcenum ImGuiHoveredFlags_
	(ImGuiHoveredFlags_ChildWindows #.(cl:ash 1 0))
	(ImGuiHoveredFlags_RootWindow #.(cl:ash 1 1))
	(ImGuiHoveredFlags_AllowWhenBlockedByPopup #.(cl:ash 1 2))
	(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem #.(cl:ash 1 4))
	(ImGuiHoveredFlags_AllowWhenOverlapped #.(cl:ash 1 5))
	(ImGuiHoveredFlags_RectOnly #.(cl:logior (cl:ash 1 2) (cl:ash 1 4) (cl:ash 1 5)))
	(ImGuiHoveredFlags_RootAndChildWindows #.(cl:logior (cl:ash 1 1) (cl:ash 1 0))))

(cffi:defcenum ImGuiDragDropFlags_
	(ImGuiDragDropFlags_SourceNoPreviewTooltip #.(cl:ash 1 0))
	(ImGuiDragDropFlags_SourceNoDisableHover #.(cl:ash 1 1))
	(ImGuiDragDropFlags_SourceNoHoldToOpenOthers #.(cl:ash 1 2))
	(ImGuiDragDropFlags_SourceAllowNullID #.(cl:ash 1 3))
	(ImGuiDragDropFlags_SourceExtern #.(cl:ash 1 4))
	(ImGuiDragDropFlags_AcceptBeforeDelivery #.(cl:ash 1 10))
	(ImGuiDragDropFlags_AcceptNoDrawDefaultRect #.(cl:ash 1 11))
	(ImGuiDragDropFlags_AcceptPeekOnly #.(cl:logior (cl:ash 1 10) (cl:ash 1 11))))

(cffi:defcenum ImGuiKey_
	ImGuiKey_Tab
	ImGuiKey_LeftArrow
	ImGuiKey_RightArrow
	ImGuiKey_UpArrow
	ImGuiKey_DownArrow
	ImGuiKey_PageUp
	ImGuiKey_PageDown
	ImGuiKey_Home
	ImGuiKey_End
	ImGuiKey_Insert
	ImGuiKey_Delete
	ImGuiKey_Backspace
	ImGuiKey_Space
	ImGuiKey_Enter
	ImGuiKey_Escape
	ImGuiKey_KeyPadEnter
	ImGuiKey_A
	ImGuiKey_C
	ImGuiKey_V
	ImGuiKey_X
	ImGuiKey_Y
	ImGuiKey_Z
	ImGuiKey_COUNT)

(cffi:defcenum ImGuiNavInput_

    ImGuiNavInput_Activate
    ImGuiNavInput_Cancel
    ImGuiNavInput_Input
    ImGuiNavInput_Menu
    ImGuiNavInput_DpadLeft
    ImGuiNavInput_DpadRight
    ImGuiNavInput_DpadUp
    ImGuiNavInput_DpadDown
    ImGuiNavInput_LStickLeft
    ImGuiNavInput_LStickRight
    ImGuiNavInput_LStickUp
    ImGuiNavInput_LStickDown
    ImGuiNavInput_FocusPrev
    ImGuiNavInput_FocusNext
    ImGuiNavInput_TweakSlow
    ImGuiNavInput_TweakFast
    ImGuiNavInput_KeyMenu_
    ImGuiNavInput_KeyTab_
    ImGuiNavInput_KeyLeft_
    ImGuiNavInput_KeyRight_
    ImGuiNavInput_KeyUp_
    ImGuiNavInput_KeyDown_
    ImGuiNavInput_COUNT
    (ImGuiNavInput_InternalStart_ 16))

(cffi:defcenum ImGuiCol_
	ImGuiCol_Text
	ImGuiCol_TextDisabled
	ImGuiCol_WindowBg
	ImGuiCol_ChildBg
	ImGuiCol_PopupBg
	ImGuiCol_Border
	ImGuiCol_BorderShadow
	ImGuiCol_FrameBg
	ImGuiCol_FrameBgHovered
	ImGuiCol_FrameBgActive
	ImGuiCol_TitleBg
	ImGuiCol_TitleBgActive
	ImGuiCol_TitleBgCollapsed
	ImGuiCol_MenuBarBg
	ImGuiCol_ScrollbarBg
	ImGuiCol_ScrollbarGrab
	ImGuiCol_ScrollbarGrabHovered
	ImGuiCol_ScrollbarGrabActive
	ImGuiCol_CheckMark
	ImGuiCol_SliderGrab
	ImGuiCol_SliderGrabActive
	ImGuiCol_Button
	ImGuiCol_ButtonHovered
	ImGuiCol_ButtonActive
	ImGuiCol_Header
	ImGuiCol_HeaderHovered
	ImGuiCol_HeaderActive
	ImGuiCol_Separator
	ImGuiCol_SeparatorHovered
	ImGuiCol_SeparatorActive
	ImGuiCol_ResizeGrip
	ImGuiCol_ResizeGripHovered
	ImGuiCol_ResizeGripActive
	ImGuiCol_CloseButton
	ImGuiCol_CloseButtonHovered
	ImGuiCol_CloseButtonActive
	ImGuiCol_PlotLines
	ImGuiCol_PlotLinesHovered
	ImGuiCol_PlotHistogram
	ImGuiCol_PlotHistogramHovered
	ImGuiCol_TextSelectedBg
	ImGuiCol_ModalWindowDarkening
	ImGuiCol_DragDropTarget
	ImGuiCol_COUNT)

(cffi:defcenum ImGuiStyleVar_
	ImGuiStyleVar_Alpha
	ImGuiStyleVar_WindowPadding
	ImGuiStyleVar_WindowRounding
	ImGuiStyleVar_WindowBorderSize
	ImGuiStyleVar_WindowMinSize
	ImGuiStyleVar_ChildRounding
	ImGuiStyleVar_ChildBorderSize
	ImGuiStyleVar_PopupRounding
	ImGuiStyleVar_PopupBorderSize
	ImGuiStyleVar_FramePadding
	ImGuiStyleVar_FrameRounding
	ImGuiStyleVar_FrameBorderSize
	ImGuiStyleVar_ItemSpacing
	ImGuiStyleVar_ItemInnerSpacing
	ImGuiStyleVar_IndentSpacing
	ImGuiStyleVar_GrabMinSize
	ImGuiStyleVar_ButtonTextAlign
	ImGuiStyleVar_Count_)

(cffi:defcenum ImGuiColorEditFlags_
	(ImGuiColorEditFlags_NoAlpha #.(cl:ash 1 1))
	(ImGuiColorEditFlags_NoPicker #.(cl:ash 1 2))
	(ImGuiColorEditFlags_NoOptions #.(cl:ash 1 3))
	(ImGuiColorEditFlags_NoSmallPreview #.(cl:ash 1 4))
	(ImGuiColorEditFlags_NoInputs #.(cl:ash 1 5))
	(ImGuiColorEditFlags_NoTooltip #.(cl:ash 1 6))
	(ImGuiColorEditFlags_NoLabel #.(cl:ash 1 7))
	(ImGuiColorEditFlags_NoSidePreview #.(cl:ash 1 8))
	(ImGuiColorEditFlags_NoDragDrop #.(cl:ash 1 9))
	(ImGuiColorEditFlags_AlphaBar #.(cl:ash 1 16))
	(ImGuiColorEditFlags_AlphaPreview #.(cl:ash 1 17))
	(ImGuiColorEditFlags_AlphaPreviewHalf #.(cl:ash 1 18))
	(ImGuiColorEditFlags_HDR #.(cl:ash 1 19))
	(ImGuiColorEditFlags_RGB #.(cl:ash 1 20))
	(ImGuiColorEditFlags_HSV #.(cl:ash 1 21))
	(ImGuiColorEditFlags_HEX #.(cl:ash 1 22))
	(ImGuiColorEditFlags_Uint8 #.(cl:ash 1 23))
	(ImGuiColorEditFlags_Float #.(cl:ash 1 24))
	(ImGuiColorEditFlags_PickerHueBar #.(cl:ash 1 25))
	(ImGuiColorEditFlags_PickerHueWheel #.(cl:ash 1 26)))

(cffi:defcenum ImGuiMouseCursor_
	(ImGuiMouseCursor_None #.-1)
	(ImGuiMouseCursor_Arrow #.0)
	ImGuiMouseCursor_TextInput
	ImGuiMouseCursor_Move
	ImGuiMouseCursor_ResizeNS
	ImGuiMouseCursor_ResizeEW
	ImGuiMouseCursor_ResizeNESW
	ImGuiMouseCursor_ResizeNWSE
	ImGuiMouseCursor_Count_)

(cffi:defcenum ImGuiCond_
	(ImGuiCond_Always #.(cl:ash 1 0))
	(ImGuiCond_Once #.(cl:ash 1 1))
	(ImGuiCond_FirstUseEver #.(cl:ash 1 2))
	(ImGuiCond_Appearing #.(cl:ash 1 3)))


(cffi:defcstruct ImVec2_Simple
	(x :float)
	(y :float))

(cffi:defcstruct ImVec4_Simple
	(x :float)
	(y :float)
	(z :float)
	(w :float))

(cffi:defcstruct ImColor_Simple
	(Value (:struct ImVec4_Simple)))

(cffi:defcstruct ImVector
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_float
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImWchar
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImFontConfig
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImFontGlyph
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_TextRange
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_CustomRect
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImDrawChannel
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_char
	(Size :int)
	(Capacity :int)
	(Data :string))

(cffi:defcstruct ImVector_ImTextureID
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImDrawVert
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_int
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_Pair
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImFontPtr
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImVec4
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImDrawCmd
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImDrawIdx
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVector_ImVec2
	(Size :int)
	(Capacity :int)
	(Data :pointer))

(cffi:defcstruct ImVec2
	(x :float)
	(y :float))

(cffi:defcstruct ImVec4
	(x :float)
	(y :float)
	(z :float)
	(w :float))

(cffi:defcstruct ImGuiStyle
	(Alpha :float)
	(WindowPadding (:struct ImVec2))
	(WindowRounding :float)
	(WindowBorderSize :float)
	(WindowMinSize (:struct ImVec2))
	(WindowTitleAlign (:struct ImVec2))
	(ChildRounding :float)
	(ChildBorderSize :float)
	(PopupRounding :float)
	(PopupBorderSize :float)
	(FramePadding (:struct ImVec2))
	(FrameRounding :float)
	(FrameBorderSize :float)
	(ItemSpacing (:struct ImVec2))
	(ItemInnerSpacing (:struct ImVec2))
	(TouchExtraPadding (:struct ImVec2))
	(IndentSpacing :float)
	(ColumnsMinSpacing :float)
	(ScrollbarSize :float)
	(ScrollbarRounding :float)
	(GrabMinSize :float)
	(GrabRounding :float)
	(TabRounding :float)
	(TabBorderSize :float)
	(ButtonTextAlign (:struct ImVec2))
	(SelectableTextAlign (:struct ImVec2))
	(DisplayWindowPadding (:struct ImVec2))
	(DisplaySafeAreaPadding (:struct ImVec2))
	(MouseCursorScale :float)
	(AntiAliasedLines :bool)
	(AntiAliasedFill :bool)
	(CurveTessellationTol :float)
	(Colors (:struct ImVec4) :count #.ImGuiCol_COUNT))

(cffi:defcstruct ImGuiIO
	(ConfigFlags ImGuiConfigFlags)
	(BackendFlags ImGuiBackendFlags)
	(DisplaySize (:struct ImVec2))
	(DeltaTime :float)
	(IniSavingRate :float)
	(IniFilename :string)
	(LogFilename :string)
	(MouseDoubleClickTime :float)
	(MouseDoubleClickMaxDist :float)
	(MouseDragThreshold :float)
	(KeyMap :int :count #.ImGuiKey_COUNT)
	(KeyRepeatDelay :float)
	(KeyRepeatRate :float)
	(UserData :pointer)
	(Fonts :pointer)
	(FontGlobalScale :float)
	(FontAllowUserScaling :bool)
	(FontDefault :pointer)
	(DisplayFramebufferScale (:struct ImVec2))
	(MouseDrawCursor :bool)
	(ConfigMacOSXBehaviors :bool)
	(ConfigInputTextCursorBlink :bool)
	(ConfigWindowsResizeFromEdges :bool)
	(ConfigWindowsMoveFromTitleBarOnly :bool)
	(BackendPlatformName :string)
	(BackendRendererName :string)
	(BackendPlatformUserData :pointer)
	(BackendRendererUserData :pointer)
	(BackendLanguageUserData :pointer)
	(GetClipboardTextFn :pointer)
	(SetClipboardTextFn :pointer)
	(ClipboardUserData :pointer)
	(ImeSetInputScreenPosFn :pointer)
	(ImeWindowHandle :pointer)
	(RenderDrawListsFnUnused :pointer)
	(MousePos (:struct ImVec2))
	(MouseDown :bool :count 5)
	(MouseWheel :float)
	(MouseWheelH :float)
	(KeyCtrl :bool)
	(KeyShift :bool)
	(KeyAlt :bool)
	(KeySuper :bool)
	(KeysDown :bool :count 512)
	(NavInputs :float :count #.ImGuiNavInput_COUNT)
	(WantCaptureMouse :bool)
	(WantCaptureKeyboard :bool)
	(WantTextInput :bool)
	(WantSetMousePos :bool)
	(WantSaveIniSettings :bool)
	(NavActive :bool)
	(NavVisible :bool)
	(Framerate :float)
	(MetricsRenderVertices :int)
	(MetricsRenderIndices :int)
	(MetricsRenderWindows :int)
	(MetricsActiveWindows :int)
	(MetricsActiveAllocations :int)
	(MouseDelta (:struct ImVec2))
	(MousePosPrev (:struct ImVec2))
	(MouseClickedPos (:struct ImVec2) :count 5)
	(MouseClickedTime :double :count 5)
	(MouseClicked :bool :count 5)
	(MouseDoubleClicked :bool :count 5)
	(MouseReleased :bool :count 5)
	(MouseDownOwned :bool :count 5)
	(MouseDownDuration :float :count 5)
	(MouseDownDurationPrev :float :count 5)
	(MouseDragMaxDistanceAbs (:struct ImVec2) :count 5)
	(MouseDragMaxDistanceSqr :float :count 5)
	(KeysDownDuration :float :count 512)
	(KeysDownDurationPrev :float :count 512)
	(NavInputsDownDuration :float :count #.ImGuiNavInput_COUNT)
	(NavInputsDownDurationPrev :float :count #.ImGuiNavInput_COUNT)
	(InputQueueCharacters (:struct ImVector_ImWchar)))

(cffi:defcstruct ImGuiInputTextCallbackData
	(EventFlag :int)
	(Flags :int)
	(UserData :pointer)
	(EventChar :unsigned-short)
	(EventKey :int)
	(Buf :string)
	(BufTextLen :int)
	(BufSize :int)
	(BufDirty :bool)
	(CursorPos :int)
	(SelectionStart :int)
	(SelectionEnd :int))

(cffi:defcstruct ImGuiSizeCallbackData
	(UserData :pointer)
	(Pos (:struct ImVec2))
	(CurrentSize (:struct ImVec2))
	(DesiredSize (:struct ImVec2)))

(cffi:defcstruct ImGuiPayload
	(Data :pointer)
	(DataSize :int)
	(SourceId :unsigned-int)
	(SourceParentId :unsigned-int)
	(DataFrameCount :int)
	(DataType :char :count 33)
	(Preview :bool)
	(Delivery :bool))

(cffi:defcstruct ImGuiOnceUponAFrame
	(RefFrame :int))

(cffi:defcstruct ImGuiTextFilter
	(InputBuf :char :count 256)
	(Filters (:struct ImVector_TextRange))
	(CountGrep :int))

(cffi:defcstruct ImGuiTextBuffer
	(Buf (:struct ImVector_char)))

(cffi:defcstruct ImGuiStorage
	(Data (:struct ImVector_Pair)))

(cffi:defcstruct ImGuiListClipper
	(StartPosY :float)
	(ItemsHeight :float)
	(ItemsCount :int)
	(StepNo :int)
	(DisplayStart :int)
	(DisplayEnd :int))

(cffi:defcstruct ImColor
	(Value (:struct ImVec4)))

(cffi:defcstruct ImDrawCmd
	(ElemCount :unsigned-int)
	(ClipRect (:struct ImVec4))
	(TextureId :pointer)
	(VtxOffset :unsigned-int)
	(IdxOffset :unsigned-int)
	(UserCallback :pointer)
	(UserCallbackData :pointer))

(cffi:defcstruct ImDrawVert
	(pos (:struct ImVec2))
	(uv (:struct ImVec2))
	(col :unsigned-int))

(cffi:defcstruct ImDrawChannel
	(CmdBuffer (:struct ImVector_ImDrawCmd))
	(IdxBuffer (:struct ImVector_ImDrawIdx)))

(cffi:defcenum ImDrawCornerFlags_
	(ImDrawCornerFlags_TopLeft #.(cl:ash 1 0))
	(ImDrawCornerFlags_TopRight #.(cl:ash 1 1))
	(ImDrawCornerFlags_BotLeft #.(cl:ash 1 2))
	(ImDrawCornerFlags_BotRight #.(cl:ash 1 3))
	(ImDrawCornerFlags_Top #.(cl:logior (cl:ash 1 0) (cl:ash 1 1)))
	(ImDrawCornerFlags_Bot #.(cl:logior (cl:ash 1 2) (cl:ash 1 3)))
	(ImDrawCornerFlags_Left #.(cl:logior (cl:ash 1 0) (cl:ash 1 2)))
	(ImDrawCornerFlags_Right #.(cl:logior (cl:ash 1 1) (cl:ash 1 3)))
	(ImDrawCornerFlags_All #.#xF))

(cffi:defcenum ImDrawListFlags_
  (ImDrawListFlags_None 0)
  (ImDrawListFlags_AntiAliasedLines #.(cl:ash 1 0))
  (ImDrawListFlags_AntiAliasedFill #.(cl:ash 1 1)))

(cffi:defcstruct ImDrawList
	(CmdBuffer (:struct ImVector_ImDrawCmd))
	(IdxBuffer (:struct ImVector_ImDrawIdx))
	(VtxBuffer (:struct ImVector_ImDrawVert))
	(Flags :int)
	(_Data :pointer)
	(_OwnerName :string)
	(_VtxCurrentIdx :unsigned-int)
	(_VtxWritePtr :pointer)
	(_IdxWritePtr :pointer)
	(_ClipRectStack (:struct ImVector_ImVec4))
	(_TextureIdStack (:struct ImVector_ImTextureID))
	(_Path (:struct ImVector_ImVec2))
	(_ChannelsCurrent :int)
	(_ChannelsCount :int)
	(_Channels (:struct ImVector_ImDrawChannel)))

(cffi:defcstruct ImDrawData
	(Valid :bool)
	(CmdLists :pointer)
	(CmdListsCount :int)
	(TotalIdxCount :int)
	(TotalVtxCount :int)
	(DisplayPos (:struct ImVec2))
	(DisplaySize (:struct ImVec2))
	(FramebufferScale (:struct ImVec2)))

(cffi:defcstruct ImFontConfig
	(FontData :pointer)
	(FontDataSize :int)
	(FontDataOwnedByAtlas :bool)
	(FontNo :int)
	(SizePixels :float)
	(OversampleH :int)
	(OversampleV :int)
	(PixelSnapH :bool)
	(GlyphExtraSpacing (:struct ImVec2))
	(GlyphOffset (:struct ImVec2))
	(GlyphRanges :pointer)
	(GlyphMinAdvanceX :float)
	(GlyphMaxAdvanceX :float)
	(MergeMode :bool)
	(RasterizerFlags :unsigned-int)
	(RasterizerMultiply :float)
	(Name :char :count 40)
	(DstFont :pointer))

(cffi:defcstruct ImFontGlyph
	(Codepoint :unsigned-short)
	(AdvanceX :float)
	(X0 :float)
	(Y0 :float)
	(X1 :float)
	(Y1 :float)
	(U0 :float)
	(V0 :float)
	(U1 :float)
	(V1 :float))

(cffi:defcstruct ImFontGlyphRangesBuilder
	(UsedChars (:struct ImVector_int)))

(cffi:defcstruct ImGuiTextEditCallbackData
	(EventFlag :int)
	(Flags :int)
	(UserData :pointer)
	(ReadOnly :pointer)
	(EventChar :unsigned-short)
	(EventKey :int)
	(Buf :string)
	(BufTextLen :int)
	(BufSize :int)
	(BufDirty :pointer)
	(CursorPos :int)
	(SelectionStart :int)
	(SelectionEnd :int))

(cffi:defcstruct ImGuiSizeConstraintCallbackData
	(UserData :pointer)
	(Pos (:struct ImVec2))
	(CurrentSize (:struct ImVec2))
	(DesiredSize (:struct ImVec2)))

(cffi:defcstruct ImFontAtlas
	(Locked :bool)
	(Flags :int)
	(TexID :pointer)
	(TexDesiredWidth :int)
	(TexGlyphPadding :int)
	(TexPixelsAlpha8 :pointer)
	(TexPixelsRGBA32 :pointer)
	(TexWidth :int)
	(TexHeight :int)
	(TexUvScale (:struct ImVec2))
	(TexUvWhitePixel (:struct ImVec2))
	(Fonts (:struct ImVector_ImFontPtr))
	(CustomRects (:struct ImVector_CustomRect))
	(ConfigData (:struct ImVector_ImFontConfig))
	(CustomRectIds :int :count 1))

(cffi:defcstruct ImFont
	(IndexAdvanceX (:struct ImVector_float))
	(FallbackAdvanceX :float)
	(FontSize :float)
	(IndexLookup (:struct ImVector_ImWchar))
	(Glyphs (:struct ImVector_ImFontGlyph))
	(FallbackGlyph :pointer)
	(DisplayOffset (:struct ImVec2))
	(ContainerAtlas :pointer)
	(ConfigData :pointer)
	(ConfigDataCount :short)
	(FallbackChar :unsigned-short)
	(Scale :float)
	(Ascent :float)
	(Descent :float)
	(MetricsTotalSurface :int)
	(DirtyLookupTables :bool))

(cffi:defcstruct TextRange
	(b :string)
	(e :string))

(cffi:defcstruct Pair
	(key :unsigned-int))

(cffi:defcstruct CustomRect
	(ID :unsigned-int)
	(Width :unsigned-short)
	(Height :unsigned-short)
	(X :unsigned-short)
	(Y :unsigned-short)
	(GlyphAdvanceX :float)
	(GlyphOffset (:struct ImVec2))
	(Font :pointer))

(cffi:defcenum ImFontAtlasFlags_
	(ImFontAtlasFlags_None #.0)
	(ImFontAtlasFlags_NoPowerOfTwoHeight #.(cl:ash 1 0))
	(ImFontAtlasFlags_NoMouseCursors #.(cl:ash 1 1)))

(cffi:defcfun ("ImVec2_ImVec2" ImVec2_ImVec2) :pointer)

(cffi:defcfun ("ImVec2_destroy" ImVec2_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVec2_ImVec2Float" ImVec2_ImVec2Float) :pointer
  (_x :float)
  (_y :float))

(cffi:defcfun ("ImVec4_ImVec4" ImVec4_ImVec4) :pointer)

(cffi:defcfun ("ImVec4_destroy" ImVec4_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVec4_ImVec4Float" ImVec4_ImVec4Float) :pointer
  (_x :float)
  (_y :float)
  (_z :float)
  (_w :float))

(cffi:defcfun ("igCreateContext" igCreateContext) :pointer
  (shared_font_atlas :pointer))

(cffi:defcfun ("igDestroyContext" igDestroyContext) :void
  (ctx :pointer))

(cffi:defcfun ("igGetCurrentContext" igGetCurrentContext) :pointer)

(cffi:defcfun ("igSetCurrentContext" igSetCurrentContext) :void
  (ctx :pointer))

(cffi:defcfun ("igDebugCheckVersionAndDataLayout" igDebugCheckVersionAndDataLayout) :bool
  (version_str :string)
  (sz_io :pointer)
  (sz_style :pointer)
  (sz_vec2 :pointer)
  (sz_vec4 :pointer)
  (sz_drawvert :pointer))

(cffi:defcfun ("igGetIO" igGetIO) :pointer)

(cffi:defcfun ("igGetStyle" igGetStyle) :pointer)

(cffi:defcfun ("igNewFrame" igNewFrame) :void)

(cffi:defcfun ("igEndFrame" igEndFrame) :void)

(cffi:defcfun ("igRender" igRender) :void)

(cffi:defcfun ("igGetDrawData" igGetDrawData) :pointer)

(cffi:defcfun ("igShowDemoWindow" igShowDemoWindow) :void
  (p_open :pointer))

(cffi:defcfun ("igShowAboutWindow" igShowAboutWindow) :void
  (p_open :pointer))

(cffi:defcfun ("igShowMetricsWindow" igShowMetricsWindow) :void
  (p_open :pointer))

(cffi:defcfun ("igShowStyleEditor" igShowStyleEditor) :void
  (ref :pointer))

(cffi:defcfun ("igShowStyleSelector" igShowStyleSelector) :bool
  (label :string))

(cffi:defcfun ("igShowFontSelector" igShowFontSelector) :void
  (label :string))

(cffi:defcfun ("igShowUserGuide" igShowUserGuide) :void)

(cffi:defcfun ("igGetVersion" igGetVersion) :string)

(cffi:defcfun ("igStyleColorsDark" igStyleColorsDark) :void
  (dst :pointer))

(cffi:defcfun ("igStyleColorsClassic" igStyleColorsClassic) :void
  (dst :pointer))

(cffi:defcfun ("igStyleColorsLight" igStyleColorsLight) :void
  (dst :pointer))

(cffi:defcfun ("igBegin" igBegin) :bool
  (name :string)
  (p_open :pointer)
  (flags :int))

(cffi:defcfun ("igEnd" igEnd) :void)



(cffi:defcfun ("igBeginChildID" igBeginChildID) :bool
  (id :pointer)
  (size :pointer)
  (border :bool)
  (flags :int))

(cffi:defcfun ("igEndChild" igEndChild) :void)

(cffi:defcfun ("igIsWindowAppearing" igIsWindowAppearing) :bool)

(cffi:defcfun ("igIsWindowCollapsed" igIsWindowCollapsed) :bool)

(cffi:defcfun ("igIsWindowFocused" igIsWindowFocused) :bool
  (flags :int))

(cffi:defcfun ("igIsWindowHovered" igIsWindowHovered) :bool
  (flags :int))

(cffi:defcfun ("igGetWindowDrawList" igGetWindowDrawList) :pointer)



(cffi:defcfun ("igGetWindowWidth" igGetWindowWidth) :float)

(cffi:defcfun ("igGetWindowHeight" igGetWindowHeight) :float)



(cffi:defcfun ("igGetContentRegionAvailWidth" igGetContentRegionAvailWidth) :float)



(cffi:defcfun ("igGetWindowContentRegionWidth" igGetWindowContentRegionWidth) :float)



(cffi:defcfun ("igSetNextWindowCollapsed" igSetNextWindowCollapsed) :void
  (collapsed :bool)
  (cond :pointer))

(cffi:defcfun ("igSetNextWindowFocus" igSetNextWindowFocus) :void)

(cffi:defcfun ("igSetNextWindowBgAlpha" igSetNextWindowBgAlpha) :void
  (alpha :float))



(cffi:defcfun ("igSetWindowCollapsedBool" igSetWindowCollapsedBool) :void
  (collapsed :bool)
  (cond :pointer))

(cffi:defcfun ("igSetWindowFocus" igSetWindowFocus) :void)

(cffi:defcfun ("igSetWindowFontScale" igSetWindowFontScale) :void
  (scale :float))



(cffi:defcfun ("igSetWindowCollapsedStr" igSetWindowCollapsedStr) :void
  (name :string)
  (collapsed :bool)
  (cond :pointer))

(cffi:defcfun ("igSetWindowFocusStr" igSetWindowFocusStr) :void
  (name :string))

(cffi:defcfun ("igGetScrollX" igGetScrollX) :float)

(cffi:defcfun ("igGetScrollY" igGetScrollY) :float)

(cffi:defcfun ("igGetScrollMaxX" igGetScrollMaxX) :float)

(cffi:defcfun ("igGetScrollMaxY" igGetScrollMaxY) :float)

(cffi:defcfun ("igSetScrollX" igSetScrollX) :void
  (scroll_x :float))

(cffi:defcfun ("igSetScrollY" igSetScrollY) :void
  (scroll_y :float))

(cffi:defcfun ("igSetScrollHereY" igSetScrollHereY) :void
  (center_y_ratio :float))

(cffi:defcfun ("igSetScrollFromPosY" igSetScrollFromPosY) :void
  (local_y :float)
  (center_y_ratio :float))

(cffi:defcfun ("igPushFont" igPushFont) :void
  (font :pointer))

(cffi:defcfun ("igPopFont" igPopFont) :void)

(cffi:defcfun ("igPushStyleColorU32" igPushStyleColorU32) :void
  (idx :pointer)
  (col :pointer))



(cffi:defcfun ("igPopStyleColor" igPopStyleColor) :void
  (count :int))

(cffi:defcfun ("igPushStyleVarFloat" igPushStyleVarFloat) :void
  (idx :pointer)
  (val :float))



(cffi:defcfun ("igPopStyleVar" igPopStyleVar) :void
  (count :int))

(cffi:defcfun ("igGetStyleColorVec4" igGetStyleColorVec4) :pointer
  (idx :pointer))

(cffi:defcfun ("igGetFont" igGetFont) :pointer)

(cffi:defcfun ("igGetFontSize" igGetFontSize) :float)


(cffi:defcfun ("igGetColorU32" igGetColorU32) :pointer
  (idx :pointer)
  (alpha_mul :float))

(cffi:defcfun ("igGetColorU32Vec4" igGetColorU32Vec4) :pointer
  (col :pointer))

(cffi:defcfun ("igGetColorU32U32" igGetColorU32U32) :pointer
  (col :pointer))

(cffi:defcfun ("igPushItemWidth" igPushItemWidth) :void
  (item_width :float))

(cffi:defcfun ("igPopItemWidth" igPopItemWidth) :void)

(cffi:defcfun ("igCalcItemWidth" igCalcItemWidth) :float)

(cffi:defcfun ("igPushTextWrapPos" igPushTextWrapPos) :void
  (wrap_local_pos_x :float))

(cffi:defcfun ("igPopTextWrapPos" igPopTextWrapPos) :void)

(cffi:defcfun ("igPushAllowKeyboardFocus" igPushAllowKeyboardFocus) :void
  (allow_keyboard_focus :bool))

(cffi:defcfun ("igPopAllowKeyboardFocus" igPopAllowKeyboardFocus) :void)

(cffi:defcfun ("igPushButtonRepeat" igPushButtonRepeat) :void
  (repeat :bool))

(cffi:defcfun ("igPopButtonRepeat" igPopButtonRepeat) :void)

(cffi:defcfun ("igSeparator" igSeparator) :void)

(cffi:defcfun ("igSameLine" igSameLine) :void
  (local_pos_x :float)
  (spacing_w :float))

(cffi:defcfun ("igNewLine" igNewLine) :void)

(cffi:defcfun ("igSpacing" igSpacing) :void)

(cffi:defcfun ("igDummy" igDummy) :void
  (size :pointer))

(cffi:defcfun ("igIndent" igIndent) :void
  (indent_w :float))

(cffi:defcfun ("igUnindent" igUnindent) :void
  (indent_w :float))

(cffi:defcfun ("igBeginGroup" igBeginGroup) :void)

(cffi:defcfun ("igEndGroup" igEndGroup) :void)



(cffi:defcfun ("igGetCursorPosX" igGetCursorPosX) :float)

(cffi:defcfun ("igGetCursorPosY" igGetCursorPosY) :float)

(cffi:defcfun ("igSetCursorPosX" igSetCursorPosX) :void
  (local_x :float))

(cffi:defcfun ("igSetCursorPosY" igSetCursorPosY) :void
  (local_y :float))







(cffi:defcfun ("igAlignTextToFramePadding" igAlignTextToFramePadding) :void)

(cffi:defcfun ("igGetTextLineHeight" igGetTextLineHeight) :float)

(cffi:defcfun ("igGetTextLineHeightWithSpacing" igGetTextLineHeightWithSpacing) :float)

(cffi:defcfun ("igGetFrameHeight" igGetFrameHeight) :float)

(cffi:defcfun ("igGetFrameHeightWithSpacing" igGetFrameHeightWithSpacing) :float)

(cffi:defcfun ("igPushIDStr" igPushIDStr) :void
  (str_id :string))

(cffi:defcfun ("igPushIDRange" igPushIDRange) :void
  (str_id_begin :string)
  (str_id_end :string))

(cffi:defcfun ("igPushIDPtr" igPushIDPtr) :void
  (ptr_id :pointer))

(cffi:defcfun ("igPushIDInt" igPushIDInt) :void
  (int_id :int))

(cffi:defcfun ("igPopID" igPopID) :void)

(cffi:defcfun ("igGetIDStr" igGetIDStr) :pointer
  (str_id :string))

(cffi:defcfun ("igGetIDRange" igGetIDRange) :pointer
  (str_id_begin :string)
  (str_id_end :string))

(cffi:defcfun ("igGetIDPtr" igGetIDPtr) :pointer
  (ptr_id :pointer))

(cffi:defcfun ("igTextUnformatted" igTextUnformatted) :void
  (text :string)
  (text_end :string))

(cffi:defcfun ("igText" igText) :void
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTextV" igTextV) :void
  (fmt :string)
  (args :pointer))



(cffi:defcfun ("igTextDisabled" igTextDisabled) :void
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTextDisabledV" igTextDisabledV) :void
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igTextWrapped" igTextWrapped) :void
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTextWrappedV" igTextWrappedV) :void
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igLabelText" igLabelText) :void
  (label :string)
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igLabelTextV" igLabelTextV) :void
  (label :string)
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igBulletText" igBulletText) :void
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igBulletTextV" igBulletTextV) :void
  (fmt :string)
  (args :pointer))



(cffi:defcfun ("igSmallButton" igSmallButton) :bool
  (label :string))



(cffi:defcfun ("igArrowButton" igArrowButton) :bool
  (str_id :string)
  (dir :pointer))





(cffi:defcfun ("igCheckbox" igCheckbox) :bool
  (label :string)
  (v :pointer))

(cffi:defcfun ("igCheckboxFlags" igCheckboxFlags) :bool
  (label :string)
  (flags :pointer)
  (flags_value :unsigned-int))

(cffi:defcfun ("igRadioButtonBool" igRadioButtonBool) :bool
  (label :string)
  (active :bool))

(cffi:defcfun ("igRadioButtonIntPtr" igRadioButtonIntPtr) :bool
  (label :string)
  (v :pointer)
  (v_button :int))

(cffi:defcfun ("igProgressBar" igProgressBar) :void
  (fraction :float)
  (size_arg :pointer)
  (overlay :string))

(cffi:defcfun ("igBullet" igBullet) :void)

(cffi:defcfun ("igBeginCombo" igBeginCombo) :bool
  (label :string)
  (preview_value :string)
  (flags :int))

(cffi:defcfun ("igEndCombo" igEndCombo) :void)

(cffi:defcfun ("igCombo" igCombo) :bool
  (label :string)
  (current_item :pointer)
  (items :pointer)
  (items_count :int)
  (popup_max_height_in_items :int))

(cffi:defcfun ("igComboStr" igComboStr) :bool
  (label :string)
  (current_item :pointer)
  (items_separated_by_zeros :string)
  (popup_max_height_in_items :int))

(cffi:defcfun ("igComboFnPtr" igComboFnPtr) :bool
  (label :string)
  (current_item :pointer)
  (items_getter :pointer)
  (data :pointer)
  (items_count :int)
  (popup_max_height_in_items :int))

(cffi:defcfun ("igDragFloat" igDragFloat) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igDragFloat2" igDragFloat2) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igDragFloat3" igDragFloat3) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igDragFloat4" igDragFloat4) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igDragFloatRange2" igDragFloatRange2) :bool
  (label :string)
  (v_current_min :pointer)
  (v_current_max :pointer)
  (v_speed :float)
  (v_min :float)
  (v_max :float)
  (format :string)
  (format_max :string)
  (power :float))

(cffi:defcfun ("igDragInt" igDragInt) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igDragInt2" igDragInt2) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igDragInt3" igDragInt3) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igDragInt4" igDragInt4) :bool
  (label :string)
  (v :pointer)
  (v_speed :float)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igDragIntRange2" igDragIntRange2) :bool
  (label :string)
  (v_current_min :pointer)
  (v_current_max :pointer)
  (v_speed :float)
  (v_min :int)
  (v_max :int)
  (format :string)
  (format_max :string))

(cffi:defcfun ("igDragScalar" igDragScalar) :bool
  (label :string)
  (data_type :pointer)
  (v :pointer)
  (v_speed :float)
  (v_min :pointer)
  (v_max :pointer)
  (format :string)
  (power :float))

(cffi:defcfun ("igDragScalarN" igDragScalarN) :bool
  (label :string)
  (data_type :pointer)
  (v :pointer)
  (components :int)
  (v_speed :float)
  (v_min :pointer)
  (v_max :pointer)
  (format :string)
  (power :float))

(cffi:defcfun ("igSliderFloat" igSliderFloat) :bool
  (label :string)
  (v :pointer)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igSliderFloat2" igSliderFloat2) :bool
  (label :string)
  (v :pointer)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igSliderFloat3" igSliderFloat3) :bool
  (label :string)
  (v :pointer)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igSliderFloat4" igSliderFloat4) :bool
  (label :string)
  (v :pointer)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("igSliderAngle" igSliderAngle) :bool
  (label :string)
  (v_rad :pointer)
  (v_degrees_min :float)
  (v_degrees_max :float)
  (format :string))

(cffi:defcfun ("igSliderInt" igSliderInt) :bool
  (label :string)
  (v :pointer)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igSliderInt2" igSliderInt2) :bool
  (label :string)
  (v :pointer)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igSliderInt3" igSliderInt3) :bool
  (label :string)
  (v :pointer)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igSliderInt4" igSliderInt4) :bool
  (label :string)
  (v :pointer)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("igSliderScalar" igSliderScalar) :bool
  (label :string)
  (data_type :pointer)
  (v :pointer)
  (v_min :pointer)
  (v_max :pointer)
  (format :string)
  (power :float))

(cffi:defcfun ("igSliderScalarN" igSliderScalarN) :bool
  (label :string)
  (data_type :pointer)
  (v :pointer)
  (components :int)
  (v_min :pointer)
  (v_max :pointer)
  (format :string)
  (power :float))

(cffi:defcfun ("igVSliderScalar" igVSliderScalar) :bool
  (label :string)
  (size :pointer)
  (data_type :pointer)
  (v :pointer)
  (v_min :pointer)
  (v_max :pointer)
  (format :string)
  (power :float))

(cffi:defcfun ("igInputText" igInputText) :bool
  (label :string)
  (buf :string)
  (buf_size :pointer)
  (flags :int)
  (callback :pointer)
  (user_data :pointer))

(cffi:defcfun ("igInputFloat" igInputFloat) :bool
  (label :string)
  (v :pointer)
  (step :float)
  (step_fast :float)
  (format :string)
  (flags :int))

(cffi:defcfun ("igInputFloat2" igInputFloat2) :bool
  (label :string)
  (v :pointer)
  (format :string)
  (flags :int))

(cffi:defcfun ("igInputFloat3" igInputFloat3) :bool
  (label :string)
  (v :pointer)
  (format :string)
  (flags :int))

(cffi:defcfun ("igInputFloat4" igInputFloat4) :bool
  (label :string)
  (v :pointer)
  (format :string)
  (flags :int))

(cffi:defcfun ("igInputInt" igInputInt) :bool
  (label :string)
  (v :pointer)
  (step :int)
  (step_fast :int)
  (flags :int))

(cffi:defcfun ("igInputInt2" igInputInt2) :bool
  (label :string)
  (v :pointer)
  (flags :int))

(cffi:defcfun ("igInputInt3" igInputInt3) :bool
  (label :string)
  (v :pointer)
  (flags :int))

(cffi:defcfun ("igInputInt4" igInputInt4) :bool
  (label :string)
  (v :pointer)
  (flags :int))

(cffi:defcfun ("igInputDouble" igInputDouble) :bool
  (label :string)
  (v :pointer)
  (step :double)
  (step_fast :double)
  (format :string)
  (flags :int))

(cffi:defcfun ("igInputScalar" igInputScalar) :bool
  (label :string)
  (data_type :pointer)
  (v :pointer)
  (step :pointer)
  (step_fast :pointer)
  (format :string)
  (flags :int))

(cffi:defcfun ("igInputScalarN" igInputScalarN) :bool
  (label :string)
  (data_type :pointer)
  (v :pointer)
  (components :int)
  (step :pointer)
  (step_fast :pointer)
  (format :string)
  (flags :int))

(cffi:defcfun ("igColorEdit3" igColorEdit3) :bool
  (label :string)
  (col :pointer)
  (flags :int))

(cffi:defcfun ("igColorEdit4" igColorEdit4) :bool
  (label :string)
  (col :pointer)
  (flags :int))

(cffi:defcfun ("igColorPicker3" igColorPicker3) :bool
  (label :string)
  (col :pointer)
  (flags :int))

(cffi:defcfun ("igColorPicker4" igColorPicker4) :bool
  (label :string)
  (col :pointer)
  (flags :int)
  (ref_col :pointer))



(cffi:defcfun ("igSetColorEditOptions" igSetColorEditOptions) :void
  (flags :int))

(cffi:defcfun ("igTreeNodeStr" igTreeNodeStr) :bool
  (label :string))

(cffi:defcfun ("igTreeNodeStrStr" igTreeNodeStrStr) :bool
  (str_id :string)
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTreeNodePtr" igTreeNodePtr) :bool
  (ptr_id :pointer)
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTreeNodeVStr" igTreeNodeVStr) :bool
  (str_id :string)
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igTreeNodeVPtr" igTreeNodeVPtr) :bool
  (ptr_id :pointer)
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igTreeNodeExStr" igTreeNodeExStr) :bool
  (label :string)
  (flags :int))

(cffi:defcfun ("igTreeNodeExStrStr" igTreeNodeExStrStr) :bool
  (str_id :string)
  (flags :int)
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTreeNodeExPtr" igTreeNodeExPtr) :bool
  (ptr_id :pointer)
  (flags :int)
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igTreeNodeExVStr" igTreeNodeExVStr) :bool
  (str_id :string)
  (flags :int)
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igTreeNodeExVPtr" igTreeNodeExVPtr) :bool
  (ptr_id :pointer)
  (flags :int)
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igTreePushStr" igTreePushStr) :void
  (str_id :string))

(cffi:defcfun ("igTreePushPtr" igTreePushPtr) :void
  (ptr_id :pointer))

(cffi:defcfun ("igTreePop" igTreePop) :void)

(cffi:defcfun ("igTreeAdvanceToLabelPos" igTreeAdvanceToLabelPos) :void)

(cffi:defcfun ("igGetTreeNodeToLabelSpacing" igGetTreeNodeToLabelSpacing) :float)

(cffi:defcfun ("igSetNextTreeNodeOpen" igSetNextTreeNodeOpen) :void
  (is_open :bool)
  (cond :pointer))

(cffi:defcfun ("igCollapsingHeader" igCollapsingHeader) :bool
  (label :string)
  (flags :int))

(cffi:defcfun ("igCollapsingHeaderBoolPtr" igCollapsingHeaderBoolPtr) :bool
  (label :string)
  (p_open :pointer)
  (flags :int))





(cffi:defcfun ("igListBoxStr_arr" igListBoxStr_arr) :bool
  (label :string)
  (current_item :pointer)
  (items :pointer)
  (items_count :int)
  (height_in_items :int))

(cffi:defcfun ("igListBoxFnPtr" igListBoxFnPtr) :bool
  (label :string)
  (current_item :pointer)
  (items_getter :pointer)
  (data :pointer)
  (items_count :int)
  (height_in_items :int))



(cffi:defcfun ("igListBoxHeaderInt" igListBoxHeaderInt) :bool
  (label :string)
  (items_count :int)
  (height_in_items :int))

(cffi:defcfun ("igListBoxFooter" igListBoxFooter) :void)



(cffi:defcfun ("igValueBool" igValueBool) :void
  (prefix :string)
  (b :bool))

(cffi:defcfun ("igValueInt" igValueInt) :void
  (prefix :string)
  (v :int))

(cffi:defcfun ("igValueUint" igValueUint) :void
  (prefix :string)
  (v :unsigned-int))

(cffi:defcfun ("igValueFloat" igValueFloat) :void
  (prefix :string)
  (v :float)
  (float_format :string))

(cffi:defcfun ("igBeginMainMenuBar" igBeginMainMenuBar) :bool)

(cffi:defcfun ("igEndMainMenuBar" igEndMainMenuBar) :void)

(cffi:defcfun ("igBeginMenuBar" igBeginMenuBar) :bool)

(cffi:defcfun ("igEndMenuBar" igEndMenuBar) :void)

(cffi:defcfun ("igBeginMenu" igBeginMenu) :bool
  (label :string)
  (enabled :bool))

(cffi:defcfun ("igEndMenu" igEndMenu) :void)

(cffi:defcfun ("igMenuItemBool" igMenuItemBool) :bool
  (label :string)
  (shortcut :string)
  (selected :bool)
  (enabled :bool))

(cffi:defcfun ("igMenuItemBoolPtr" igMenuItemBoolPtr) :bool
  (label :string)
  (shortcut :string)
  (p_selected :pointer)
  (enabled :bool))

(cffi:defcfun ("igBeginTooltip" igBeginTooltip) :void)

(cffi:defcfun ("igEndTooltip" igEndTooltip) :void)

(cffi:defcfun ("igSetTooltip" igSetTooltip) :void
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igSetTooltipV" igSetTooltipV) :void
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("igOpenPopup" igOpenPopup) :void
  (str_id :string))

(cffi:defcfun ("igBeginPopup" igBeginPopup) :bool
  (str_id :string)
  (flags :int))

(cffi:defcfun ("igBeginPopupContextItem" igBeginPopupContextItem) :bool
  (str_id :string)
  (mouse_button :int))

(cffi:defcfun ("igBeginPopupContextWindow" igBeginPopupContextWindow) :bool
  (str_id :string)
  (mouse_button :int)
  (also_over_items :pointer))

(cffi:defcfun ("igBeginPopupContextVoid" igBeginPopupContextVoid) :bool
  (str_id :string)
  (mouse_button :int))

(cffi:defcfun ("igBeginPopupModal" igBeginPopupModal) :bool
  (name :string)
  (p_open :pointer)
  (flags :int))

(cffi:defcfun ("igEndPopup" igEndPopup) :void)

(cffi:defcfun ("igOpenPopupOnItemClick" igOpenPopupOnItemClick) :bool
  (str_id :string)
  (mouse_button :int))

(cffi:defcfun ("igIsPopupOpen" igIsPopupOpen) :bool
  (str_id :string))

(cffi:defcfun ("igCloseCurrentPopup" igCloseCurrentPopup) :void)

(cffi:defcfun ("igColumns" igColumns) :void
  (count :int)
  (id :string)
  (border :bool))

(cffi:defcfun ("igNextColumn" igNextColumn) :void)

(cffi:defcfun ("igGetColumnIndex" igGetColumnIndex) :int)

(cffi:defcfun ("igGetColumnWidth" igGetColumnWidth) :float
  (column_index :int))

(cffi:defcfun ("igSetColumnWidth" igSetColumnWidth) :void
  (column_index :int)
  (width :float))

(cffi:defcfun ("igGetColumnOffset" igGetColumnOffset) :float
  (column_index :int))

(cffi:defcfun ("igSetColumnOffset" igSetColumnOffset) :void
  (column_index :int)
  (offset_x :float))

(cffi:defcfun ("igGetColumnsCount" igGetColumnsCount) :int)

(cffi:defcfun ("igBeginTabBar" igBeginTabBar) :bool
  (str_id :string)
  (flags :int))

(cffi:defcfun ("igEndTabBar" igEndTabBar) :void)

(cffi:defcfun ("igBeginTabItem" igBeginTabItem) :bool
  (label :string)
  (p_open :pointer)
  (flags :int))

(cffi:defcfun ("igEndTabItem" igEndTabItem) :void)

(cffi:defcfun ("igSetTabItemClosed" igSetTabItemClosed) :void
  (tab_or_docked_window_label :string))

(cffi:defcfun ("igLogToTTY" igLogToTTY) :void
  (max_depth :int))

(cffi:defcfun ("igLogToFile" igLogToFile) :void
  (max_depth :int)
  (filename :string))

(cffi:defcfun ("igLogToClipboard" igLogToClipboard) :void
  (max_depth :int))

(cffi:defcfun ("igLogFinish" igLogFinish) :void)

(cffi:defcfun ("igLogButtons" igLogButtons) :void)

(cffi:defcfun ("igBeginDragDropSource" igBeginDragDropSource) :bool
  (flags :int))

(cffi:defcfun ("igSetDragDropPayload" igSetDragDropPayload) :bool
  (type :string)
  (data :pointer)
  (size :pointer)
  (cond :pointer))

(cffi:defcfun ("igEndDragDropSource" igEndDragDropSource) :void)

(cffi:defcfun ("igBeginDragDropTarget" igBeginDragDropTarget) :bool)

(cffi:defcfun ("igAcceptDragDropPayload" igAcceptDragDropPayload) :pointer
  (type :string)
  (flags :int))

(cffi:defcfun ("igEndDragDropTarget" igEndDragDropTarget) :void)

(cffi:defcfun ("igGetDragDropPayload" igGetDragDropPayload) :pointer)



(cffi:defcfun ("igPopClipRect" igPopClipRect) :void)

(cffi:defcfun ("igSetItemDefaultFocus" igSetItemDefaultFocus) :void)

(cffi:defcfun ("igSetKeyboardFocusHere" igSetKeyboardFocusHere) :void
  (offset :int))

(cffi:defcfun ("igIsItemHovered" igIsItemHovered) :bool
  (flags :int))

(cffi:defcfun ("igIsItemActive" igIsItemActive) :bool)

(cffi:defcfun ("igIsItemFocused" igIsItemFocused) :bool)

(cffi:defcfun ("igIsItemClicked" igIsItemClicked) :bool
  (mouse_button :int))

(cffi:defcfun ("igIsItemVisible" igIsItemVisible) :bool)

(cffi:defcfun ("igIsItemEdited" igIsItemEdited) :bool)

(cffi:defcfun ("igIsItemActivated" igIsItemActivated) :bool)

(cffi:defcfun ("igIsItemDeactivated" igIsItemDeactivated) :bool)

(cffi:defcfun ("igIsItemDeactivatedAfterEdit" igIsItemDeactivatedAfterEdit) :bool)

(cffi:defcfun ("igIsAnyItemHovered" igIsAnyItemHovered) :bool)

(cffi:defcfun ("igIsAnyItemActive" igIsAnyItemActive) :bool)

(cffi:defcfun ("igIsAnyItemFocused" igIsAnyItemFocused) :bool)


(cffi:defcfun ("igSetItemAllowOverlap" igSetItemAllowOverlap) :void)



(cffi:defcfun ("igIsRectVisibleVec2" igIsRectVisibleVec2) :bool
  (rect_min :pointer)
  (rect_max :pointer))

(cffi:defcfun ("igGetTime" igGetTime) :double)

(cffi:defcfun ("igGetFrameCount" igGetFrameCount) :int)

(cffi:defcfun ("igGetOverlayDrawList" igGetOverlayDrawList) :pointer)

(cffi:defcfun ("igGetDrawListSharedData" igGetDrawListSharedData) :pointer)

(cffi:defcfun ("igGetStyleColorName" igGetStyleColorName) :string
  (idx :pointer))

(cffi:defcfun ("igSetStateStorage" igSetStateStorage) :void
  (storage :pointer))

(cffi:defcfun ("igGetStateStorage" igGetStateStorage) :pointer)



(cffi:defcfun ("igCalcListClipping" igCalcListClipping) :void
  (items_count :int)
  (items_height :float)
  (out_items_display_start :pointer)
  (out_items_display_end :pointer))



(cffi:defcfun ("igEndChildFrame" igEndChildFrame) :void)





(cffi:defcfun ("igGetKeyIndex" igGetKeyIndex) :int
  (imgui_key :pointer))

(cffi:defcfun ("igIsKeyDown" igIsKeyDown) :bool
  (user_key_index :int))

(cffi:defcfun ("igIsKeyPressed" igIsKeyPressed) :bool
  (user_key_index :int)
  (repeat :bool))

(cffi:defcfun ("igIsKeyReleased" igIsKeyReleased) :bool
  (user_key_index :int))

(cffi:defcfun ("igGetKeyPressedAmount" igGetKeyPressedAmount) :int
  (key_index :int)
  (repeat_delay :float)
  (rate :float))

(cffi:defcfun ("igIsMouseDown" igIsMouseDown) :bool
  (button :int))

(cffi:defcfun ("igIsAnyMouseDown" igIsAnyMouseDown) :bool)

(cffi:defcfun ("igIsMouseClicked" igIsMouseClicked) :bool
  (button :int)
  (repeat :bool))

(cffi:defcfun ("igIsMouseDoubleClicked" igIsMouseDoubleClicked) :bool
  (button :int))

(cffi:defcfun ("igIsMouseReleased" igIsMouseReleased) :bool
  (button :int))

(cffi:defcfun ("igIsMouseDragging" igIsMouseDragging) :bool
  (button :int)
  (lock_threshold :float))



(cffi:defcfun ("igIsMousePosValid" igIsMousePosValid) :bool
  (mouse_pos :pointer))




(cffi:defcfun ("igResetMouseDragDelta" igResetMouseDragDelta) :void
  (button :int))

(cffi:defcfun ("igGetMouseCursor" igGetMouseCursor) :pointer)

(cffi:defcfun ("igSetMouseCursor" igSetMouseCursor) :void
  (type :pointer))

(cffi:defcfun ("igCaptureKeyboardFromApp" igCaptureKeyboardFromApp) :void
  (want_capture_keyboard_value :bool))

(cffi:defcfun ("igCaptureMouseFromApp" igCaptureMouseFromApp) :void
  (want_capture_mouse_value :bool))

(cffi:defcfun ("igGetClipboardText" igGetClipboardText) :string)

(cffi:defcfun ("igSetClipboardText" igSetClipboardText) :void
  (text :string))

(cffi:defcfun ("igLoadIniSettingsFromDisk" igLoadIniSettingsFromDisk) :void
  (ini_filename :string))

(cffi:defcfun ("igLoadIniSettingsFromMemory" igLoadIniSettingsFromMemory) :void
  (ini_data :string)
  (ini_size :pointer))

(cffi:defcfun ("igSaveIniSettingsToDisk" igSaveIniSettingsToDisk) :void
  (ini_filename :string))

(cffi:defcfun ("igSaveIniSettingsToMemory" igSaveIniSettingsToMemory) :string
  (out_ini_size :pointer))

(cffi:defcfun ("igSetAllocatorFunctions" igSetAllocatorFunctions) :void
  (alloc_func :pointer)
  (free_func :pointer)
  (user_data :pointer))

(cffi:defcfun ("igMemAlloc" igMemAlloc) :pointer
  (size :pointer))

(cffi:defcfun ("igMemFree" igMemFree) :void
  (ptr :pointer))

(cffi:defcfun ("ImGuiStyle_ImGuiStyle" ImGuiStyle_ImGuiStyle) :pointer)

(cffi:defcfun ("ImGuiStyle_destroy" ImGuiStyle_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiStyle_ScaleAllSizes" ImGuiStyle_ScaleAllSizes) :void
  (self :pointer)
  (scale_factor :float))

(cffi:defcfun ("ImGuiIO_AddInputCharacter" ImGuiIO_AddInputCharacter) :void
  (self :pointer)
  (c :unsigned-short))

(cffi:defcfun ("ImGuiIO_AddInputCharactersUTF8" ImGuiIO_AddInputCharactersUTF8) :void
  (self :pointer)
  (str :string))

(cffi:defcfun ("ImGuiIO_ClearInputCharacters" ImGuiIO_ClearInputCharacters) :void
  (self :pointer))

(cffi:defcfun ("ImGuiIO_ImGuiIO" ImGuiIO_ImGuiIO) :pointer)

(cffi:defcfun ("ImGuiIO_destroy" ImGuiIO_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiInputTextCallbackData_ImGuiInputTextCallbackData" ImGuiInputTextCallbackData_ImGuiInputTextCallbackData) :pointer)

(cffi:defcfun ("ImGuiInputTextCallbackData_destroy" ImGuiInputTextCallbackData_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiInputTextCallbackData_DeleteChars" ImGuiInputTextCallbackData_DeleteChars) :void
  (self :pointer)
  (pos :int)
  (bytes_count :int))

(cffi:defcfun ("ImGuiInputTextCallbackData_InsertChars" ImGuiInputTextCallbackData_InsertChars) :void
  (self :pointer)
  (pos :int)
  (text :string)
  (text_end :string))

(cffi:defcfun ("ImGuiInputTextCallbackData_HasSelection" ImGuiInputTextCallbackData_HasSelection) :pointer
  (self :pointer))

(cffi:defcfun ("ImGuiPayload_ImGuiPayload" ImGuiPayload_ImGuiPayload) :pointer)

(cffi:defcfun ("ImGuiPayload_destroy" ImGuiPayload_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiPayload_Clear" ImGuiPayload_Clear) :void
  (self :pointer))

(cffi:defcfun ("ImGuiPayload_IsDataType" ImGuiPayload_IsDataType) :pointer
  (self :pointer)
  (type :string))

(cffi:defcfun ("ImGuiPayload_IsPreview" ImGuiPayload_IsPreview) :pointer
  (self :pointer))

(cffi:defcfun ("ImGuiPayload_IsDelivery" ImGuiPayload_IsDelivery) :pointer
  (self :pointer))

(cffi:defcfun ("ImGuiOnceUponAFrame_ImGuiOnceUponAFrame" ImGuiOnceUponAFrame_ImGuiOnceUponAFrame) :pointer)

(cffi:defcfun ("ImGuiOnceUponAFrame_destroy" ImGuiOnceUponAFrame_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiTextFilter_ImGuiTextFilter" ImGuiTextFilter_ImGuiTextFilter) :pointer
  (default_filter :string))

(cffi:defcfun ("ImGuiTextFilter_destroy" ImGuiTextFilter_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiTextFilter_Draw" ImGuiTextFilter_Draw) :pointer
  (self :pointer)
  (label :string)
  (width :float))

(cffi:defcfun ("ImGuiTextFilter_PassFilter" ImGuiTextFilter_PassFilter) :pointer
  (self :pointer)
  (text :string)
  (text_end :string))

(cffi:defcfun ("ImGuiTextFilter_Build" ImGuiTextFilter_Build) :void
  (self :pointer))

(cffi:defcfun ("ImGuiTextFilter_Clear" ImGuiTextFilter_Clear) :void
  (self :pointer))

(cffi:defcfun ("ImGuiTextFilter_IsActive" ImGuiTextFilter_IsActive) :pointer
  (self :pointer))

(cffi:defcfun ("TextRange_TextRange" TextRange_TextRange) :pointer)

(cffi:defcfun ("TextRange_destroy" TextRange_destroy) :void
  (self :pointer))

(cffi:defcfun ("TextRange_TextRangeStr" TextRange_TextRangeStr) :pointer
  (_b :string)
  (_e :string))

(cffi:defcfun ("TextRange_begin" TextRange_begin) :string
  (self :pointer))

(cffi:defcfun ("TextRange_end" TextRange_end) :string
  (self :pointer))

(cffi:defcfun ("TextRange_empty" TextRange_empty) :pointer
  (self :pointer))

(cffi:defcfun ("TextRange_split" TextRange_split) :void
  (self :pointer)
  (separator :char)
  (out :pointer))

(cffi:defcfun ("ImGuiTextBuffer_ImGuiTextBuffer" ImGuiTextBuffer_ImGuiTextBuffer) :pointer)

(cffi:defcfun ("ImGuiTextBuffer_destroy" ImGuiTextBuffer_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_begin" ImGuiTextBuffer_begin) :string
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_end" ImGuiTextBuffer_end) :string
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_size" ImGuiTextBuffer_size) :int
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_empty" ImGuiTextBuffer_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_clear" ImGuiTextBuffer_clear) :void
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_reserve" ImGuiTextBuffer_reserve) :void
  (self :pointer)
  (capacity :int))

(cffi:defcfun ("ImGuiTextBuffer_c_str" ImGuiTextBuffer_c_str) :string
  (self :pointer))

(cffi:defcfun ("ImGuiTextBuffer_append" ImGuiTextBuffer_append) :void
  (self :pointer)
  (str :string)
  (str_end :string))

(cffi:defcfun ("ImGuiTextBuffer_appendfv" ImGuiTextBuffer_appendfv) :void
  (self :pointer)
  (fmt :string)
  (args :pointer))

(cffi:defcfun ("Pair_PairInt" Pair_PairInt) :pointer
  (_key :pointer)
  (_val_i :int))

(cffi:defcfun ("Pair_destroy" Pair_destroy) :void
  (self :pointer))

(cffi:defcfun ("Pair_PairFloat" Pair_PairFloat) :pointer
  (_key :pointer)
  (_val_f :float))

(cffi:defcfun ("Pair_PairPtr" Pair_PairPtr) :pointer
  (_key :pointer)
  (_val_p :pointer))

(cffi:defcfun ("ImGuiStorage_Clear" ImGuiStorage_Clear) :void
  (self :pointer))

(cffi:defcfun ("ImGuiStorage_GetInt" ImGuiStorage_GetInt) :int
  (self :pointer)
  (key :pointer)
  (default_val :int))

(cffi:defcfun ("ImGuiStorage_SetInt" ImGuiStorage_SetInt) :void
  (self :pointer)
  (key :pointer)
  (val :int))

(cffi:defcfun ("ImGuiStorage_GetBool" ImGuiStorage_GetBool) :pointer
  (self :pointer)
  (key :pointer)
  (default_val :pointer))

(cffi:defcfun ("ImGuiStorage_SetBool" ImGuiStorage_SetBool) :void
  (self :pointer)
  (key :pointer)
  (val :pointer))

(cffi:defcfun ("ImGuiStorage_GetFloat" ImGuiStorage_GetFloat) :float
  (self :pointer)
  (key :pointer)
  (default_val :float))

(cffi:defcfun ("ImGuiStorage_SetFloat" ImGuiStorage_SetFloat) :void
  (self :pointer)
  (key :pointer)
  (val :float))

(cffi:defcfun ("ImGuiStorage_GetVoidPtr" ImGuiStorage_GetVoidPtr) :pointer
  (self :pointer)
  (key :pointer))

(cffi:defcfun ("ImGuiStorage_SetVoidPtr" ImGuiStorage_SetVoidPtr) :void
  (self :pointer)
  (key :pointer)
  (val :pointer))

(cffi:defcfun ("ImGuiStorage_GetIntRef" ImGuiStorage_GetIntRef) :pointer
  (self :pointer)
  (key :pointer)
  (default_val :int))

(cffi:defcfun ("ImGuiStorage_GetBoolRef" ImGuiStorage_GetBoolRef) :pointer
  (self :pointer)
  (key :pointer)
  (default_val :pointer))

(cffi:defcfun ("ImGuiStorage_GetFloatRef" ImGuiStorage_GetFloatRef) :pointer
  (self :pointer)
  (key :pointer)
  (default_val :float))

(cffi:defcfun ("ImGuiStorage_GetVoidPtrRef" ImGuiStorage_GetVoidPtrRef) :pointer
  (self :pointer)
  (key :pointer)
  (default_val :pointer))

(cffi:defcfun ("ImGuiStorage_SetAllInt" ImGuiStorage_SetAllInt) :void
  (self :pointer)
  (val :int))

(cffi:defcfun ("ImGuiStorage_BuildSortByKey" ImGuiStorage_BuildSortByKey) :void
  (self :pointer))

(cffi:defcfun ("ImGuiListClipper_ImGuiListClipper" ImGuiListClipper_ImGuiListClipper) :pointer
  (items_count :int)
  (items_height :float))

(cffi:defcfun ("ImGuiListClipper_destroy" ImGuiListClipper_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImGuiListClipper_Step" ImGuiListClipper_Step) :pointer
  (self :pointer))

(cffi:defcfun ("ImGuiListClipper_Begin" ImGuiListClipper_Begin) :void
  (self :pointer)
  (items_count :int)
  (items_height :float))

(cffi:defcfun ("ImGuiListClipper_End" ImGuiListClipper_End) :void
  (self :pointer))

(cffi:defcfun ("ImColor_ImColor" ImColor_ImColor) :pointer)

(cffi:defcfun ("ImColor_destroy" ImColor_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImColor_ImColorInt" ImColor_ImColorInt) :pointer
  (r :int)
  (g :int)
  (b :int)
  (a :int))

(cffi:defcfun ("ImColor_ImColorU32" ImColor_ImColorU32) :pointer
  (rgba :pointer))

(cffi:defcfun ("ImColor_ImColorFloat" ImColor_ImColorFloat) :pointer
  (r :float)
  (g :float)
  (b :float)
  (a :float))

(cffi:defcfun ("ImColor_ImColorVec4" ImColor_ImColorVec4) :pointer
  (col :pointer))

(cffi:defcfun ("ImColor_SetHSV" ImColor_SetHSV) :void
  (self :pointer)
  (h :float)
  (s :float)
  (v :float)
  (a :float))



(cffi:defcfun ("ImDrawCmd_ImDrawCmd" ImDrawCmd_ImDrawCmd) :pointer)

(cffi:defcfun ("ImDrawCmd_destroy" ImDrawCmd_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_ImDrawList" ImDrawList_ImDrawList) :pointer
  (shared_data :pointer))

(cffi:defcfun ("ImDrawList_destroy" ImDrawList_destroy) :void
  (self :pointer))



(cffi:defcfun ("ImDrawList_PushClipRectFullScreen" ImDrawList_PushClipRectFullScreen) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_PopClipRect" ImDrawList_PopClipRect) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_PushTextureID" ImDrawList_PushTextureID) :void
  (self :pointer)
  (texture_id :pointer))

(cffi:defcfun ("ImDrawList_PopTextureID" ImDrawList_PopTextureID) :void
  (self :pointer))





(cffi:defcfun ("ImDrawList_AddConvexPolyFilled" ImDrawList_AddConvexPolyFilled) :void
  (self :pointer)
  (points :pointer)
  (num_points :int)
  (col :pointer))

(cffi:defcfun ("ImDrawList_AddBezierCurve" ImDrawList_AddBezierCurve) :void
  (self :pointer)
  (pos0 :pointer)
  (cp0 :pointer)
  (cp1 :pointer)
  (pos1 :pointer)
  (col :pointer)
  (thickness :float)
  (num_segments :int))

(cffi:defcfun ("ImDrawList_PathClear" ImDrawList_PathClear) :void
  (self :pointer))



(cffi:defcfun ("ImDrawList_PathFillConvex" ImDrawList_PathFillConvex) :void
  (self :pointer)
  (col :pointer))

(cffi:defcfun ("ImDrawList_PathStroke" ImDrawList_PathStroke) :void
  (self :pointer)
  (col :pointer)
  (closed :pointer)
  (thickness :float))





(cffi:defcfun ("ImDrawList_ChannelsSplit" ImDrawList_ChannelsSplit) :void
  (self :pointer)
  (channels_count :int))

(cffi:defcfun ("ImDrawList_ChannelsMerge" ImDrawList_ChannelsMerge) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_ChannelsSetCurrent" ImDrawList_ChannelsSetCurrent) :void
  (self :pointer)
  (channel_index :int))

(cffi:defcfun ("ImDrawList_AddCallback" ImDrawList_AddCallback) :void
  (self :pointer)
  (callback :pointer)
  (callback_data :pointer))

(cffi:defcfun ("ImDrawList_AddDrawCmd" ImDrawList_AddDrawCmd) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_CloneOutput" ImDrawList_CloneOutput) :pointer
  (self :pointer))

(cffi:defcfun ("ImDrawList_Clear" ImDrawList_Clear) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_ClearFreeMemory" ImDrawList_ClearFreeMemory) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_PrimReserve" ImDrawList_PrimReserve) :void
  (self :pointer)
  (idx_count :int)
  (vtx_count :int))





(cffi:defcfun ("ImDrawList_PrimWriteIdx" ImDrawList_PrimWriteIdx) :void
  (self :pointer)
  (idx :pointer))



(cffi:defcfun ("ImDrawList_UpdateClipRect" ImDrawList_UpdateClipRect) :void
  (self :pointer))

(cffi:defcfun ("ImDrawList_UpdateTextureID" ImDrawList_UpdateTextureID) :void
  (self :pointer))

(cffi:defcfun ("ImDrawData_ImDrawData" ImDrawData_ImDrawData) :pointer)

(cffi:defcfun ("ImDrawData_destroy" ImDrawData_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImDrawData_Clear" ImDrawData_Clear) :void
  (self :pointer))

(cffi:defcfun ("ImDrawData_DeIndexAllBuffers" ImDrawData_DeIndexAllBuffers) :void
  (self :pointer))



(cffi:defcfun ("ImFontConfig_ImFontConfig" ImFontConfig_ImFontConfig) :pointer)

(cffi:defcfun ("ImFontConfig_destroy" ImFontConfig_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImFontGlyphRangesBuilder_ImFontGlyphRangesBuilder" ImFontGlyphRangesBuilder_ImFontGlyphRangesBuilder) :pointer)

(cffi:defcfun ("ImFontGlyphRangesBuilder_destroy" ImFontGlyphRangesBuilder_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImFontGlyphRangesBuilder_GetBit" ImFontGlyphRangesBuilder_GetBit) :pointer
  (self :pointer)
  (n :int))

(cffi:defcfun ("ImFontGlyphRangesBuilder_SetBit" ImFontGlyphRangesBuilder_SetBit) :void
  (self :pointer)
  (n :int))

(cffi:defcfun ("ImFontGlyphRangesBuilder_AddChar" ImFontGlyphRangesBuilder_AddChar) :void
  (self :pointer)
  (c :pointer))

(cffi:defcfun ("ImFontGlyphRangesBuilder_AddText" ImFontGlyphRangesBuilder_AddText) :void
  (self :pointer)
  (text :string)
  (text_end :string))

(cffi:defcfun ("ImFontGlyphRangesBuilder_AddRanges" ImFontGlyphRangesBuilder_AddRanges) :void
  (self :pointer)
  (ranges :pointer))

(cffi:defcfun ("ImFontGlyphRangesBuilder_BuildRanges" ImFontGlyphRangesBuilder_BuildRanges) :void
  (self :pointer)
  (out_ranges :pointer))

(cffi:defcfun ("ImFontAtlas_ImFontAtlas" ImFontAtlas_ImFontAtlas) :pointer)

(cffi:defcfun ("ImFontAtlas_destroy" ImFontAtlas_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_AddFont" ImFontAtlas_AddFont) :pointer
  (self :pointer)
  (font_cfg :pointer))

(cffi:defcfun ("ImFontAtlas_AddFontDefault" ImFontAtlas_AddFontDefault) :pointer
  (self :pointer)
  (font_cfg :pointer))

(cffi:defcfun ("ImFontAtlas_AddFontFromFileTTF" ImFontAtlas_AddFontFromFileTTF) :pointer
  (self :pointer)
  (filename :string)
  (size_pixels :float)
  (font_cfg :pointer)
  (glyph_ranges :pointer))

(cffi:defcfun ("ImFontAtlas_AddFontFromMemoryTTF" ImFontAtlas_AddFontFromMemoryTTF) :pointer
  (self :pointer)
  (font_data :pointer)
  (font_size :int)
  (size_pixels :float)
  (font_cfg :pointer)
  (glyph_ranges :pointer))

(cffi:defcfun ("ImFontAtlas_AddFontFromMemoryCompressedTTF" ImFontAtlas_AddFontFromMemoryCompressedTTF) :pointer
  (self :pointer)
  (compressed_font_data :pointer)
  (compressed_font_size :int)
  (size_pixels :float)
  (font_cfg :pointer)
  (glyph_ranges :pointer))

(cffi:defcfun ("ImFontAtlas_AddFontFromMemoryCompressedBase85TTF" ImFontAtlas_AddFontFromMemoryCompressedBase85TTF) :pointer
  (self :pointer)
  (compressed_font_data_base85 :string)
  (size_pixels :float)
  (font_cfg :pointer)
  (glyph_ranges :pointer))

(cffi:defcfun ("ImFontAtlas_ClearInputData" ImFontAtlas_ClearInputData) :void
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_ClearTexData" ImFontAtlas_ClearTexData) :void
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_ClearFonts" ImFontAtlas_ClearFonts) :void
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_Clear" ImFontAtlas_Clear) :void
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_Build" ImFontAtlas_Build) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetTexDataAsAlpha8" ImFontAtlas_GetTexDataAsAlpha8) :void
  (self :pointer)
  (out_pixels :pointer)
  (out_width :pointer)
  (out_height :pointer)
  (out_bytes_per_pixel :pointer))

(cffi:defcfun ("ImFontAtlas_GetTexDataAsRGBA32" ImFontAtlas_GetTexDataAsRGBA32) :void
  (self :pointer)
  (out_pixels :pointer)
  (out_width :pointer)
  (out_height :pointer)
  (out_bytes_per_pixel :pointer))

(cffi:defcfun ("ImFontAtlas_IsBuilt" ImFontAtlas_IsBuilt) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_SetTexID" ImFontAtlas_SetTexID) :void
  (self :pointer)
  (id :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesDefault" ImFontAtlas_GetGlyphRangesDefault) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesKorean" ImFontAtlas_GetGlyphRangesKorean) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesJapanese" ImFontAtlas_GetGlyphRangesJapanese) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesChineseFull" ImFontAtlas_GetGlyphRangesChineseFull) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesChineseSimplifiedCommon" ImFontAtlas_GetGlyphRangesChineseSimplifiedCommon) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesCyrillic" ImFontAtlas_GetGlyphRangesCyrillic) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_GetGlyphRangesThai" ImFontAtlas_GetGlyphRangesThai) :pointer
  (self :pointer))

(cffi:defcfun ("CustomRect_CustomRect" CustomRect_CustomRect) :pointer)

(cffi:defcfun ("CustomRect_destroy" CustomRect_destroy) :void
  (self :pointer))

(cffi:defcfun ("CustomRect_IsPacked" CustomRect_IsPacked) :pointer
  (self :pointer))

(cffi:defcfun ("ImFontAtlas_AddCustomRectRegular" ImFontAtlas_AddCustomRectRegular) :int
  (self :pointer)
  (id :unsigned-int)
  (width :int)
  (height :int))

(cffi:defcfun ("ImFontAtlas_AddCustomRectFontGlyph" ImFontAtlas_AddCustomRectFontGlyph) :int
  (self :pointer)
  (font :pointer)
  (id :pointer)
  (width :int)
  (height :int)
  (advance_x :float)
  (offset :pointer))

(cffi:defcfun ("ImFontAtlas_GetCustomRectByIndex" ImFontAtlas_GetCustomRectByIndex) :pointer
  (self :pointer)
  (index :int))

(cffi:defcfun ("ImFontAtlas_CalcCustomRectUV" ImFontAtlas_CalcCustomRectUV) :void
  (self :pointer)
  (rect :pointer)
  (out_uv_min :pointer)
  (out_uv_max :pointer))

(cffi:defcfun ("ImFontAtlas_GetMouseCursorTexData" ImFontAtlas_GetMouseCursorTexData) :pointer
  (self :pointer)
  (cursor :pointer)
  (out_offset :pointer)
  (out_size :pointer)
  (out_uv_border :pointer)
  (out_uv_fill :pointer))

(cffi:defcfun ("ImFont_ImFont" ImFont_ImFont) :pointer)

(cffi:defcfun ("ImFont_destroy" ImFont_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImFont_FindGlyph" ImFont_FindGlyph) :pointer
  (self :pointer)
  (c :pointer))

(cffi:defcfun ("ImFont_FindGlyphNoFallback" ImFont_FindGlyphNoFallback) :pointer
  (self :pointer)
  (c :pointer))

(cffi:defcfun ("ImFont_GetCharAdvance" ImFont_GetCharAdvance) :float
  (self :pointer)
  (c :pointer))

(cffi:defcfun ("ImFont_IsLoaded" ImFont_IsLoaded) :pointer
  (self :pointer))

(cffi:defcfun ("ImFont_GetDebugName" ImFont_GetDebugName) :string
  (self :pointer))



(cffi:defcfun ("ImFont_CalcWordWrapPositionA" ImFont_CalcWordWrapPositionA) :string
  (self :pointer)
  (scale :float)
  (text :string)
  (text_end :string)
  (wrap_width :float))

(cffi:defcfun ("ImFont_RenderChar" ImFont_RenderChar) :void
  (self :pointer)
  (draw_list :pointer)
  (size :float)
  (pos :pointer)
  (col :pointer)
  (c :pointer))

(cffi:defcfun ("ImFont_RenderText" ImFont_RenderText) :void
  (self :pointer)
  (draw_list :pointer)
  (size :float)
  (pos :pointer)
  (col :pointer)
  (clip_rect :pointer)
  (text_begin :string)
  (text_end :string)
  (wrap_width :float)
  (cpu_fine_clip :pointer))

(cffi:defcfun ("ImFont_BuildLookupTable" ImFont_BuildLookupTable) :void
  (self :pointer))

(cffi:defcfun ("ImFont_ClearOutputData" ImFont_ClearOutputData) :void
  (self :pointer))

(cffi:defcfun ("ImFont_GrowIndex" ImFont_GrowIndex) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImFont_AddGlyph" ImFont_AddGlyph) :void
  (self :pointer)
  (c :pointer)
  (x0 :float)
  (y0 :float)
  (x1 :float)
  (y1 :float)
  (u0 :float)
  (v0 :float)
  (u1 :float)
  (v1 :float)
  (advance_x :float))

(cffi:defcfun ("ImFont_AddRemapChar" ImFont_AddRemapChar) :void
  (self :pointer)
  (dst :pointer)
  (src :pointer)
  (overwrite_dst :pointer))

(cffi:defcfun ("ImFont_SetFallbackChar" ImFont_SetFallbackChar) :void
  (self :pointer)
  (c :pointer))

(cffi:defcfun ("igGetWindowPos_nonUDT" igGetWindowPos_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetWindowSize_nonUDT" igGetWindowSize_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetContentRegionMax_nonUDT" igGetContentRegionMax_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetContentRegionAvail_nonUDT" igGetContentRegionAvail_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetWindowContentRegionMin_nonUDT" igGetWindowContentRegionMin_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetWindowContentRegionMax_nonUDT" igGetWindowContentRegionMax_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetFontTexUvWhitePixel_nonUDT" igGetFontTexUvWhitePixel_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetCursorPos_nonUDT" igGetCursorPos_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetCursorStartPos_nonUDT" igGetCursorStartPos_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetCursorScreenPos_nonUDT" igGetCursorScreenPos_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetItemRectMin_nonUDT" igGetItemRectMin_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetItemRectMax_nonUDT" igGetItemRectMax_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetItemRectSize_nonUDT" igGetItemRectSize_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igCalcTextSize_nonUDT" igCalcTextSize_nonUDT) :void
  (pOut :pointer)
  (text :string)
  (text_end :string)
  (hide_text_after_double_hash :bool)
  (wrap_width :float))



(cffi:defcfun ("igColorConvertU32ToFloat4_nonUDT" igColorConvertU32ToFloat4_nonUDT) :void
  (pOut :pointer)
  (in :pointer))



(cffi:defcfun ("igGetMousePos_nonUDT" igGetMousePos_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetMousePosOnOpeningCurrentPopup_nonUDT" igGetMousePosOnOpeningCurrentPopup_nonUDT) :void
  (pOut :pointer))



(cffi:defcfun ("igGetMouseDragDelta_nonUDT" igGetMouseDragDelta_nonUDT) :void
  (pOut :pointer)
  (button :int)
  (lock_threshold :float))



(cffi:defcfun ("ImColor_HSV_nonUDT" ImColor_HSV_nonUDT) :void
  (pOut :pointer)
  (self :pointer)
  (h :float)
  (s :float)
  (v :float)
  (a :float))



(cffi:defcfun ("ImDrawList_GetClipRectMin_nonUDT" ImDrawList_GetClipRectMin_nonUDT) :void
  (pOut :pointer)
  (self :pointer))



(cffi:defcfun ("ImDrawList_GetClipRectMax_nonUDT" ImDrawList_GetClipRectMax_nonUDT) :void
  (pOut :pointer)
  (self :pointer))



(cffi:defcfun ("ImFont_CalcTextSizeA_nonUDT" ImFont_CalcTextSizeA_nonUDT) :void
  (pOut :pointer)
  (self :pointer)
  (size :float)
  (max_width :float)
  (wrap_width :float)
  (text_begin :string)
  (text_end :string)
  (remaining :pointer))



(cffi:defcfun ("ImVector_float_ImVector_float" ImVector_float_ImVector_float) :pointer)

(cffi:defcfun ("ImVector_float_destroy" ImVector_float_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_ImVector_ImWchar" ImVector_ImWchar_ImVector_ImWchar) :pointer)

(cffi:defcfun ("ImVector_ImWchar_destroy" ImVector_ImWchar_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_ImVector_ImFontConfig" ImVector_ImFontConfig_ImVector_ImFontConfig) :pointer)

(cffi:defcfun ("ImVector_ImFontConfig_destroy" ImVector_ImFontConfig_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_ImVector_ImFontGlyph" ImVector_ImFontGlyph_ImVector_ImFontGlyph) :pointer)

(cffi:defcfun ("ImVector_ImFontGlyph_destroy" ImVector_ImFontGlyph_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_ImVector_TextRange" ImVector_TextRange_ImVector_TextRange) :pointer)

(cffi:defcfun ("ImVector_TextRange_destroy" ImVector_TextRange_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_ImVector_CustomRect" ImVector_CustomRect_ImVector_CustomRect) :pointer)

(cffi:defcfun ("ImVector_CustomRect_destroy" ImVector_CustomRect_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_ImVector_ImDrawChannel" ImVector_ImDrawChannel_ImVector_ImDrawChannel) :pointer)

(cffi:defcfun ("ImVector_ImDrawChannel_destroy" ImVector_ImDrawChannel_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_char_ImVector_char" ImVector_char_ImVector_char) :pointer)

(cffi:defcfun ("ImVector_char_destroy" ImVector_char_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_ImVector_ImTextureID" ImVector_ImTextureID_ImVector_ImTextureID) :pointer)

(cffi:defcfun ("ImVector_ImTextureID_destroy" ImVector_ImTextureID_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_ImVector_ImDrawVert" ImVector_ImDrawVert_ImVector_ImDrawVert) :pointer)

(cffi:defcfun ("ImVector_ImDrawVert_destroy" ImVector_ImDrawVert_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_int_ImVector_int" ImVector_int_ImVector_int) :pointer)

(cffi:defcfun ("ImVector_int_destroy" ImVector_int_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_ImVector_Pair" ImVector_Pair_ImVector_Pair) :pointer)

(cffi:defcfun ("ImVector_Pair_destroy" ImVector_Pair_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_ImVector_ImFontPtr" ImVector_ImFontPtr_ImVector_ImFontPtr) :pointer)

(cffi:defcfun ("ImVector_ImFontPtr_destroy" ImVector_ImFontPtr_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_ImVector_ImVec4" ImVector_ImVec4_ImVector_ImVec4) :pointer)

(cffi:defcfun ("ImVector_ImVec4_destroy" ImVector_ImVec4_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_ImVector_ImDrawCmd" ImVector_ImDrawCmd_ImVector_ImDrawCmd) :pointer)

(cffi:defcfun ("ImVector_ImDrawCmd_destroy" ImVector_ImDrawCmd_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_ImVector_ImDrawIdx" ImVector_ImDrawIdx_ImVector_ImDrawIdx) :pointer)

(cffi:defcfun ("ImVector_ImDrawIdx_destroy" ImVector_ImDrawIdx_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_ImVector_ImVec2" ImVector_ImVec2_ImVector_ImVec2) :pointer)

(cffi:defcfun ("ImVector_ImVec2_destroy" ImVector_ImVec2_destroy) :void
  (self :pointer))

(cffi:defcfun ("ImVector_float_ImVector_floatVector" ImVector_float_ImVector_floatVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImWchar_ImVector_ImWcharVector" ImVector_ImWchar_ImVector_ImWcharVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_ImVector_ImFontConfigVector" ImVector_ImFontConfig_ImVector_ImFontConfigVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_ImVector_ImFontGlyphVector" ImVector_ImFontGlyph_ImVector_ImFontGlyphVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_TextRange_ImVector_TextRangeVector" ImVector_TextRange_ImVector_TextRangeVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_CustomRect_ImVector_CustomRectVector" ImVector_CustomRect_ImVector_CustomRectVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_ImVector_ImDrawChannelVector" ImVector_ImDrawChannel_ImVector_ImDrawChannelVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_char_ImVector_charVector" ImVector_char_ImVector_charVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImTextureID_ImVector_ImTextureIDVector" ImVector_ImTextureID_ImVector_ImTextureIDVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_ImVector_ImDrawVertVector" ImVector_ImDrawVert_ImVector_ImDrawVertVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_int_ImVector_intVector" ImVector_int_ImVector_intVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_Pair_ImVector_PairVector" ImVector_Pair_ImVector_PairVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_ImVector_ImFontPtrVector" ImVector_ImFontPtr_ImVector_ImFontPtrVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImVec4_ImVector_ImVec4Vector" ImVector_ImVec4_ImVector_ImVec4Vector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_ImVector_ImDrawCmdVector" ImVector_ImDrawCmd_ImVector_ImDrawCmdVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_ImVector_ImDrawIdxVector" ImVector_ImDrawIdx_ImVector_ImDrawIdxVector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_ImVec2_ImVector_ImVec2Vector" ImVector_ImVec2_ImVector_ImVec2Vector) :pointer
  (src :pointer))

(cffi:defcfun ("ImVector_float_empty" ImVector_float_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_empty" ImVector_ImWchar_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_empty" ImVector_ImFontConfig_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_empty" ImVector_ImFontGlyph_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_empty" ImVector_TextRange_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_empty" ImVector_CustomRect_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_empty" ImVector_ImDrawChannel_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_empty" ImVector_char_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_empty" ImVector_ImTextureID_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_empty" ImVector_ImDrawVert_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_empty" ImVector_int_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_empty" ImVector_Pair_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_empty" ImVector_ImFontPtr_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_empty" ImVector_ImVec4_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_empty" ImVector_ImDrawCmd_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_empty" ImVector_ImDrawIdx_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_empty" ImVector_ImVec2_empty) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_size" ImVector_float_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_size" ImVector_ImWchar_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_size" ImVector_ImFontConfig_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_size" ImVector_ImFontGlyph_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_size" ImVector_TextRange_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_size" ImVector_CustomRect_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_size" ImVector_ImDrawChannel_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_char_size" ImVector_char_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_size" ImVector_ImTextureID_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_size" ImVector_ImDrawVert_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_int_size" ImVector_int_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_size" ImVector_Pair_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_size" ImVector_ImFontPtr_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_size" ImVector_ImVec4_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_size" ImVector_ImDrawCmd_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_size" ImVector_ImDrawIdx_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_size" ImVector_ImVec2_size) :int
  (self :pointer))

(cffi:defcfun ("ImVector_float_size_in_bytes" ImVector_float_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_size_in_bytes" ImVector_ImWchar_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_size_in_bytes" ImVector_ImFontConfig_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_size_in_bytes" ImVector_ImFontGlyph_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_size_in_bytes" ImVector_TextRange_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_size_in_bytes" ImVector_CustomRect_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_size_in_bytes" ImVector_ImDrawChannel_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_char_size_in_bytes" ImVector_char_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_size_in_bytes" ImVector_ImTextureID_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_size_in_bytes" ImVector_ImDrawVert_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_int_size_in_bytes" ImVector_int_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_size_in_bytes" ImVector_Pair_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_size_in_bytes" ImVector_ImFontPtr_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_size_in_bytes" ImVector_ImVec4_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_size_in_bytes" ImVector_ImDrawCmd_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_size_in_bytes" ImVector_ImDrawIdx_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_size_in_bytes" ImVector_ImVec2_size_in_bytes) :int
  (self :pointer))

(cffi:defcfun ("ImVector_float_capacity" ImVector_float_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_capacity" ImVector_ImWchar_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_capacity" ImVector_ImFontConfig_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_capacity" ImVector_ImFontGlyph_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_capacity" ImVector_TextRange_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_capacity" ImVector_CustomRect_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_capacity" ImVector_ImDrawChannel_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_char_capacity" ImVector_char_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_capacity" ImVector_ImTextureID_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_capacity" ImVector_ImDrawVert_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_int_capacity" ImVector_int_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_capacity" ImVector_Pair_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_capacity" ImVector_ImFontPtr_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_capacity" ImVector_ImVec4_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_capacity" ImVector_ImDrawCmd_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_capacity" ImVector_ImDrawIdx_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_capacity" ImVector_ImVec2_capacity) :int
  (self :pointer))

(cffi:defcfun ("ImVector_float_clear" ImVector_float_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_clear" ImVector_ImWchar_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_clear" ImVector_ImFontConfig_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_clear" ImVector_ImFontGlyph_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_clear" ImVector_TextRange_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_clear" ImVector_CustomRect_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_clear" ImVector_ImDrawChannel_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_char_clear" ImVector_char_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_clear" ImVector_ImTextureID_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_clear" ImVector_ImDrawVert_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_int_clear" ImVector_int_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_clear" ImVector_Pair_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_clear" ImVector_ImFontPtr_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_clear" ImVector_ImVec4_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_clear" ImVector_ImDrawCmd_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_clear" ImVector_ImDrawIdx_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_clear" ImVector_ImVec2_clear) :void
  (self :pointer))

(cffi:defcfun ("ImVector_float_begin" ImVector_float_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_begin" ImVector_ImWchar_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_begin" ImVector_ImFontConfig_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_begin" ImVector_ImFontGlyph_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_begin" ImVector_TextRange_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_begin" ImVector_CustomRect_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_begin" ImVector_ImDrawChannel_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_begin" ImVector_char_begin) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_begin" ImVector_ImTextureID_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_begin" ImVector_ImDrawVert_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_begin" ImVector_int_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_begin" ImVector_Pair_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_begin" ImVector_ImFontPtr_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_begin" ImVector_ImVec4_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_begin" ImVector_ImDrawCmd_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_begin" ImVector_ImDrawIdx_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_begin" ImVector_ImVec2_begin) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_begin_const" ImVector_float_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_begin_const" ImVector_ImWchar_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_begin_const" ImVector_ImFontConfig_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_begin_const" ImVector_ImFontGlyph_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_begin_const" ImVector_TextRange_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_begin_const" ImVector_CustomRect_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_begin_const" ImVector_ImDrawChannel_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_begin_const" ImVector_char_begin_const) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_begin_const" ImVector_ImTextureID_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_begin_const" ImVector_ImDrawVert_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_begin_const" ImVector_int_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_begin_const" ImVector_Pair_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_begin_const" ImVector_ImFontPtr_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_begin_const" ImVector_ImVec4_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_begin_const" ImVector_ImDrawCmd_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_begin_const" ImVector_ImDrawIdx_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_begin_const" ImVector_ImVec2_begin_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_end" ImVector_float_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_end" ImVector_ImWchar_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_end" ImVector_ImFontConfig_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_end" ImVector_ImFontGlyph_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_end" ImVector_TextRange_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_end" ImVector_CustomRect_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_end" ImVector_ImDrawChannel_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_end" ImVector_char_end) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_end" ImVector_ImTextureID_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_end" ImVector_ImDrawVert_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_end" ImVector_int_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_end" ImVector_Pair_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_end" ImVector_ImFontPtr_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_end" ImVector_ImVec4_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_end" ImVector_ImDrawCmd_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_end" ImVector_ImDrawIdx_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_end" ImVector_ImVec2_end) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_end_const" ImVector_float_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_end_const" ImVector_ImWchar_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_end_const" ImVector_ImFontConfig_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_end_const" ImVector_ImFontGlyph_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_end_const" ImVector_TextRange_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_end_const" ImVector_CustomRect_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_end_const" ImVector_ImDrawChannel_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_end_const" ImVector_char_end_const) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_end_const" ImVector_ImTextureID_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_end_const" ImVector_ImDrawVert_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_end_const" ImVector_int_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_end_const" ImVector_Pair_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_end_const" ImVector_ImFontPtr_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_end_const" ImVector_ImVec4_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_end_const" ImVector_ImDrawCmd_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_end_const" ImVector_ImDrawIdx_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_end_const" ImVector_ImVec2_end_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_front" ImVector_float_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_front" ImVector_ImWchar_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_front" ImVector_ImFontConfig_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_front" ImVector_ImFontGlyph_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_front" ImVector_TextRange_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_front" ImVector_CustomRect_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_front" ImVector_ImDrawChannel_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_front" ImVector_char_front) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_front" ImVector_ImTextureID_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_front" ImVector_ImDrawVert_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_front" ImVector_int_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_front" ImVector_Pair_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_front" ImVector_ImFontPtr_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_front" ImVector_ImVec4_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_front" ImVector_ImDrawCmd_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_front" ImVector_ImDrawIdx_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_front" ImVector_ImVec2_front) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_front_const" ImVector_float_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_front_const" ImVector_ImWchar_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_front_const" ImVector_ImFontConfig_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_front_const" ImVector_ImFontGlyph_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_front_const" ImVector_TextRange_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_front_const" ImVector_CustomRect_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_front_const" ImVector_ImDrawChannel_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_front_const" ImVector_char_front_const) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_front_const" ImVector_ImTextureID_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_front_const" ImVector_ImDrawVert_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_front_const" ImVector_int_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_front_const" ImVector_Pair_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_front_const" ImVector_ImFontPtr_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_front_const" ImVector_ImVec4_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_front_const" ImVector_ImDrawCmd_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_front_const" ImVector_ImDrawIdx_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_front_const" ImVector_ImVec2_front_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_back" ImVector_float_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_back" ImVector_ImWchar_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_back" ImVector_ImFontConfig_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_back" ImVector_ImFontGlyph_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_back" ImVector_TextRange_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_back" ImVector_CustomRect_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_back" ImVector_ImDrawChannel_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_back" ImVector_char_back) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_back" ImVector_ImTextureID_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_back" ImVector_ImDrawVert_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_back" ImVector_int_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_back" ImVector_Pair_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_back" ImVector_ImFontPtr_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_back" ImVector_ImVec4_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_back" ImVector_ImDrawCmd_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_back" ImVector_ImDrawIdx_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_back" ImVector_ImVec2_back) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_back_const" ImVector_float_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_back_const" ImVector_ImWchar_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_back_const" ImVector_ImFontConfig_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_back_const" ImVector_ImFontGlyph_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_back_const" ImVector_TextRange_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_back_const" ImVector_CustomRect_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_back_const" ImVector_ImDrawChannel_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_char_back_const" ImVector_char_back_const) :string
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_back_const" ImVector_ImTextureID_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_back_const" ImVector_ImDrawVert_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_int_back_const" ImVector_int_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_back_const" ImVector_Pair_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_back_const" ImVector_ImFontPtr_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_back_const" ImVector_ImVec4_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_back_const" ImVector_ImDrawCmd_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_back_const" ImVector_ImDrawIdx_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_back_const" ImVector_ImVec2_back_const) :pointer
  (self :pointer))

(cffi:defcfun ("ImVector_float_swap" ImVector_float_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImWchar_swap" ImVector_ImWchar_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_swap" ImVector_ImFontConfig_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_swap" ImVector_ImFontGlyph_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_TextRange_swap" ImVector_TextRange_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_CustomRect_swap" ImVector_CustomRect_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_swap" ImVector_ImDrawChannel_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_char_swap" ImVector_char_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImTextureID_swap" ImVector_ImTextureID_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_swap" ImVector_ImDrawVert_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_int_swap" ImVector_int_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_Pair_swap" ImVector_Pair_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_swap" ImVector_ImFontPtr_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImVec4_swap" ImVector_ImVec4_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_swap" ImVector_ImDrawCmd_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_swap" ImVector_ImDrawIdx_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_ImVec2_swap" ImVector_ImVec2_swap) :void
  (self :pointer)
  (rhs :pointer))

(cffi:defcfun ("ImVector_float__grow_capacity" ImVector_float__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImWchar__grow_capacity" ImVector_ImWchar__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImFontConfig__grow_capacity" ImVector_ImFontConfig__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImFontGlyph__grow_capacity" ImVector_ImFontGlyph__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_TextRange__grow_capacity" ImVector_TextRange__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_CustomRect__grow_capacity" ImVector_CustomRect__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImDrawChannel__grow_capacity" ImVector_ImDrawChannel__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_char__grow_capacity" ImVector_char__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImTextureID__grow_capacity" ImVector_ImTextureID__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImDrawVert__grow_capacity" ImVector_ImDrawVert__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_int__grow_capacity" ImVector_int__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_Pair__grow_capacity" ImVector_Pair__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImFontPtr__grow_capacity" ImVector_ImFontPtr__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImVec4__grow_capacity" ImVector_ImVec4__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImDrawCmd__grow_capacity" ImVector_ImDrawCmd__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImDrawIdx__grow_capacity" ImVector_ImDrawIdx__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_ImVec2__grow_capacity" ImVector_ImVec2__grow_capacity) :int
  (self :pointer)
  (sz :int))

(cffi:defcfun ("ImVector_float_resize" ImVector_float_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImWchar_resize" ImVector_ImWchar_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImFontConfig_resize" ImVector_ImFontConfig_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImFontGlyph_resize" ImVector_ImFontGlyph_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_TextRange_resize" ImVector_TextRange_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_CustomRect_resize" ImVector_CustomRect_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImDrawChannel_resize" ImVector_ImDrawChannel_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_char_resize" ImVector_char_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImTextureID_resize" ImVector_ImTextureID_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImDrawVert_resize" ImVector_ImDrawVert_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_int_resize" ImVector_int_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_Pair_resize" ImVector_Pair_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImFontPtr_resize" ImVector_ImFontPtr_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImVec4_resize" ImVector_ImVec4_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImDrawCmd_resize" ImVector_ImDrawCmd_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImDrawIdx_resize" ImVector_ImDrawIdx_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_ImVec2_resize" ImVector_ImVec2_resize) :void
  (self :pointer)
  (new_size :int))

(cffi:defcfun ("ImVector_float_resizeT" ImVector_float_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :float))

(cffi:defcfun ("ImVector_ImWchar_resizeT" ImVector_ImWchar_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_resizeT" ImVector_ImFontConfig_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_resizeT" ImVector_ImFontGlyph_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_TextRange_resizeT" ImVector_TextRange_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_CustomRect_resizeT" ImVector_CustomRect_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_resizeT" ImVector_ImDrawChannel_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_char_resizeT" ImVector_char_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :char))

(cffi:defcfun ("ImVector_ImTextureID_resizeT" ImVector_ImTextureID_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_resizeT" ImVector_ImDrawVert_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_int_resizeT" ImVector_int_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :int))

(cffi:defcfun ("ImVector_Pair_resizeT" ImVector_Pair_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_resizeT" ImVector_ImFontPtr_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec4_resizeT" ImVector_ImVec4_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_resizeT" ImVector_ImDrawCmd_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_resizeT" ImVector_ImDrawIdx_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec2_resizeT" ImVector_ImVec2_resizeT) :void
  (self :pointer)
  (new_size :int)
  (v :pointer))

(cffi:defcfun ("ImVector_float_reserve" ImVector_float_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImWchar_reserve" ImVector_ImWchar_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImFontConfig_reserve" ImVector_ImFontConfig_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImFontGlyph_reserve" ImVector_ImFontGlyph_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_TextRange_reserve" ImVector_TextRange_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_CustomRect_reserve" ImVector_CustomRect_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImDrawChannel_reserve" ImVector_ImDrawChannel_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_char_reserve" ImVector_char_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImTextureID_reserve" ImVector_ImTextureID_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImDrawVert_reserve" ImVector_ImDrawVert_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_int_reserve" ImVector_int_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_Pair_reserve" ImVector_Pair_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImFontPtr_reserve" ImVector_ImFontPtr_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImVec4_reserve" ImVector_ImVec4_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImDrawCmd_reserve" ImVector_ImDrawCmd_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImDrawIdx_reserve" ImVector_ImDrawIdx_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_ImVec2_reserve" ImVector_ImVec2_reserve) :void
  (self :pointer)
  (new_capacity :int))

(cffi:defcfun ("ImVector_float_push_back" ImVector_float_push_back) :void
  (self :pointer)
  (v :float))

(cffi:defcfun ("ImVector_ImWchar_push_back" ImVector_ImWchar_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_push_back" ImVector_ImFontConfig_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_push_back" ImVector_ImFontGlyph_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_TextRange_push_back" ImVector_TextRange_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_CustomRect_push_back" ImVector_CustomRect_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_push_back" ImVector_ImDrawChannel_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_char_push_back" ImVector_char_push_back) :void
  (self :pointer)
  (v :char))

(cffi:defcfun ("ImVector_ImTextureID_push_back" ImVector_ImTextureID_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_push_back" ImVector_ImDrawVert_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_int_push_back" ImVector_int_push_back) :void
  (self :pointer)
  (v :int))

(cffi:defcfun ("ImVector_Pair_push_back" ImVector_Pair_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_push_back" ImVector_ImFontPtr_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec4_push_back" ImVector_ImVec4_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_push_back" ImVector_ImDrawCmd_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_push_back" ImVector_ImDrawIdx_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec2_push_back" ImVector_ImVec2_push_back) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_float_pop_back" ImVector_float_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImWchar_pop_back" ImVector_ImWchar_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_pop_back" ImVector_ImFontConfig_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_pop_back" ImVector_ImFontGlyph_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_TextRange_pop_back" ImVector_TextRange_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_CustomRect_pop_back" ImVector_CustomRect_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_pop_back" ImVector_ImDrawChannel_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_char_pop_back" ImVector_char_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImTextureID_pop_back" ImVector_ImTextureID_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_pop_back" ImVector_ImDrawVert_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_int_pop_back" ImVector_int_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_Pair_pop_back" ImVector_Pair_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_pop_back" ImVector_ImFontPtr_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec4_pop_back" ImVector_ImVec4_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_pop_back" ImVector_ImDrawCmd_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_pop_back" ImVector_ImDrawIdx_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_ImVec2_pop_back" ImVector_ImVec2_pop_back) :void
  (self :pointer))

(cffi:defcfun ("ImVector_float_push_front" ImVector_float_push_front) :void
  (self :pointer)
  (v :float))

(cffi:defcfun ("ImVector_ImWchar_push_front" ImVector_ImWchar_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_push_front" ImVector_ImFontConfig_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_push_front" ImVector_ImFontGlyph_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_TextRange_push_front" ImVector_TextRange_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_CustomRect_push_front" ImVector_CustomRect_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_push_front" ImVector_ImDrawChannel_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_char_push_front" ImVector_char_push_front) :void
  (self :pointer)
  (v :char))

(cffi:defcfun ("ImVector_ImTextureID_push_front" ImVector_ImTextureID_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_push_front" ImVector_ImDrawVert_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_int_push_front" ImVector_int_push_front) :void
  (self :pointer)
  (v :int))

(cffi:defcfun ("ImVector_Pair_push_front" ImVector_Pair_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_push_front" ImVector_ImFontPtr_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec4_push_front" ImVector_ImVec4_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_push_front" ImVector_ImDrawCmd_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_push_front" ImVector_ImDrawIdx_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec2_push_front" ImVector_ImVec2_push_front) :void
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_float_erase" ImVector_float_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImWchar_erase" ImVector_ImWchar_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_erase" ImVector_ImFontConfig_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_erase" ImVector_ImFontGlyph_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_TextRange_erase" ImVector_TextRange_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_CustomRect_erase" ImVector_CustomRect_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_erase" ImVector_ImDrawChannel_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_char_erase" ImVector_char_erase) :string
  (self :pointer)
  (it :string))

(cffi:defcfun ("ImVector_ImTextureID_erase" ImVector_ImTextureID_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_erase" ImVector_ImDrawVert_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_int_erase" ImVector_int_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_Pair_erase" ImVector_Pair_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_erase" ImVector_ImFontPtr_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImVec4_erase" ImVector_ImVec4_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_erase" ImVector_ImDrawCmd_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_erase" ImVector_ImDrawIdx_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImVec2_erase" ImVector_ImVec2_erase) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_float_eraseTPtr" ImVector_float_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImWchar_eraseTPtr" ImVector_ImWchar_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_eraseTPtr" ImVector_ImFontConfig_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_eraseTPtr" ImVector_ImFontGlyph_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_TextRange_eraseTPtr" ImVector_TextRange_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_CustomRect_eraseTPtr" ImVector_CustomRect_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_eraseTPtr" ImVector_ImDrawChannel_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_char_eraseTPtr" ImVector_char_eraseTPtr) :string
  (self :pointer)
  (it :string)
  (it_last :string))

(cffi:defcfun ("ImVector_ImTextureID_eraseTPtr" ImVector_ImTextureID_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_eraseTPtr" ImVector_ImDrawVert_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_int_eraseTPtr" ImVector_int_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_Pair_eraseTPtr" ImVector_Pair_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_eraseTPtr" ImVector_ImFontPtr_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImVec4_eraseTPtr" ImVector_ImVec4_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_eraseTPtr" ImVector_ImDrawCmd_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_eraseTPtr" ImVector_ImDrawIdx_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_ImVec2_eraseTPtr" ImVector_ImVec2_eraseTPtr) :pointer
  (self :pointer)
  (it :pointer)
  (it_last :pointer))

(cffi:defcfun ("ImVector_float_erase_unsorted" ImVector_float_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImWchar_erase_unsorted" ImVector_ImWchar_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_erase_unsorted" ImVector_ImFontConfig_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_erase_unsorted" ImVector_ImFontGlyph_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_TextRange_erase_unsorted" ImVector_TextRange_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_CustomRect_erase_unsorted" ImVector_CustomRect_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_erase_unsorted" ImVector_ImDrawChannel_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_char_erase_unsorted" ImVector_char_erase_unsorted) :string
  (self :pointer)
  (it :string))

(cffi:defcfun ("ImVector_ImTextureID_erase_unsorted" ImVector_ImTextureID_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_erase_unsorted" ImVector_ImDrawVert_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_int_erase_unsorted" ImVector_int_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_Pair_erase_unsorted" ImVector_Pair_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_erase_unsorted" ImVector_ImFontPtr_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImVec4_erase_unsorted" ImVector_ImVec4_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_erase_unsorted" ImVector_ImDrawCmd_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_erase_unsorted" ImVector_ImDrawIdx_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImVec2_erase_unsorted" ImVector_ImVec2_erase_unsorted) :pointer
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_float_insert" ImVector_float_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :float))

(cffi:defcfun ("ImVector_ImWchar_insert" ImVector_ImWchar_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_insert" ImVector_ImFontConfig_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_insert" ImVector_ImFontGlyph_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_TextRange_insert" ImVector_TextRange_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_CustomRect_insert" ImVector_CustomRect_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_insert" ImVector_ImDrawChannel_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_char_insert" ImVector_char_insert) :string
  (self :pointer)
  (it :string)
  (v :char))

(cffi:defcfun ("ImVector_ImTextureID_insert" ImVector_ImTextureID_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_insert" ImVector_ImDrawVert_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_int_insert" ImVector_int_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :int))

(cffi:defcfun ("ImVector_Pair_insert" ImVector_Pair_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_insert" ImVector_ImFontPtr_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec4_insert" ImVector_ImVec4_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_insert" ImVector_ImDrawCmd_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_insert" ImVector_ImDrawIdx_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_ImVec2_insert" ImVector_ImVec2_insert) :pointer
  (self :pointer)
  (it :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_float_contains" ImVector_float_contains) :pointer
  (self :pointer)
  (v :float))

(cffi:defcfun ("ImVector_ImWchar_contains" ImVector_ImWchar_contains) :pointer
  (self :pointer)
  (v :pointer))

(cffi:defcfun ("ImVector_char_contains" ImVector_char_contains) :pointer
  (self :pointer)
  (v :char))

(cffi:defcfun ("ImVector_int_contains" ImVector_int_contains) :pointer
  (self :pointer)
  (v :int))

(cffi:defcfun ("ImVector_float_index_from_ptr" ImVector_float_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImWchar_index_from_ptr" ImVector_ImWchar_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontConfig_index_from_ptr" ImVector_ImFontConfig_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontGlyph_index_from_ptr" ImVector_ImFontGlyph_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_TextRange_index_from_ptr" ImVector_TextRange_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_CustomRect_index_from_ptr" ImVector_CustomRect_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawChannel_index_from_ptr" ImVector_ImDrawChannel_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_char_index_from_ptr" ImVector_char_index_from_ptr) :int
  (self :pointer)
  (it :string))

(cffi:defcfun ("ImVector_ImTextureID_index_from_ptr" ImVector_ImTextureID_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawVert_index_from_ptr" ImVector_ImDrawVert_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_int_index_from_ptr" ImVector_int_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_Pair_index_from_ptr" ImVector_Pair_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImFontPtr_index_from_ptr" ImVector_ImFontPtr_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImVec4_index_from_ptr" ImVector_ImVec4_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawCmd_index_from_ptr" ImVector_ImDrawCmd_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImDrawIdx_index_from_ptr" ImVector_ImDrawIdx_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("ImVector_ImVec2_index_from_ptr" ImVector_ImVec2_index_from_ptr) :int
  (self :pointer)
  (it :pointer))

(cffi:defcfun ("igLogText" igLogText) :void
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("ImGuiTextBuffer_appendf" ImGuiTextBuffer_appendf) :void
  (buffer :pointer)
  (fmt :string)
  cl::&rest)

(cffi:defcfun ("igGET_FLT_MAX" igGET_FLT_MAX) :float)

(cffi:defcfun ("igColorConvertRGBtoHSV" igColorConvertRGBtoHSV) :void
  (r :float)
  (g :float)
  (b :float)
  (out_h :pointer)
  (out_s :pointer)
  (out_v :pointer))

(cffi:defcfun ("igColorConvertHSVtoRGB" igColorConvertHSVtoRGB) :void
  (h :float)
  (s :float)
  (v :float)
  (out_r :pointer)
  (out_g :pointer)
  (out_b :pointer))

(cffi:defcfun ("ImVector_ImWchar_create" ImVector_ImWchar_create) :pointer)

(cffi:defcfun ("ImVector_ImWchar_Init" ImVector_ImWchar_Init) :void
  (p :pointer))

(cffi:defcfun ("ImVector_ImWchar_UnInit" ImVector_ImWchar_UnInit) :void
  (p :pointer))
;;---
(cffi:defcfun ("_igBeginChild" igBeginChild) :int
  (str_id :string)
  (size :pointer)
  (border :bool)
  (extra_flags :int))

(cffi:defcfun ("_igSetNextWindowPos" igSetNextWindowPos) :void
  (pos :pointer)
  (cond :int)
  (pivot :pointer))

(cffi:defcfun ("_igSetNextWindowSize" igSetNextWindowSize) :void
  (size :pointer)
  (cond :int))

(cffi:defcfun ("_igSetNextWindowSizeConstraints" igSetNextWindowSizeConstraints) :void
  (size_min :pointer)
  (size_max :pointer)
  (custom_callback :pointer)
  (custom_callback_data :pointer))

(cffi:defcfun ("_igSetNextWindowContentSize" igSetNextWindowContentSize) :void
  (size :pointer))

(cffi:defcfun ("_igSetWindowPosVec2" igSetWindowPosVec2) :void
  (larg1 :pointer)
  (larg2 :pointer))

(cffi:defcfun ("_igSetWindowSizeVec2" igSetWindowSizeVec2) :void
  (larg1 :pointer)
  (larg2 :pointer))

(cffi:defcfun ("_igSetWindowPosStr" igSetWindowPosStr) :void
  (larg1 :string)
  (larg2 :pointer)
  (larg3 :pointer))

(cffi:defcfun ("_igSetWindowSizeStr" igSetWindowSizeStr) :void
  (larg1 :string)
  (larg2 :pointer)
  (larg3 :pointer))

(cffi:defcfun ("_igPushStyleColor" igPushStyleColor) :void
  (larg1 :pointer)
  (larg2 :pointer))

(cffi:defcfun ("_igPushStyleVarVec2" igPushStyleVarVec2) :void
  (larg1 :pointer)
  (larg2 :pointer))

(cffi:defcfun ("_igSetCursorPos" igSetCursorPos) :void
  (larg1 :pointer))

(cffi:defcfun ("_igSetCursorScreenPos" igSetCursorScreenPos) :void
  (larg1 :pointer))

(cffi:defcfun ("_igTextColored" igTextColored) :void
  (larg1 :pointer)
  (larg2 :string))

(cffi:defcfun ("_igTextColoredV" igTextColoredV) :void
  (larg1 :pointer)
  (larg2 :string)
  (larg3 :pointer))

(cffi:defcfun ("_igButton" igButton) :bool
  (label :string)
  (size :pointer))

(cffi:defcfun ("_igInvisibleButton" igInvisibleButton) :bool
  (str_id :string)
  (size :pointer))

(cffi:defcfun ("_igImage" igImage) :void
  (user-texture-id :pointer)
  (size :pointer)
  (uv0 :pointer)
  (uv1 :pointer)
  (tint-col :pointer)
  (border-col :pointer))

(cffi:defcfun ("_igImageButton" igImageButton) :int
  (user-texture-id :pointer)
  (size :pointer)
  (uv0 :pointer)
  (uv1 :pointer)
  (frame-padding :int)
  (bg-col :pointer)
  (tint-col :pointer))

(cffi:defcfun ("_igPlotLines" igPlotLines) :void
  (label :string)
  (values :pointer)
  (values-count :int)
  (values-offset :int)
  (overlay-text :string)
  (scale-min :float)
  (scale-max :float)
  (graph-size :pointer)
  (stride :int))

(cffi:defcfun ("_igPlotLinesFnPtr" igPlotLinesFnPtr) :void
  (label :string)
  (values-getter-callback :pointer)
  (data :pointer)
  (values-count :int)
  (values-offset :int)
  (overlay-text :string)
  (scale-min :float)
  (scale-max :float)
  (graph-size :pointer))

(cffi:defcfun ("_igPlotHistogramFloatPtr" igPlotHistogramFloatPtr) :void
  (label :string)
  (values :pointer)
  (values-count :int)
  (values-offset :int)
  (overlay-text :string)
  (scale-min :float)
  (scale-max :float)
  (graph-size :pointer)
  (stride :int))

(cffi:defcfun ("_igPlotHistogramFnPtr" igPlotHistogramFnPtr) :void
  (label :string)
  (values-getter-callback :pointer)
  (data :pointer)
  (values-count :int)
  (values-offset :int)
  (overlay-text :string)
  (scale-min :float)
  (scale-max :float)
  (graph-size :pointer))

(cffi:defcfun ("_igInputTextMultiline" igInputTextMultiline) :int
  (label :string)
  (buf :string)
  (buf_size :pointer)
  (size :pointer)
  (flags :int)
  (callback :pointer)
  (user_data :pointer))

(cffi:defcfun ("_igVSliderFloat" igVSliderFloat) :int
  (label :string)
  (size :pointer)
  (v :pointer)
  (v_min :float)
  (v_max :float)
  (format :string)
  (power :float))

(cffi:defcfun ("_igVSliderInt" igVSliderInt) :int
  (label :string)
  (size :pointer)
  (v :pointer)
  (v_min :int)
  (v_max :int)
  (format :string))

(cffi:defcfun ("_igColorButton" igColorButton) :int
  (desc_id :string)
  (col :pointer)
  (flags :int)
  (size :pointer))

(cffi:defcfun ("_igSelectable" igSelectable) :int
  (label :string)
  (selected :pointer)
  (flags :int)
  (size :pointer))

(cffi:defcfun ("_igSelectableBoolPtr" igSelectableBoolPtr) :int
  (label :string)
  (p_selected :pointer)
  (flags :int)
  (size :pointer))

(cffi:defcfun ("_igListBoxHeaderVec2" igListBoxHeaderVec2) :int
  (label :string)
  (size :pointer))

(cffi:defcfun ("_igPushClipRect" igPushClipRect) :void
  (clip_rect_min :pointer)
  (clip_rect_max :pointer)
  (intersect_with_current_clip_rect :int))

(cffi:defcfun ("_igIsRectVisible" igIsRectVisible) :int
  (size :pointer))

(cffi:defcfun ("_igBeginChildFrame" igBeginChildFrame) :bool
  (id :pointer)
  (size :pointer)
  (flags :int))

(cffi:defcfun ("_igColorConvertFloat4ToU32" igColorConvertFloat4ToU32) :pointer
  (in :pointer))

(cffi:defcfun ("_igIsMouseHoveringRect" igIsMouseHoveringRect) :int
  (r_min :pointer)
  (r_max :pointer)
  (clip :int))

(cffi:defcfun ("_ImDrawList_PushClipRect" ImDrawList_PushClipRect) :void
  (self :pointer)
  (clip_rect_min :pointer)
  (clip_rect_max :pointer)
  (intersect_with_current_clip_rect :int))

(cffi:defcfun ("_ImDrawList_AddLine" ImDrawList_AddLine) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (col :pointer)
  (thickness :float))

(cffi:defcfun ("_ImDrawList_AddRect" ImDrawList_AddRect) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (col :pointer)
  (rounding :float)
  (rounding_corners_flags :int)
  (thickness :float))

(cffi:defcfun ("_ImDrawList_AddRectFilled" ImDrawList_AddRectFilled) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (col :pointer)
  (rounding :float)
  (rounding_corners_flags :int))

(cffi:defcfun ("_ImDrawList_AddRectFilledMultiColor" ImDrawList_AddRectFilledMultiColor) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (col_upr_left :pointer)
  (col_upr_right :pointer)
  (col_bot_right :pointer)
  (col_bot_left :pointer))

(cffi:defcfun ("_ImDrawList_AddQuad" ImDrawList_AddQuad) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (c :pointer)
  (d :pointer)
  (col :pointer)
  (thickness :float))

(cffi:defcfun ("_ImDrawList_AddQuadFilled" ImDrawList_AddQuadFilled) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (c :pointer)
  (d :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawList_AddTriangle" ImDrawList_AddTriangle) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (c :pointer)
  (col :pointer)
  (thickness :float))

(cffi:defcfun ("_ImDrawList_AddTriangleFilled" ImDrawList_AddTriangleFilled) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (c :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawList_AddCircle" ImDrawList_AddCircle) :void
  (self :pointer)
  (centre :pointer)
  (radius :float)
  (col :pointer)
  (num_segments :int)
  (thickness :float))

(cffi:defcfun ("_ImDrawList_AddCircleFilled" ImDrawList_AddCircleFilled) :void
  (self :pointer)
  (centre :pointer)
  (radius :float)
  (col :pointer)
  (num_segments :int))


(cffi:defcfun ("_ImDrawList_AddText" ImDrawList_AddText) :void
  (self :pointer)
  (pos :pointer)
  (col :pointer)
  (text_begin :string)
  (text_end :string))

(cffi:defcfun ("_ImDrawList_AddTextFontPtr" ImDrawList_AddTextFontPtr) :void
  (self :pointer)
  (font :pointer)
  (font_size :float)
  (pos :pointer)
  (col :pointer)
  (text_begin :string)
  (text_end :string)
  (wrap_width :float)
  (cpu_fine_clip_rect :pointer))

(cffi:defcfun ("ImDrawList_AddPolyline" ImDrawList_AddPolyline) :void
  (self :pointer)
  (points :pointer)
  (num_points :int)
  (col :pointer)
  (closed :pointer)
  (thickness :float))

(cffi:defcfun ("_ImDrawList_AddImage" ImDrawList_AddImage) :void
  (self :pointer)
  (user_texture_id :pointer)
  (a :pointer)
  (b :pointer)
  (uv_a :pointer)
  (uv_b :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawList_AddImageQuad" ImDrawList_AddImageQuad) :void
  (self :pointer)
  (user_texture_id :pointer)
  (a :pointer)
  (b :pointer)
  (c :pointer)
  (d :pointer)
  (uv_a :pointer)
  (uv_b :pointer)
  (uv_c :pointer)
  (uv_d :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawList_AddImageRounded" ImDrawList_AddImageRounded) :void
  (self :pointer)
  (user_texture_id :pointer)
  (a :pointer)
  (b :pointer)
  (uv_a :pointer)
  (uv_b :pointer)
  (col :pointer)
  (rounding :float)
  (rounding_corners :int))

(cffi:defcfun ("_ImDrawList_PathLineTo" ImDrawList_PathLineTo) :void
  (self :pointer)
  (pos :pointer))

(cffi:defcfun ("_ImDrawList_PathLineToMergeDuplicate" ImDrawList_PathLineToMergeDuplicate) :void
  (self :pointer)
  (pos :pointer))

(cffi:defcfun ("_ImDrawList_PathArcTo" ImDrawList_PathArcTo) :void
  (self :pointer)
  (centre :pointer)
  (radius :float)
  (a_min :float)
  (a_max :float)
  (num_segments :int))

(cffi:defcfun ("_ImDrawList_PathArcToFast" ImDrawList_PathArcToFast) :void
  (self :pointer)
  (centre :pointer)
  (radius :float)
  (a_min_of_12 :int)
  (a_max_of_12 :int))

(cffi:defcfun ("_ImDrawList_PathBezierCurveTo" ImDrawList_PathBezierCurveTo) :void
  (self :pointer)
  (p1 :pointer)
  (p2 :pointer)
  (p3 :pointer)
  (num_segments :int))

(cffi:defcfun ("_ImDrawList_PathRect" ImDrawList_PathRect) :void
  (self :pointer)
  (rect_min :pointer)
  (rect_max :pointer)
  (rounding :float)
  (rounding_corners_flags :int))

(cffi:defcfun ("_ImDrawList_PrimQuadUV" ImDrawList_PrimQuadUV) :void
  (self :pointer)
  (a :pointer)
  (b :pointer)
  (c :pointer)
  (d :pointer)
  (uv_a :pointer)
  (uv_b :pointer)
  (uv_c :pointer)
  (uv_d :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawList_PrimWriteVtx" ImDrawList_PrimWriteVtx) :void
  (self :pointer)
  (pos :pointer)
  (uv :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawList_PrimVtx" ImDrawList_PrimVtx) :void
  (self :pointer)
  (pos :pointer)
  (uv :pointer)
  (col :pointer))

(cffi:defcfun ("_ImDrawData_ScaleClipRects" ImDrawData_ScaleClipRects) :void
  (self :pointer)
  (fb_scale :pointer))

(cffi:defcfun ("_igGetWindowPos" igGetWindowPos) :pointer)

(cffi:defcfun ("_igGetWindowSize" igGetWindowSize) :pointer)

(cffi:defcfun ("_igGetContentRegionMax" igGetContentRegionMax) :pointer)

(cffi:defcfun ("_igGetContentRegionAvail" igGetContentRegionAvail) :pointer)

(cffi:defcfun ("_igGetWindowContentRegionMin" igGetWindowContentRegionMin) :pointer)

(cffi:defcfun ("_igGetWindowContentRegionMax" igGetWindowContentRegionMax) :pointer)

(cffi:defcfun ("_igGetFontTexUvWhitePixel" igGetFontTexUvWhitePixel) :pointer)

(cffi:defcfun ("_igGetCursorPos" igGetCursorPos) :pointer)

(cffi:defcfun ("_igGetCursorStartPos" igGetCursorStartPos) :pointer)

(cffi:defcfun ("_igGetCursorScreenPos" igGetCursorScreenPos) :pointer)

(cffi:defcfun ("_igGetItemRectSize" igGetItemRectSize) :pointer)

(cffi:defcfun ("_igGetItemRectMin" igGetItemRectMin) :pointer)

(cffi:defcfun ("_igGetItemRectMax" igGetItemRectMax) :pointer)

(cffi:defcfun ("_igCalcTextSize" igCalcTextSize) :pointer
  (text :string)
  (text_end :string)
  (hide_text_after_double_hash :pointer)
  (wrap_width :float))

(cffi:defcfun ("_igColorConvertU32ToFloat4" igColorConvertU32ToFloat4) :pointer
  (in :pointer))

(cffi:defcfun ("_igGetMousePos" igGetMousePos) :pointer)

(cffi:defcfun ("_igGetMousePosOnOpeningCurrentPopup" igGetMousePosOnOpeningCurrentPopup) :pointer)

(cffi:defcfun ("_igGetMouseDragDelta" igGetMouseDragDelta) :pointer
  (button :int)
  (lock_threshold :float))

(cffi:defcfun ("_ImColor_HSV" ImColor_HSV) :pointer
  (self :pointer)
  (h :float)
  (s :float)
  (v :float)
  (a :float))

(cffi:defcfun ("_ImDrawList_GetClipRectMin" ImDrawList_GetClipRectMin) :pointer
  (self :pointer))

(cffi:defcfun ("_ImDrawList_GetClipRectMax" ImDrawList_GetClipRectMax) :pointer
  (self :pointer))

(cffi:defcfun ("_ImFont_CalcTextSizeA" ImFont_CalcTextSizeA) :pointer
  (self :pointer)
  (size :float)
  (max_width :float)
  (wrap_width :float)
  (text_begin :string)
  (text_end :string)
  (remaining :pointer))

(cffi:defcfun ("_igGetWindowPos_nonUDT2" igGetWindowPos_nonUDT2) :pointer)

(cffi:defcfun ("_igGetWindowSize_nonUDT2" igGetWindowSize_nonUDT2) :pointer)

(cffi:defcfun ("_igGetContentRegionMax_nonUDT2" igGetContentRegionMax_nonUDT2) :pointer)

(cffi:defcfun ("_igGetContentRegionAvail_nonUDT2" igGetContentRegionAvail_nonUDT2) :pointer)

(cffi:defcfun ("_igGetWindowContentRegionMin_nonUDT2" igGetWindowContentRegionMin_nonUDT2) :pointer)

(cffi:defcfun ("_igGetWindowContentRegionMax_nonUDT2" igGetWindowContentRegionMax_nonUDT2) :pointer)

(cffi:defcfun ("_igGetFontTexUvWhitePixel_nonUDT2" igGetFontTexUvWhitePixel_nonUDT2) :pointer)

(cffi:defcfun ("_igGetCursorPos_nonUDT2" igGetCursorPos_nonUDT2) :pointer)

(cffi:defcfun ("_igGetCursorStartPos_nonUDT2" igGetCursorStartPos_nonUDT2) :pointer)

(cffi:defcfun ("_igGetCursorScreenPos_nonUDT2" igGetCursorScreenPos_nonUDT2) :pointer)

(cffi:defcfun ("_igGetItemRectMin_nonUDT2" igGetItemRectMin_nonUDT2) :pointer)

(cffi:defcfun ("_igGetItemRectMax_nonUDT2" igGetItemRectMax_nonUDT2) :pointer)

(cffi:defcfun ("_igCalcTextSize_nonUDT2" igCalcTextSize_nonUDT2) :pointer
  (larg1 :string)
  (larg2 :string)
  (larg3 :int)
  (larg4 :float))

(cffi:defcfun ("_igColorConvertU32ToFloat4_nonUDT2" igColorConvertU32ToFloat4_nonUDT2) :pointer
  (larg1 :pointer))

(cffi:defcfun ("_igGetMousePos_nonUDT2" igGetMousePos_nonUDT2) :pointer)

(cffi:defcfun ("_igGetMousePosOnOpeningCurrentPopup_nonUDT2" igGetMousePosOnOpeningCurrentPopup_nonUDT2) :pointer)

(cffi:defcfun ("_igGetMouseDragDelta_nonUDT2" igGetMouseDragDelta_nonUDT2) :pointer
  (larg1 :int)
  (larg2 :float))

(cffi:defcfun ("_ImColor_HSV_nonUDT2" ImColor_HSV_nonUDT2) :pointer
  (larg1 :pointer)
  (larg2 :float)
  (larg3 :float)
  (larg4 :float)
  (larg5 :float))

(cffi:defcfun ("_ImDrawList_GetClipRectMin_nonUDT2" ImDrawList_GetClipRectMin_nonUDT2) :pointer
  (larg1 :pointer))

(cffi:defcfun ("_ImDrawList_GetClipRectMax_nonUDT2" ImDrawList_GetClipRectMax_nonUDT2) :pointer
  (larg1 :pointer))

(cffi:defcfun ("_ImFont_CalcTextSizeA_nonUDT2" ImFont_CalcTextSizeA_nonUDT2) :pointer
  (larg1 :pointer)
  (larg2 :float)
  (larg3 :float)
  (larg4 :float)
  (larg5 :string)
  (larg6 :string)
  (larg7 :pointer))

(cffi:defcfun ("_ImDrawList_PrimRect" ImDrawList_PrimRect) :void
  (list :pointer)
  (a :pointer)
  (b :pointer)
  (col :unsigned-int))

(cffi:defcfun ("_ImDrawList_PrimRectUV" ImDrawList_PrimRectUV) :void
  (list :pointer)
  (a :pointer)
  (b :pointer)
  (uv_a :pointer)
  (uv_b :pointer)
  (col :unsigned-int))


