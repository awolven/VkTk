# Quick start:

Install vulkan sdk.

`git clone https://github.com/awolven/VkTk.git <vktk-dir>`

Where <vktk-dir> is the directory you want git to create, defaults to VkTk.

`cd <vktk-dir>`

`git submodule update --init --recursive`

`cd cimgui`

`git checkout cimgui-cffi`

`cd ..`

Edit ifc/foreign-libraries.lisp:

Change *vktk-dir* to point to your <vktk-dir>.

Change *vulkan-sdk-path* to point to your vulkan sdk.

In emacs `M-x slime`.

`(push "<vktk-dir>/" asdf:*central-registry*)`

`(asdf:oos 'asdf:load-op :vktk)`

`(vktk:run-demo)`

--------

### What is vktk?

vktk is a library for programming Vulkan in Common Lisp.

"vktk" is an abbreviation for Vulkan Toolkit.

### What can vktk do?

vktk provides an abstraction for Vulkan, GLFW, and ImGui.

Vulkan is an interactive 3d-graphics library specification with implementations on Windows, Linux, Android, and Mac.  (vktk currently only supports Windows and Mac, but Linux could easily be added.)  GLFW is a platform independent Windowing library, and ImGUI is a GUI library which supports interactive 3D libraries such as Vulkan, OpenGL and Direct3D.

### Why Vulkan?

Vulkan aims to be the cross-platform accelerated 3D graphics library standard to replace OpenGL and is more flexible to program in threaded environments than OpenGL.

### Why Common Lisp?

Common Lisp is a well supported, standardized, functional/imperative/object-oriented programming language that is designed to allow large applications to be written with it.  Common Lisp is efficient and expressive but most of all I like it.

### Why was vktk created?

I needed a 3D graphics/UI library for Computer Aided Design programming in Common Lisp.  vktk has a sister project for using the OpenCascade solid modeler with Common Lisp.



