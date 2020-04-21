# Latest news:

Linux AMD64 port now available.

# Quick start:

These instructions assume you have [SBCL](http://www.sbcl.org/) installed on your system with asdf and quicklisp.  If you have a macOS port of SBCL that is single threaded, you can easily build a multithreaded version, but the single thread version should run the VkTk Demo just fine.

Install vulkan sdk.

`git clone https://github.com/awolven/VkTk.git <vktk-dir>`

Where `<vktk-dir>` is a placeholder for the directory you want git to create.  Pick a directory name to use there.  (Defaults to VkTk.)

`cd <vktk-dir>`

`git submodule update --init --recursive`

`cd cimgui`

`git checkout cimgui-cffi`

`cd ..`

Edit ifc/foreign-libraries.lisp:

Change `*vktk-dir*` to point to your `<vktk-dir>`.

Change `*vulkan-sdk-path*` to point to your vulkan sdk.

In emacs `M-x slime`.

`(push "<vktk-dir>/" asdf:*central-registry*)`

`(asdf:oos 'asdf:load-op :vktk)`

There currently is no standalone demo for vktk.  That is a todo item.  There is, however, a demo of VkTk functionality combined with `oc`.  oc is an interface to the OpenCASCADE solid modeler.  The demo shows vulkan functionality, imgui functionality and solid modeler functionality together.  The idea is to turn this demo into an application.  To install/run the demo, follow these additional instructions:

`git clone https://github.com/awolven/oc.git`

This operation may take a while as OpenCASCADE binaries are provided.

In slime:

`(push "<oc-dir>/" asdf:*central-registry*)`

`(asdf:oos 'asdf:load-op :igp)`

`(igp::run-demo)`

When the linux port was created, it was created on a virtual machine, so a software installable client driver was used: swiftshader.  If you're running with a real vulkan capable graphics card, the performance should be fine.  Swiftshader is a little slow.

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



