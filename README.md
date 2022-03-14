# project-cmake

Emacs extension to the project package for supporting CMake as build system.

## Description

This package is an extension to Emacs' own `project` package [(see link)](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html) .  Emacs' `project` understands more or less the source coude structure of a repository, it can help you find files and open shells at the right locations. However, its `project-compile` minimalistic function does not really work and does not understand modern build systems.

The package `project-cmake` incorporates the required logic to understand that a project is to be configured, built and tested using CMake and CTest.  It also is capable of recognizing different build kits, on platforms that support it (see below).

`project-cmake` also adds new key bindings to the `project-prefix-map`, with the following default assignments
````
   C-x p C   -  project-cmake-configure
   C-x p m   -  project-cmake-build
   C-x p t   -  project-cmake-test
   C-x p i   -  project-cmake-install
   C-x p s   -  project-cmake-shell
   C-x p S K -  project-cmake-select-kit
   C-x p S S -  project-cmake-save-settings
   C-x p S E -  project-cmake-edit-settings
   C-x p U   -  project-cmake-debug
````

`project-cmake-configure`, `project-cmake-build`, `project-cmake-test`, `project-cmake-install` and `project-cmake-debug` do the expected thing: configure, compile, test and install the software using CMake and CTest. When provided arguments (e.g. by pressing `C-u` before invoking the command) `project-cmake-configure`  and `project-cmake-build` with perform a clean phase. When configuring, the clean phase means that the build directory will be wiped out. When building, the clean phase simply deletes all compiled files and runs the build process from scratch. When building or debugging, `project-cmake` will query the user for the desired target out of those that are know to CMake.

`project-cmake-shell` is a wrapper around `project-shell` that enables using shells and environments appropriate to the development kit. When working with MSYS2/MINGW64 or the Windows Subsystem for Linux, this shell is not the usual Emacs shell and requires some magic that `project-cmake` already takes care of.

All these commands require that you have selected a build kit to build the software. This is a project-local value that you will be prompted for, and which will be saved in the `.project.el` file, together with other settings.

`project-cmake` can also help LSP servers by providing them with the right configuration flags on how to locate a project's build structure and build flags.  At this moment, this integration is only provided for `eglot` (see [project webage](https://github.com/joaotavora/eglot)), via the function `project-cmake-eglot-integration` which hooks into `eglot-ensure` and updates `eglot-server-programs`.

## Build kits

A build kit is a set of configuration flags for CMake that determine the type of C/C++ compiler and various other flags to build the system. It can be a very trivial definition, or it can be very complex and involve certain wrappers, depending on the platform.

### Linux

On Unix-like systems, `project-cmake` has a very simple logic. It always create a default kit, called `unix` which uses whatever compiler CMake decides. Then, if there are multiple compilers, it creates one kit for each of them, with names `unix-gcc`, `unix-clang`, `unix-icc`. It uses the same name (`gcc`, `clang`, `icc`) for the C and C++ front-ends, defining the `CC` and `CXX` environment variables.

### Windows

On Windows the usual thing is that we do not have compilers immediately available. In this case `project-cmake` offers these possibilities:
- Defining build kits for an existing [MSYS2 / MINGW64](https://www.msys2.org/) environment.
- Defining Unix-like build kits for any Linux distribution available via the [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install)
- Eventually, `project-cmake` should also be able to detect Microsoft Visual Studio C/C++ compilers, or other compilers made availably by Anaconda or different package managers.

## Usage

Let me briefly describe how I use this project. I have checked out this repository at a common location for my libraries, say `~/src/project-cmake`. I then make the project available to Emacs with the help of `use-package`, as follows
````
(use-package project-cmake
    :load-path "~/src/project-cmake/"
    :config
    (require 'eglot)
    (project-cmake-scan-kits)
    (project-cmake-eglot-integration))
````
For this to work, I have first downloaded [MSYS2](https://www.msys2.org/), installing a rather minimal set of packages which consists of the following ones (and others that my project depends on)
````
mingw64-ucrt-w64-x86_64-gcc
mingw64-ucrt-w64-x86_64-cmake
mingw64-ucrt-w64-x86_64-clang-extra-tools
````

For the `eglot` integration I use a very minimalistic configuration, as `project-cmake` already takes care of configuring the C/C++ language server for me:
````
(use-package eglot
  :ensure t
  :hook
  ((c-mode . eglot-ensure)
   (c++-mode . eglot-ensure))
  )
````

Note that in order for `eglot` to work with [`clangd`](https://clangd.llvm.org/), it may need the `compile_commands.json` generated by CMake at configuration time. When CMake has failed to configure or you did not have yet time to do it, `clangd` will not find that file. In that case you might have to restart the language server by calling `eglot-shutdown`, configuring the project with `project-cmake-configure` and reloading the C/C++ files.

## Project-local variables

`project-cmake` creates a file with name `.project.el` at the root of every project, in which it stores variables that determine how a project is configured and built.

The file has a format similar to the one below:
```elisp
;;; Project-local definitions.
;;; Saved on 2022-03-14T13:52:26
(defvar project-cmake-configuration-arguments '"-DTENSOR_OPTIMIZE=ON -DTENSOR_ARPACK=ON -DTENSOR_FFTW3=ON")
(defvar project-cmake-generator '"Ninja")
(defvar project-cmake-kit 'msys2-ucrt64)
```

Note that despite the use of `defvar`, the file is never directly evaluated. Instead, the statements are read and interpreted by `project-cmake`, storing the values into a local database.

In order to edit this database for the current project, you can use `project-cmake-edit-settings`. The values that result from this edition will be typically saved before Emacs exits, but you can also force writing those values into `.project.el` through the command `project-cmake-save-settings`.
