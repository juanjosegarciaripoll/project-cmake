;;; project-cmake.el --- Extension for project to use CMake as build system  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Author: Juan Jose Garcia-Ripoll
;; Maintainer: Juan Jose Garcia-Ripoll <juanjose.garciaripoll@gmail.com>
;; URL: https://github.com/juanjosegarciaripoll/project-cmake
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (project "0.3.0") project-local project-cmake-api)

;; MIT License

;; Copyright (c) 2022 Juan José García Ripoll

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:

;; This package is an extension to Emacs' own `project` package.  While
;; the latter understand project directories, can find files and has some
;; `project-compile` minimalistic function, but it does not understand build
;; systems.
;;
;; The package `project-cmake` incorporates the required logic to understand
;; that a project is to be configured, built and tested with CMake.  It also is
;; capable of recognizing different build kits, on platforms that support
;; it.  For instance, on Windows it can scan for MSYS2 / MINGW64 environments and
;; used the versions of CMake and the compiler supported by it, and eventually
;; it will also support Microsoft compilation environments.
;;
;; `project-cmake` also adds new key bindings to the `project-prefix-map`, with
;; the following default assignments
;;
;;    C-x p C   -  project-cmake-configure
;;    C-x p m   -  project-cmake-build
;;    C-x p t   -  project-cmake-test
;;
;; `project-cmake` can also help LSP servers by providing them with the
;; right configuration flags on how to locate a project's build structure
;; and build flags.  At this moment, this integration is only provided for
;; `eglot`, via the function `project-cmake-eglot-integration` which
;; hooks into `eglot-ensure` and updates `eglot-server-programs`.
;;

(require 'shell)
(require 'project)
(require 'project-local)
(require 'project-cmake-api)

(defgroup project-cmake nil
  "Project-assisted management of CMake builds.")

(defcustom project-cmake-variables
  '(project-cmake-build-type
	project-cmake-configuration-arguments
	project-cmake-generator
	project-cmake-jobs)
  "List of project-local variables that can be edited by the user")

(defcustom project-cmake-build-type "Debug"
  "Build type. Usually one of \"Debug\", \"Release\",
\"RelWithDebInfo\" or \"MinSizeRel\"."
  :type 'string
  :safe 'stringp)

(defcustom project-cmake-kit nil
  "C++ kit for building this project. It is recommended to leave
unset and use `project-cmake-select-kit` to select a kit for each
project."
  :type '(or null symbol)
  :safe (lambda (x) (or (null x) (symbolp x))))

(defcustom project-cmake-configuration-arguments nil
  "Arguments when invoking CMake for configuration."
  :type '(repeat string)
  :safe #'listp)

(defcustom project-cmake-generator nil
  "CMake generator and driver."
  :type '(choice (const nil :tag "No parallelization")
				 (string :tag "Generator"))
  :safe (lambda (x) (or (stringp x) (null x))))

(defcustom project-cmake-jobs nil
  "Number of jobs to use for building CMake projects.  If NIL,
this option is ignored.  Otherwise it should be an integer larger
than 0."
  :type '(choice (const nil :tag "No parallelization")
				 (integer :tag "Number of jobs"))
  :safe (lambda (x) (or (null x) (and (integerp x) (>= x 1)))))

(defcustom project-cmake-msys2-root "c:/msys64"
  "Location of the MSYS2 distribution."
  :type 'string
  :safe #'stringp)

(defcustom project-cmake-kits nil
  "CMake/C++/C build kits.  Ideally, it should be populated by
calling PROJECT-CMAKE-SCAN-KITS, but you can provide your own
definitions.  It is an association list between kit name (a
SYMBOL) and a property list containing one or more of the
following fields:

  :ENVIRONMENT - LIST

      List of environment variable assignments which are combined
      with `process-environment` when using CMake, CTest or other
      programs from this kit.

  :CMAKE - STRING

      Location of the CMake executable for this kit.
  :CTEST - STRING

      Location of the CTest executable for this kit.

  :CLANGD - STRING

      Location of Clangd in this kit.  Used for integration with
      `eglot` (see `project-cmake-eglot-integration`)

  :GDB - STRING

      Location of 'gdb' in this kit.  See `project-cmake-gdb`.

  :SHELL

      A single-argument function that defines the value of
      explicit-shell-file-name before calling project-shell,
      to select the right shell for this kit.
")

(defcustom project-cmake-ctest-buffer "*ctest*"
  "Name of buffer where tests are run."
  :type 'string
  :safe #'stringp)

(defcustom project-cmake-build-directory-name nil
  "Where CMake will configure and build the project.  If NIL, it
is derived from the kit's name.  Otherwise it is a path expanded
at the root of the project's directory."
  :type '(or null string)
  :type (lambda (x) (or (null x) (stringp x))))

(defun project-cmake-guess-environment (&optional program &rest args)
  "Guess the environment variables from the current shell"
  (let ((program (or program shell-file-name))
        (args (or args (list shell-command-switch "set")))
        output)
    (with-temp-buffer
      (unless (zerop (apply #'call-process program nil (current-buffer) nil args))
		(error "Unable to guess environment values from %s" program))
      (goto-char (point-min))
      (while (re-search-forward "^[A-Za-z0-9_]*=..*$" nil t)
        (push (match-string-no-properties 0) output)))
    output))


(defun project-cmake-build-directory ()
  "Return the full path to the build directory"
  (expand-file-name (or project-cmake-build-directory-name
                        (concat "build-" (symbol-name (project-cmake-kit-name))))
                    (project-root (project-current t))))

(defun project-cmake-source-directory ()
  "Return the full path to the project's source (or root) directory"
  (expand-file-name (project-root (project-current t))))


;;;
;;; INTEGRATION WITH WSL
;;;
;;; The following functions provide the detection and integration of MSYS2 /
;;; MINGW64 environments for building and compiling C and C++ codes.  Notably,
;;; the software in these packages requires environment variables that help them
;;; detect where other software components are stored.  Those environment
;;; variables cannot be set globally in Emacs and are instead stored in the
;;; kit's environment fields.
;;;

(defcustom project-cmake-wsl-shell-args '("bash" "--noediting" "-l" "-i")
  "Options to invoke MSYS2 bash. Note that you will see an error
message about ioctl that can be ignored.")

(defun project-cmake-wsl-invoke (&rest arguments)
  (if (executable-find "wsl")
	  (zerop (apply #'call-process "wsl" nil (current-buffer) nil arguments))
	nil))

(defun project-cmake-wsl-exec-path (executable)
  (with-temp-buffer
	(when (project-cmake-wsl-invoke "which" executable)
	  (car (split-string (buffer-string) "\n")))))

(defun project-cmake-wsl-list-distributions ()
  "Return a list of strings with all installed WSL distributions"
  (let ((project-environment))
	(with-temp-buffer
	  ;; The WSL by itself uses a weird encoding. However,
	  ;; Unix code invoked by it will use simpler encodings.
	  (let ((coding-system-for-read 'utf-16le)
			(coding-system-for-write 'utf-16le))
		(when (project-cmake-wsl-invoke "-l")
		  (let (distributions)
			(dolist (line (cdr (split-string (buffer-string) "\n")))
			  (let ((name (car (split-string line " "))))
				(unless (zerop (length name))
				  (push name distributions))))
			distributions))))))

(defvar explicit-wsl.exe-args nil)

(defun project-cmake-wsl-shell-launcher (wsl-type)
  (lambda (function-to-call)
	(let* ((explicit-shell-file-name "wsl.exe")
		   (explicit-wsl.exe-args (cl-list* "-d" wsl-type project-cmake-wsl-shell-args)))
	  (funcall function-to-call))))

(defun project-cmake-wsl-path (path)
  (let ((path (subst-char-in-string ?\\ ?/ (expand-file-name path))))
	(if (zerop (string-match "[a-z]:" path))
		(concat "/mnt/" (substring path 0 1) (substring path 2))
	  path)))

(defun project-cmake-wsl-kits ()
  (when (project-cmake-msys2-path 'msys)
	(let (all-kits)
      (dolist (wsl-type (project-cmake-wsl-list-distributions))
		(let ((kit-name (concat "wsl-" (downcase wsl-type))))
		  ;; Identification of the system may fail
		  (condition-case condition
			  (let* ((shell-launcher
					  (project-cmake-wsl-shell-launcher wsl-type))
					 (kit (project-cmake-build-kit kit-name
												   nil
												   shell-launcher
												   'project-cmake-wsl-exec-path)))
				(push (append kit `(:command-prefix ("wsl" "-d" ,wsl-type)
									:convert-path project-cmake-wsl-path))
					  all-kits))
			(error (message "Failed when configuring kit %s with condition %s"
							kit-name condition)))))
	  all-kits)))


;;;
;;; INTEGRATION WITH MSYS2
;;;
;;; The following functions provide the detection and integration of MSYS2 /
;;; MINGW64 environments for building and compiling C and C++ codes.  Notably,
;;; the software in these packages requires environment variables that help them
;;; detect where other software components are stored.  Those environment
;;; variables cannot be set globally in Emacs and are instead stored in the
;;; kit's environment fields.
;;;

(defun project-cmake-guess-msys2-environment (type)
  "Guess the environment variables from the MSYS2 shell, for the
given TYPE of MSYS2 build.  It can be one of the symbols msys,
mingw64, mingw32 or ucrt64."
  (let* ((default-environment (project-cmake-guess-environment))
         (msys2-type (upcase (symbol-name type)))
         (msys2-environment
		  (let ((process-environment (list "TERM=dumb"
                                           (concat "MSYSTEM="
                                                   msys2-type))))
            (project-cmake-guess-environment (project-cmake-msys2-bash)
                                             "-l" "-c" "cmd.exe /c set"))))
    (cl-set-difference msys2-environment default-environment)))

(defun project-cmake-msys2-path (type)
  (let ((path (expand-file-name
               (or (plist-get '(ucrt64 "ucrt64/bin"
								mingw64 "mingw64/bin"
								mingw32 "mingw32/bin"
								msys "usr/bin")
							  type)
				   (error "Unknown MSYS2 backend type %S" type))
               project-cmake-msys2-root)))
    (and (file-exists-p path) path)))

(defcustom project-cmake-msys2-bash-args '("--noediting" "-l" "-i")
  "Options to invoke MSYS2 bash. Note that you will see an error
message about ioctl that can be ignored.")

(defvar explicit-bash.exe-args nil)

(defun project-cmake-msys2-change-directory (path)
  (let ((buffer (current-buffer)))
	(when (comint-check-proc buffer)
	  (comint-send-string (get-buffer-process buffer)
						  (format "cd \"%s\"\n" path)))))

(defun project-cmake-msys2-shell-launcher (type)
  (let* ((msys2-path (project-cmake-msys2-path 'msys))
		 (shell-path (expand-file-name "bash.exe" msys2-path)))
	(and shell-path
		 (lambda (function-to-call)
		   (let* ((explicit-shell-file-name shell-path)
				  (default-directory (expand-file-name (project-cmake-kit-source-directory)))
				  (msystem (concat "MSYSTEM=" (upcase (symbol-name type))))
				  (process-environment (cl-list* "PS1=$ " "TERM=dumb" msystem process-environment))
				  (explicit-bash.exe-args project-cmake-msys2-bash-args))
			 (funcall function-to-call)
			 ;; The login option makes bash change to the home directory
			 ;; We must use 'cd' to move to the project's directory
			 (project-cmake-msys2-change-directory default-directory))))))

(defun project-cmake-msys2-bash ()
  (let ((msys-path (project-cmake-msys2-path 'msys)))
    (and msys-path (expand-file-name "bash.exe" msys-path))))

(defun project-cmake-msys2-kits ()
  (when (project-cmake-msys2-path 'msys)
	(let (all-kits)
      (dolist (msys-type '(msys mingw64 mingw32 ucrt64))
		(let ((kit-name (concat "msys2-" (symbol-name msys-type)))
			  (path (project-cmake-msys2-path msys-type)))
          (if path
			  ;; Identification of the system may fail
			  (condition-case condition
				  (let* ((environment (project-cmake-guess-msys2-environment msys-type))
						 (shell-launcher (project-cmake-msys2-shell-launcher msys-type))
						 (exec-path (list path)))
					(push (project-cmake-build-kit kit-name environment shell-launcher)
						  all-kits))
				(error (message "Failed when configuring kit %s with condition %s" kit-name condition)))
			(message "Cannot find kit %s" kit-name))))
	  all-kits)))


;;;
;;; INTEGRATION WITH VISUAL STUDIO
;;;
;;;

(defvar project-cmake-msvc-kits nil)

(defun project-cmake-msvc-find-kits ()
  (let* ((program-files (getenv "ProgramFiles"))
		 (program-files-x86 (getenv "ProgramFiles(x86)"))
		 (roots (append (and program-files (list program-files))
						(and program-files-x86 (list program-files-x86))))
		 (versions '(("vs17-buildtools" . "2017/BuildTools/")
					 ("vs17-community" . "2017/Community/")
					 ("vs17-enterprise" . "2017/Enterprise/")
					 ("vs22-buildtools" . "2022/BuildTools/")
					 ("vs22-community" . "2022/Community/")
					 ("vs22-enterprise" . "2022/Enterprise/")))
		 (architectures '(("-32" . "vcvars32.bat")
						  ("-64" . "vcvars64.bat")))
		 kits)
	(dolist (root roots)
	  (dolist (version versions)
		(dolist (arch architectures)
		  (let ((path (format "%s/Microsoft Visual Studio/%s/VC/Auxiliary/Build/%s"
							  root (cdr version) (cdr arch)))
				(name (intern (format "%s%s" (car version) (car arch)))))
			(when (file-exists-p path)
			  (push (cons name (subst-char-in-string ?\\ ?/ path)) kits))))))
	(setq project-cmake-msvc-kits kits)))

(defun project-cmake-msvc-vcvars (type)
  (or (cdr (assoc type project-cmake-msvc-kits))
	  (error "No Microsoft Visual Studio kit of type %s" type)))

(defun project-cmake-guess-msvc-environment (type)
  "Guess the environment variables from the MSVC shell, for the
given TYPE of MSVC build.  It can be one of the symbols msys,
mingw64, mingw32 or ucrt64."
  (let* ((default-environment (project-cmake-guess-environment))
		 (batch-file (project-cmake-msvc-vcvars type))
		 (env-command (format "call \"%s\" && set" batch-file))
         (msvc-environment
          (project-cmake-guess-environment "cmdproxy.exe" "/c" env-command)))
    (cl-set-difference msvc-environment default-environment)))

(defun project-cmake-msvc-shell-launcher (type)
  (and (assoc type project-cmake-msvc-kits)
	   (lambda (function-to-call)
		 (let* ((default-directory (expand-file-name (project-cmake-kit-source-directory)))
				(process-environment (project-cmake-kit-value :environment type process-environment)))
		   (funcall function-to-call)))))

(defun project-cmake-msvc-kits ()
  (let* ((all-kit-names (mapcar #'car (project-cmake-msvc-find-kits)))
		 all-kits)
    (dolist (kit-name all-kit-names)
	  ;; Identification of the system may fail
	  (condition-case condition
		  (let* ((process-environment (project-cmake-guess-msvc-environment kit-name))
				 (shell-launcher (project-cmake-msvc-shell-launcher kit-name))
				 (exec-path (split-string (getenv "PATH") ";")))
			(push (project-cmake-build-kit (symbol-name kit-name)
										   process-environment shell-launcher)
				  all-kits))
		(error (message "Failed when configuring kit %s with condition %s"
						kit-name condition)))
	  all-kits)))


;;;
;;; INTEGRATION WITH UNIX
;;;
;;; The unix environments are comparatively simple.  We just need to scan *one*
;;; set of variables, leaving to the user the task of adding extra kits.
;;;

(defun project-cmake-unix-kits ()
  (let* ((all-kits (list (project-cmake-build-kit "unix")))
         (compiler-alist '(("gcc" . (("cc" . "gcc")
                                     ("cxx" . "g++")))
                           ("clang" . (("cc" . "clang")
                                       ("cxx" . "clang++")))
                           ("icc" . (("cc" . "icc")
                                     ("cxx" . "icpc")))))
         (compiler-names (mapcar #'car compiler-alist))

         (all-compilers (delq nil (mapcar 'executable-find compiler-names))))
    (when (> (length all-compilers) 1)
      ;; If there are multiple compilers, create specialized kits
      (dolist (compiler all-compilers)
        (let* ((kit-name (concat "unix-" (file-name-base compiler)))
               (compiler-set (cdr (assoc (file-name-base compiler) compiler-alist)))
               (cc-compiler (cdr (assoc "cc" compiler-set)))
               (cxx-compiler (cdr (assoc "cxx" compiler-set)))
               (environment (cl-list* (format "CC=%s" (executable-find cc-compiler))
                                      (format "CXX=%s" (executable-find cxx-compiler))
                                      process-environment)))
          (push (project-cmake-build-kit kit-name environment)
                all-kits))))
    all-kits))

(defun project-cmake-build-kit (kit-name &optional environment shell-launcher
										 exec-find)
  (cl-labels ((kit-exec-find
			   (name)
			   (funcall (or exec-find 'executable-find) name)))
	`(,(intern kit-name)
      ,@(if (kit-exec-find "ninja")
			`(:cmake-generator "Ninja")
		  `(:cmake-generator "Unix Makefiles"))
	  ,@(and environment
			 `(:environment ,environment))
      ,@(let* ((cmake (kit-exec-find "cmake")))
          (and cmake
               `(:cmake ,cmake)))
      ,@(let* ((ctest (kit-exec-find "ctest")))
          (and ctest
               `(:ctest ,ctest)))
      ,@(let* ((clangd (kit-exec-find "clangd")))
          (and clangd
               `(:clangd ,clangd)))
      ,@(let* ((gdb (kit-exec-find "gdb")))
          (and gdb
               `(:gdb ,gdb)))
	  ,@(and shell-launcher
			 `(:shell ,shell-launcher))
      )))


;;;
;;; BUILD KITS
;;;
;;; A kit is an environment that contains CMake, some compilers and optionally
;;; other tools such as Clangd.  Kits are used to define separate build
;;; directories, to configure, build and test a project, and to define other
;;; properties required for language servers.
;;;

(defun project-cmake-scan-kits ()
  (interactive)
  (let (all-kits)
	(when (eq system-type 'windows-nt)
	  (setq all-kits (append (project-cmake-msys2-kits)
							 (project-cmake-wsl-kits)
							 (project-cmake-msvc-kits))))
	(when (member system-type '(gnu gnu/linux gnu/kfreebsd darwin))
	  (setq all-kits (project-cmake-unix-kits)))
    (unless all-kits
      (warn "No CMake/C++/C kits found in this computer"))
    (setq project-cmake-kits all-kits)))

(defun project-cmake-kit-convert-path (path)
  (funcall (or (project-cmake-kit-value :convert-path)
			   'identity)
		   path))

(defun project-cmake-kit-build-directory ()
  "Return the path for the project's build directory as understood by the build kit."
  (project-cmake-kit-convert-path
   (project-cmake-build-directory)))

(defun project-cmake-kit-source-directory ()
  "Return the path for the project's source directory as understood by the build kit."
  (project-cmake-kit-convert-path
   (project-cmake-source-directory)))

(defun project-cmake-kit-name ()
  "Return the symbol for the selected kit."
  (or (project-local-value (project-current t)
						   'project-cmake-kit)
	  (call-interactively #'project-cmake-select-kit)
	  (error "No build kit selected.")))

(defun project-cmake-kit (&optional kit)
  "Return the definition (name with property list) for the
selected kit, or NIL if it does not exist."
  (assoc (or kit (project-cmake-kit-name)) project-cmake-kits))

(defun project-cmake-kit-value (keyword &optional kit default)
  (let* ((plist (cdr (project-cmake-kit kit))))
    (if (plist-member plist keyword)
        (plist-get plist keyword)
      default)))

(defun project-cmake-kit-compilation-environment ()
  (append compilation-environment
          (project-cmake-kit-value :environment)))

(defun project-cmake-kit-debug-environment ()
  (append (project-cmake-kit-value :environment)
		  process-environment))

(defun project-cmake-kit-cmake-command (&rest arguments)
  (let ((cmake (project-cmake-kit-value :cmake)))
    (if cmake
        (append (list cmake)
                (project-cmake-kit-value :cmake-flags)
                arguments)
      (error "Cannot find CMake in current kit %s\nKit configuration:\n%s"
             (project-cmake-kit-name)
			 (project-cmake-kit)))))

(defun project-cmake-kit-wrap (command-list)
  (combine-and-quote-strings (append (project-cmake-kit-value :command-prefix)
									 command-list)))

(defun project-cmake-kit-compile (command-list &optional interactive-p)
  (let* ((compile-command (project-cmake-kit-wrap command-list))
		 (compilation-environment (project-cmake-kit-compilation-environment))
		 (compilation-buffer-name-function
          (or project-compilation-buffer-name-function
              compilation-buffer-name-function))
		 (default-directory (project-cmake-source-directory)))
	(message "compile-command: %S" compile-command)
	(if interactive-p
		(call-interactively #'compile)
	  (compile compile-command))))


;;;
;;; INTEGRATION WITH EGLOT
;;;
;;; `project-cmake` can hook into `eglot`, configuring it so that it can find
;;; language servers and provide them with the right arguments.  In particular,
;;; it gives them the location of the `compile_commands.json` database, which
;;; lists all sources and how they are built.  Without that database, language
;;; servers cannot properly find include files and decode C/C++/Fortran/... or
;;; any other sources.

(defun project-cmake-eglot-clangd-command-line (interactive-p)
  (let* ((build-directory (project-cmake-build-directory))
         (database (expand-file-name "compile_commands.json"
                                     build-directory)))
    (when (and interactive-p
               (not (file-exists-p database))
               (y-or-n-p "Compilation database for this project is missing.\nThis may affect how the language server performs.\nShall I reconfigure the project?"))
      (project-cmake-configure))
	(let ((clangd (project-cmake-kit-value :clangd)))
	  (if clangd
		  (list clangd
			"--clang-tidy" ;; Enable clang tidy checks
			(format "--compile-commands-dir=%s" (project-cmake-kit-build-directory)))
		(error "Cannot find clangd in current kit %s\nKit configuration:\n%S"
               (project-cmake-kit-name)
			   (project-cmake-kit)
			   )))))

(defun project-cmake-eglot-ensure-wrapper (orig-fun &rest args)
  "Intercept calls to `eglot-ensure` to verify it uses the right
LSP server and LSP server directory."
  (let ((process-environment (append process-environment
									 (project-cmake-kit-value :environment))))
	(apply orig-fun args)))

(defun project-cmake-eglot-integration ()
  "Customize the `eglot` environment so that it understands where
to find the C/C++ language servers, compile_commands.json and
other environment flags."
  (interactive)
  (require 'eglot)
  (add-to-list 'eglot-server-programs
			   '((c++-mode c-mode) . project-cmake-eglot-clangd-command-line))
  (advice-add 'eglot-ensure :around 'project-cmake-eglot-ensure-wrapper))


;;;
;;; CMAKE INTEGRATION
;;;
;;; `project-cmake` assumes that the current project is based on CMake,
;;; with the following structure:
;;;
;;; - There is a CMakeLists.txt file at the root of the project.
;;;
;;; - There will be a build directory where CMake will create all of the
;;;   project's assets (executables, libraries, etc), named by `project-cmake`
;;;   according to `project-cmake-build-directory-name`.
;;;
;;; - The project must be first configured, then built and then possibly tested
;;;   or installed.
;;;

;;;
;;; Utility functions
;;;

(defun project-cmake-guess-generator ()
  (or (project-local-value (project-current t) 'project-cmake-generator)
      (project-cmake-kit-value :cmake-generator)
      "Makefiles"))

(defun project-cmake-ensure-cmakelist ()
  "Ensure that the current project has a valid CMakeLists.txt file."
  (let ((root (project-cmake-source-directory)))
	(unless (file-exists-p (expand-file-name "CMakeLists.txt" root))
	  (error (format "Project at %s does not have a CMakeLists.txt"
					 root)))))

(defun project-cmake-build-type ()
  (project-local-value (project-current t) 'project-cmake-build-type))

(defun project-cmake-parse-configuration-arguments ()
  (let ((arguments (project-local-value (project-current t) 'project-cmake-configuration-arguments)))
	(cond ((listp arguments)
		   arguments)
		  ((stringp arguments)
		   (list arguments)))))

(defun project-cmake-kit-configure-command ()
  "Return the command line to configure the project using
CMake.  The function guesses the project source directory, project
build directory (see `project-cmake-build-directory-name`) and
the CMake generator."
  (let ((args (project-cmake-parse-configuration-arguments)))
	(apply #'project-cmake-kit-cmake-command
           "-G" (project-cmake-guess-generator)
		   (concat "-DCMAKE_BUILD_TYPE:STRING=" (project-cmake-build-type))
		   "-DCMAKE_EXPORT_COMPILE_COMMANDS=1"
           (concat "-S" (project-cmake-kit-source-directory))
           (concat "-B" (project-cmake-kit-build-directory))
		   args)))

(defun project-cmake-ensure-configured ()
  "Ensure that the project has been configured before building it."
  (let ((build (project-cmake-build-directory)))
	(or (file-exists-p build)
		(when (y-or-n-p (format "Project has not been configured.  Do you want to configure it first?"))
		  (project-cmake-configure)
		  t))))

(defun project-cmake-kit-build-command (&optional target clean)
  "Return the command line to build the project using CMake.  If
CLEAN is not NIL, specify that the project is recompiled from
scratch."
  (let* ((project-cmake-jobs (project-local-value (project-current t)
												 'project-cmake-jobs))
		 (args (list "--build" (project-cmake-kit-build-directory)
					 "--target" target)))
	(when project-cmake-jobs
      (setq args (append args
						 (list "--parallel" (format "%s" project-cmake-jobs)))))
    (when clean
      (setq args (append args (list "--clean-first"))))
    (apply #'project-cmake-kit-cmake-command args)))

(defun project-cmake-kit-install-command ()
  "Return the command line to install the project using CMake.  If
CLEAN is not NIL, specify that the project is recompiled from
scratch."
  (let ((args (list "--install" (project-cmake-kit-build-directory))))
    (apply #'project-cmake-kit-cmake-command args)))

(defun project-cmake-kit-cmake-find-test-directory ()
  "Find the first directory with a CTest*.cmake file understanding that
it will contain all files for CTest."
  (let* ((root (project-cmake-build-directory))
         (candidates (directory-files-recursively root "CTest.*.cmake"))
         output)
    (dolist (path candidates)
      (let ((dir (file-name-directory path)))
        (unless (string-match "[/\\]_deps[/\\]" dir)
          (setq output dir))))
    (project-cmake-kit-convert-path output)))

(defun project-cmake-kit-ctest-command (&optional verbose)
  "Return the command line to run CTest in the right directory,
possibly in VERBOSE mode."
  (let* ((project-cmake-jobs (project-local-value (project-current t)
												 'project-cmake-jobs))
		 (ctest-directory (project-cmake-kit-cmake-find-test-directory))
		 (ctest-args (cl-list* "--test-dir" ctest-directory "--output-on-failure"
							   (and verbose '("-VV"))))
		 (ctest (or (project-cmake-kit-value :ctest)
					(error "Cannot find CTest in current kit %s"
						   (project-cmake-kit-name)))))
	(when project-cmake-jobs
      (setq ctest-args
			(cl-list* "-j" (format "%s" project-cmake-jobs) ctest-args)))
	(cl-list* ctest ctest-args)))

(defun project-cmake-remove-build-directory ()
  (let ((build-directory (project-cmake-build-directory)))
    (when (and (file-exists-p build-directory)
               (y-or-n-p (format "Delete directory %s" build-directory)))
      (delete-directory build-directory t))))


;;;
;;; Top-level user interaction
;;;

(defun project-cmake-configure (&optional clean)
  "Configure a project tree using CMake and the current kit.  If
provided an argument, it can optionally remove an existing build
directory to start from scratch."
  (interactive "P")
  (when clean
	(project-cmake-remove-build-directory))
  (let ((compilation-mode-hook compilation-mode-hook))
	(project-cmake-api-query-prepare)
	(add-to-list 'compilation-mode-hook
				 (lambda ()
				   (setq-local compilation-finish-functions
							   (cons (lambda (buffer status)
									   (project-cmake-api-query-complete))
									 compilation-finish-functions))))
	(project-cmake-kit-compile (project-cmake-kit-configure-command))))

(defun project-cmake-build (&optional clean)
  "Build a project tree using CMake and the current kit.  If
provided an argument, it can recompile the whole project from
scratch, preserving the existing configuration."
  (interactive "P")
  (unless (project-cmake-ensure-configured)
	(error "Cannot build project that has not been configured first."))
  (project-cmake-kit-compile (project-cmake-kit-build-command
							  (project-cmake-api-choose-target)
							  clean)))

(defun project-cmake-install ()
  "Build a project tree using CMake and the current kit.  If
provided an argument, it can recompile the whole project from
scratch, preserving the existing configuration."
  (interactive)
  (unless (project-cmake-ensure-configured)
	(error "Cannot build project that has not been configured first."))
  (project-cmake-kit-compile (project-cmake-kit-install-command)))

(defun project-cmake-test (&optional verbose)
  "Run the tests in a using CTest and the current kit."
  (interactive "P")
  (unless (project-cmake-ensure-configured)
	(error "Cannot build project that has not been configured first."))
  (let* ((default-directory (project-cmake-build-directory))
		 (buffer-name (format "*CTest %s*" default-directory))
		 (compilation-buffer-name-function (lambda (mode) buffer-name)))
	(project-cmake-kit-compile (project-cmake-kit-ctest-command verbose))
    (switch-to-buffer-other-window (get-buffer buffer-name))))

(defun project-cmake-shell ()
  "Run a shell which is appropriate for the given compilation kit."
  (interactive)
  (require 'comint)
  (let ((default-directory (project-cmake-source-directory))
		(shell-launcher (project-cmake-kit-value :shell)))
	(if shell-launcher
		(funcall shell-launcher 'project-shell-fix)
	  (project-shell-fix))))

(defun project-shell-fix ()
  "Fixed version for project-shell"
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-cmake-source-directory))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
	(if (comint-check-proc shell-buffer)
        (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
      (shell (or shell-buffer default-project-shell-name)))))

(defun project-cmake-select-kit (kit-name)
  "Select a kit for this project."
  (interactive
   (let ((all-kits (project-local-value (project-current t)
										'project-cmake-kits)))
	 (when (and (null all-kits)
				(y-or-n-p "Scan computer for build kits?"))
	   (setq all-kits (project-cmake-scan-kits)))
	 (unless all-kits
	   (error "There are no build kits to choose from in this computer."))
	 (list (completing-read "Project build kit:"
							(mapcar 'car all-kits)))))
  (when kit-name
	(project-local-set (project-current t) 'project-cmake-kit (intern kit-name))))


(defun project-cmake-debug ()
  "Run a debugger on a selected target. Run the toolkit's
debugger passing a compiled executable target as argument. The
target is selected from the list of CMake executable targets. If
the current buffer belongs to one such executable target, it is
passed as initial value in the selection list.

Note: This function defaults to calling the old interface GUD-GDB
on the Windows platform."
  (interactive)
  (require 'comint)
  (let ((default-directory (project-cmake-source-directory))
		(target (project-cmake-kit-convert-path
				 (project-cmake-api-choose-executable-file)))
        (process-environment (project-cmake-kit-debug-environment))
		(gdb-executable (project-cmake-kit-value :gdb)))
	(if gdb-executable
		(if (eq system-type 'windows-nt)
			(gud-gdb (project-cmake-kit-wrap (list gdb-executable "--fullname" target)))
		  (gdb (project-cmake-kit-wrap (list gdb-executable "-i=mi" target))))
	  (error "No GDB installed in kit %s."))))

(defun project-cmake-run-target ()
  "selecting target and run it inside a compilation-mode buffer."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-cmake-source-directory))
         (buffer-name (format "*Run Target %s*" default-directory))
         (compilation-buffer-name-function (lambda (mode) buffer-name))
         (target (project-cmake-kit-convert-path
                  (project-cmake-api-choose-executable-file)))
         (process-environment (project-cmake-kit-debug-environment))
         )
    (compile (project-cmake-kit-wrap (list target)))))

(defun project-cmake-edit-settings (variable)
  (interactive (list (completing-read "Project variable: "
   				      project-cmake-variables
   				      nil t)))
  (project-local-edit (project-current t) (intern variable)))

(defun project-cmake-save-settings (&optional all-projects)
  (interactive)
  (project-local-save-records all-projects))

(defun project-cmake-load-settings (&optional no-confirm)
  (interactive)
  (let ((current-project (project-current t)))
	(when (or no-confirm (y-or-n-p (format "Reload settings for project %s"
										   current-project)))
	  (project-local-load-record current-project))))


;;
;; Default key bindings into the `project` map
;;
(define-key project-prefix-map "t" 'project-cmake-test)
(define-key project-prefix-map "m" 'project-cmake-build)
(define-key project-prefix-map "C" 'project-cmake-configure)
(define-key project-prefix-map "s" 'project-cmake-shell)
(define-key project-prefix-map "SK" 'project-cmake-select-kit)
(define-key project-prefix-map "SE" 'project-cmake-edit-settings)
(define-key project-prefix-map "SS" 'project-cmake-save-settings)
(define-key project-prefix-map "SL" 'project-cmake-load-settings)
(define-key project-prefix-map "U" 'project-cmake-debug)

(provide 'project-cmake)
