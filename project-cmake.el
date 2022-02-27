;;; project-cmake.el --- Extension for project to use CMake as build system  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Juan Jose Garcia-Ripoll

;; Version: 0.1
;; Author: Juan Jose Garcia-Ripoll
;; Maintainer: Juan Jose Garcia-Ripoll <juanjose.garciaripoll@gmail.com>
;; URL: https://github.com/juanjosegarciaripoll/project-cmake
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (project "0.3.0"))

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

(require 'project)

(setq debug-on-error t)

(defcustom project-cmake-default-kit nil
  "Default C++ kit for building project.  It is a symbol naming one
of the kits available in `project-cmake-kits`.")

(defcustom project-cmake-config-args
  '("-DCMAKE_EXPORT_COMPILE_COMMANDS=1")
  "Arguments when invoking CMake for configuration.")

(defcustom project-cmake-generator nil
  "CMake generator and driver.")

(defcustom project-cmake-jobs nil
  "Number of jobs to use for building CMake projects.  If NIL,
this option is ignored.  Otherwise it should be an integer larger
than 0.")

(defcustom project-cmake-msys2-root "c:/msys64"
  "Location of the MSYS2 distribution.")

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
")

(defcustom project-cmake-ctest-buffer "*ctest*"
  "Name of buffer where tests are run.")

(defcustom project-cmake-build-directory-name nil
  "Where CMake will configure and build the project.  If NIL, it
is derived from the kit's name.  Otherwise it is a path expanded
at the root of the project's directory.")

(defun project-cmake-guess-environment (&optional program &rest args)
  "Guess the environment variables from the current shell"
  (let ((program (or program shell-file-name))
        (args (or args (list shell-command-switch "set")))
        output)
    (with-temp-buffer
      (unless (zerop (apply #'call-process program nil (current-buffer) nil args))
		(message "%s" (buffer-string))
		(error "Unable to guess environment values from %s" program))
      (goto-char (point-min))
      (while (re-search-forward "^[A-Za-z0-9_]*=.*$" nil t)
        (push (match-string-no-properties 0) output)))
    output))


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
                                             "-l" "-i" "-c" "cmd.exe /c set"))))
    (cl-set-difference msys2-environment default-environment)))

(defun project-cmake-msys2-path (type)
  (let ((path (expand-file-name
               (plist-get '(ucrt64 "ucrt64/bin"
                            mingw64 "mingw64/bin"
                            mingw32 "mingw32/bin"
                            msys "usr/bin")
                          type)
               project-cmake-msys2-root)))
    (and (file-exists-p path) path)))

(defun project-cmake-msys2-bash ()
  (let ((msys-path (project-cmake-msys2-path 'msys)))
    (and msys-path (expand-file-name "bash.exe" msys-path))))

(defun project-cmake-msys2-kits ()
  (when (project-cmake-msys2-path 'msys)
	(let (all-kits)
      (dolist (msys-type '(msys mingw64 mingw32 ucrt64))
		(let ((path (project-cmake-msys2-path msys-type)))
          (when path
			;; Identification of the system may fail
			(condition-case nil
				(let* ((kit-name (concat "msys2-" (symbol-name msys-type)))
					   (environment (project-cmake-guess-msys2-environment msys-type))
					   (exec-path (list path)))
				  (push (project-cmake-build-kit kit-name environment)
						all-kits))
			  (error nil)))))
	  all-kits)))


;;;
;;; INTEGRATION WITH UNIX
;;;
;;; The unix environments are comparatively simple.  We just need to scan *one*
;;; set of variables, leaving to the user the task of adding extra kits.
;;;

(defun project-cmake-unix-kits ()
  (let (all-kits)
    (dolist (compiler '("gcc" "clang"))
	  (when (executable-find compiler)
		(let ((kit-name (concat "unix-" compiler)))
          (push (project-cmake-build-kit kit-name)
				all-kits)))))
	all-kits)

(defun project-cmake-build-kit (kit-name &optional environment)
  `(,(intern kit-name)
	,@(and environment
		`(:environment ,environment))
    ,@(let* ((cmake (executable-find "cmake")))
        (and cmake
             `(:cmake ,cmake)))
    ,@(let* ((ctest (executable-find "ctest")))
        (and ctest
             `(:ctest ,ctest)))
    ,@(let* ((clangd (executable-find "clangd")))
        (and clangd
             `(:clangd ,clangd)))
    ,@(if (executable-find "ninja")
          `(:cmake-generator "Ninja")
		`(:cmake-generator "Unix Makefiles"))
    ))


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
	  (setq all-kits (project-cmake-msys2-kits)))
	(when (member system-type '(gnu gnu/linux gnu/kfreebsd darwin))
	  (setq all-kits (project-cmake-unix-kits)))
    (unless all-kits
      (warn "No CMake/C++/C kits found in this computer"))
    (setq project-cmake-kits all-kits)))

(defun project-cmake-kit-build-directory ()
  (expand-file-name (or project-cmake-build-directory-name
                        (concat "build-" (symbol-name (project-cmake-kit-name))))
                    (project-root (project-current t))))

(defun project-cmake-kit-source-directory ()
  (expand-file-name (project-root (project-current t))))

(defun project-cmake-kit-name ()
  "Return the symbol for the selected kit."
  (or project-cmake-default-kit
      (car (cl-first project-cmake-kits))))

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

(defun project-cmake-kit-cmake-command (&rest arguments)
  (let ((cmake (project-cmake-kit-value :cmake)))
    (if cmake
        (combine-and-quote-strings
         (append (list cmake)
                 (project-cmake-kit-value :cmake-flags)
                 arguments))
      (error "Cannot find CMake in current kit %s\nKit configuration:\n%s"
             (project-cmake-kit-name)
			 (project-cmake-kit)))))


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
  (let* ((build-directory (project-cmake-kit-build-directory))
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
			(format "--compile-commands-dir=%s" build-directory))
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
  (or project-cmake-generator
      (project-cmake-kit-value :cmake-generator)
      "Makefiles"))

(defun project-cmake-ensure-cmakelist ()
  "Ensure that the current project has a valid CMakeLists.txt file."
  (let ((root (project-cmake-kit-source-directory)))
	(unless (file-exists-p (expand-file-name "CMakeLists.txt" root))
	  (error (format t "Project at %s does not have a CMakeLists.txt"
					 root)))))

(defun project-cmake-kit-configure-command ()
  "Return the command line to configure the project using
CMake.  The function guesses the project source directory, project
build directory (see `project-cmake-build-directory-name`) and
the CMake generator."
  (apply #'project-cmake-kit-cmake-command
         "-G" (project-cmake-guess-generator)
         (concat "-H" (project-cmake-kit-source-directory))
         (concat "-B" (project-cmake-kit-build-directory))
         project-cmake-config-args))

(defun project-cmake-ensure-configured ()
  "Ensure that the project has been configured before building it."
  (let ((build (project-cmake-kit-build-directory)))
	(or (file-exists-p build)
		(when (y-or-n-p (format t "Project has not been configured.  Do you want to configure it first?"))
		  (project-cmake-configure)
		  t))))

(defun project-cmake-kit-build-command (&optional clean)
  "Return the command line to build the project using CMake.  If
CLEAN is not NIL, specify that the project is recompiled from
scratch."
  (let ((args (list "--build" (project-cmake-kit-build-directory))))
    (when project-cmake-jobs
      (setq args (append (list "-j" (format "%s" project-cmake-jobs))
                         args)))
    (when clean
      (setq args (append args (list "--clean-first"))))
    (apply #'project-cmake-kit-cmake-command args)))

(defun project-cmake-kit-install-command ()
  "Return the command line to install the project using CMake.  If
CLEAN is not NIL, specify that the project is recompiled from
scratch."
  (let ((args (list "--install" (project-cmake-kit-build-directory))))
    (when project-cmake-jobs
      (setq args (append (list "-j" (format "%s" project-cmake-jobs))
                         args)))
    (apply #'project-cmake-kit-cmake-command args)))

(defun project-cmake-kit-cmake-find-test-directory ()
  "Find the first directory with a CTest*.cmake file understanding that
it will contain all files for CTest."
  (let* ((root (project-cmake-kit-build-directory))
         (candidates (directory-files-recursively root "CTest.*.cmake"))
         output)
    (dolist (path candidates)
      (let ((dir (file-name-directory path)))
        (unless (string-match "[/\\]_deps[/\\]" dir)
          (setq output dir))))
    output))

(defun project-cmake-remove-build-directory ()
  (let ((build-directory (project-cmake-kit-build-directory)))
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
  (let* ((compile-command (project-cmake-kit-configure-command))
         (compilation-environment (project-cmake-kit-compilation-environment)))
	(message "compilation-environment:\n%S" compilation-environment)
    (project-compile)))

(defun project-cmake-build (&optional clean)
  "Build a project tree using CMake and the current kit.  If
provided an argument, it can recompile the whole project from
scratch, preserving the existing configuration."
  (interactive "P")
  (unless (project-cmake-ensure-configured)
	(error "Cannot build project that has not been configured first."))
  (let* ((compile-command (project-cmake-kit-build-command clean))
         (compilation-environment (project-cmake-kit-compilation-environment)))
    (project-compile)))

(defun project-cmake-install ()
  "Build a project tree using CMake and the current kit.  If
provided an argument, it can recompile the whole project from
scratch, preserving the existing configuration."
  (interactive "P")
  (unless (project-cmake-ensure-configured)
	(error "Cannot build project that has not been configured first."))
  (let* ((compile-command (project-cmake-kit-install-command))
         (compilation-environment (project-cmake-kit-compilation-environment)))
    (project-compile)))

(defun project-cmake-test ()
  "Run the tests in a using CTest and the current kit."
  (interactive)
  (let* ((ctest-directory (project-cmake-kit-cmake-find-test-directory))
         (ctest-args (list "--test-dir" ctest-directory))
         (default-directory (project-cmake-kit-build-directory))
         (process-environment (project-cmake-kit-compilation-environment))
         (outbuf (get-buffer-create project-cmake-ctest-buffer)))
    (with-current-buffer outbuf
      (delete-region (point-min) (point-max))
      (display-buffer outbuf '(nil (allow-no-window . t)))
      (insert ";;; Running CTest\n")
      (switch-to-buffer-other-window outbuf)
      (end-of-buffer)
      (apply #'start-process
             "CTest"
             (current-buffer)
             (or (project-cmake-kit-value :ctest) "ctest")
             ctest-args))))

;;
;; Default key bindings into the `project` map
;;
(define-key project-prefix-map "t" 'project-cmake-test)
(define-key project-prefix-map "m" 'project-cmake-build)
(define-key project-prefix-map "C" 'project-cmake-configure)

(provide 'project-cmake)
