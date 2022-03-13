;;; project-cmake.el --- Extension for project to use CMake as build system  -*- lexical-binding: t; -*-

;; Version: 0.1
;; Author: Juan Jose Garcia-Ripoll
;; Maintainer: Juan Jose Garcia-Ripoll <juanjose.garciaripoll@gmail.com>
;; URL: https://github.com/juanjosegarciaripoll/project-cmake
;; Keywords: convenience, languages
;; Package-Requires: ((emacs "26.1") (project "0.3.0"))

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

;; This package supplies `project-cmake` with tools to query CMake and obtain
;; a list of all targets, separated by executable, library and custom target
;; types.

(defvar project-cmake-api-database (make-hash-table :test 'equal))

(cl-defstruct project-cmake-targets
  executable-targets
  library-targets
  all-targets
  all-target-names
  sources)

(defun project-cmake-api-query-filename ()
  (expand-file-name ".cmake/api/v1/query/codemodel-v2"
					(project-cmake-kit-build-directory)))

(defun project-cmake-api-reply-directory ()
  (expand-file-name ".cmake/api/v1/reply"
					(project-cmake-kit-build-directory)))

(defun project-cmake-api-prepare-query-file ()
  (let* ((query-filename (project-cmake-api-query-filename))
		 (directory (file-name-directory query-filename)))
	(unless (file-exists-p query-filename)
	  (unless (file-exists-p directory)
		(mkdir directory 'parents))
	  (with-temp-buffer
		(write-region (point-min) (point-max) query-filename)))
	query-filename))

(defun project-cmake-api-list-reply-files ()
  (let ((reply-directory (project-cmake-api-reply-directory)))
	(message "Reply directory: %s" reply-directory)
	(message "Exists: %s" (file-exists-p reply-directory))
	(message "List: %s" (directory-files reply-directory 'full-name ".*\\.json"))
	(and (file-exists-p reply-directory)
		 (directory-files reply-directory 'full-name ".*\\.json"))))

(defun project-cmake-api-empty-reply-directory ()
  (mapc 'delete-file (project-cmake-api-list-reply-files)))

(defun project-cmake-api-read-json (file)
  (with-temp-buffer
	(insert-file-contents file)
	(json-parse-buffer :object-type 'plist :array-type 'list)))

(defun project-cmake-api-target-type (target)
  (plist-get target :type))

(defun project-cmake-api-target-name (target)
  (plist-get target :name))

(defun project-cmake-api-target-sources (target)
  (plist-get target :sources))

(defun project-cmake-api-source-path (target)
  (plist-get target :path))

(defun project-cmake-api-project-database ()
  (gethash (project-current t) project-cmake-api-database nil))

(defun project-cmake-api-current-buffer-targets ()
  (let* ((file-name (expand-file-name (buffer-file-name (current-buffer))))
		 (database (project-cmake-api-project-database)))
	(and database
		 (gethash file-name (project-cmake-targets-sources database) nil))))

(defun project-cmake-api-choose-target ()
  (let ((database (project-cmake-api-project-database)))
	(completing-read "CMake target: "
					 (project-cmake-targets-all-target-names database))))


(defun project-cmake-api-read-targets (reply-files)
  (let ((source-directory (project-cmake-kit-source-directory))
		(sources (make-hash-table :test 'equal))
		(all-target-names '("all" "clean"))
		executables
		libraries
		all-targets)
	(dolist (file reply-files)
	  (let* ((json (project-cmake-api-read-json file))
			 (type (project-cmake-api-target-type json)))
		(cond ((null type)
			   (setq json nil))
			  ((string= type "EXECUTABLE")
			   (push json executables))
			  ((string-match-p type ".*LIBRARY")
			   (push json libraries)))
		(when json
		  (push json all-targets)
		  (push (project-cmake-api-target-name json) all-target-names)
		  (dolist (source (project-cmake-api-target-sources json))
			(let ((path (expand-file-name (project-cmake-api-source-path source)
										  source-directory)))
			  (puthash path json sources))))))
	(when all-targets
	  (puthash (project-current t)
			   (make-project-cmake-targets
				:executable-targets executables
				:library-targets libraries
				:all-targets all-targets
				:all-target-names (sort all-target-names #'string<)
				:sources sources)
			   project-cmake-api-database))))

(defun project-cmake-api-query-prepare ()
  (project-cmake-api-prepare-query-file)
  (project-cmake-api-empty-reply-directory))

(defun project-cmake-api-query-complete ()
  (let ((reply-files (project-cmake-api-list-reply-files)))
	(message "Reply files: %s" reply-files)
	(if reply-files
		(project-cmake-api-read-targets reply-files)
	  (warn "CMake project contains no targets or does not support file API."))))

(provide 'project-cmake-api)
