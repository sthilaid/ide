;;; ide.el --- minor mode for working is solution projets  -*- Mode: Emacs-Lisp -*-
;;
;; Copyright (C) 2018 David St-Hilaire
;;
;; ide mode is free software: you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; To install, place ide.el in your path with
;;
;;	(add-to-list 'load-path "<path-to-elisp>/ide")
;;
;; then add this line to your .emacs file:
;;
;;	(require 'ide)
;;
;; The main functions are:
;;	- ide-change-solution: sets your current project
;;	- ide-find-file: find file in project
;;	- ide-find-other-file: find the next file related to the current file (.h/.cpp)
;;
;; See the keymap below for more interesting functions and their shortcuts.
;;

;; Can be customized to set a default projecto to load
(defcustom ide-default-current-project ""
  "The name of your default current project file"
  :type 'string
  :group 'ide)

;; can be wet to a function that takes a file as parameter and outputs the next
;; file when cycling files with alt-o (ide-find-other-file)
(defcustom ide-custom-get-next-file nil
  "Funciton that can be used to override the default behaviour of (ide-find-other-file)"
  :type 'function
  :group 'ide)

(defcustom ide-msbuild-path "\"C:/Program Files (x86)/MSBuild/14.0/Bin/MSBuild.exe\""
  "Defines the path to MSBuild, used to compile visual studio solutions"
  :type 'string
  :group 'ide)

(defcustom ide-vs-configurations '("Debug" "DebugGame" "Development" "Development Client" "Development Server" "Development Editor" "Shipping Client" "Shipping Server" "Test Client" "Test Server" "Test")
  "Defines the list of configurations available in the visual studio solution."
  :type 'sexp
  :group 'ide)

(defcustom ide-vs-platforms '("HTML5" "IOS" "Mac" "TVOS" "Win32" "Win64")
  "Defines the list of platforms available in the visual studio solution."
  :type 'sexp
  :group 'ide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide state manipulation

;; will hold the current project file path (set by ide-change-solution)
(defvar ide-current-project)

;; will hold the internal data used by ide-mode
(defvar ide-data)

(defun ide-solution ()
  "Shows what is the current solution file."
  (interactive)
  (message
   (if ide-current-project
	   ide-current-project
	 "No solutions are active, set one with 'ide-change-solution'")))

(defun ide-data-file-paths ()
  (elt ide-data 0))

(defun ide-data-file-names ()
  (elt ide-data 1))

(defun ide-data-projects ()
  (elt ide-data 2))

(defun ide-data-project-paths ()
  (elt ide-data 3))

(defun ide-data-append-file-path (path)
  (aset ide-data 0 (cons path (ide-data-file-paths))))

(defun ide-data-append-file-name (file)
  (aset ide-data 1 (cons file (ide-data-file-names))))

(defun ide-data-append-project-name (project)
  (aset ide-data 2 (cons project (ide-data-projects))))

(defun ide-data-append-project-path (project)
  (aset ide-data 3 (cons project (ide-data-project-paths))))

(defun ide-data-size ()
  (length (ide-data-file-names)))

(defun ide-reset-data ()
  "Resets all data used by ide-mode"
  (interactive)
  (setq ide-current-project nil)
  (setq ide-data (vector '() '() '() '()))
  (if (and (boundp 'ide-mode)
		   (eval 'ide-mode))
	  (ide-mode -1))
  (message "ide-data was reset..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide change project

(defun ide-change-solution (solution-file)
  "Will set the solution file as the current solution for this mode."
  (interactive (list (let ((default-path (if ide-current-project
											 (file-name-directory ide-current-project)
										   (file-name-directory (buffer-file-name (current-buffer)))))
						   (default-project (if ide-current-project
												(file-name-nondirectory ide-current-project)
											  "")))
					   (ido-read-file-name "solution file: " default-path default-project t default-project))))
  (condition-case ex
	  (progn
		(if (not (file-exists-p solution-file))
			(throw 'ide-error "invalid solution file, does not exists..."))

		(let ((absolute-solution-file (expand-file-name solution-file)))
		  (ide-reset-data)
		  (setq ide-current-project absolute-solution-file)
		  (ide-parse-current-solution)
		  (let* ((file-count (ide-data-size))
				 (msg (concat "Now using project file: " (file-name-nondirectory absolute-solution-file)
							  " [parsed " (number-to-string file-count) " files]")))
			(message msg)
			(ide-mode t)
			msg)))
	
	(ide-error
	 (ide-reset-data)
	 (ide-mode -1)
	 (message (cadr ex)))))

;;(ide-change-solution "e:/UnrealEngine/UE4.sln")

(defun ide-parse-current-solution ()
  (if (or (not (boundp 'ide-current-project))
		  (not ide-current-project))
	  (throw 'ide-error "need to setup the current project first..."))

  (message "Parsing solution file...")
  (cond
   ((ide-is-vs-solution ide-current-project)				(ide-parse-vs-solution ide-current-project))
   ((ide-is-xcode-solution ide-current-project)				(ide-parse-xcode-solution ide-current-project))
   ((ide-try-to-parse-text-solution ide-current-project)	t)
   (t (throw 'ide-error (concat "unsupported solution type for project file: "
								(file-name-nondirectory ide-current-project))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide utils

(defun ide-get-substrings (str)
  (let ((start nil)
		(substrings '()))
	(cl-loop for current-char across str
			 for index to (length str)
			 do (if (= current-char ?\")
					(if start
						(progn (setq substrings (cons (substring str (+ start 1) index) substrings))
							   (setq start nil))
					  (setq start index))))
	substrings))

;;(ide-get-substrings "test \"hello.vcxproj\" \"test\"")

(defun ide-parse-file-by-line (file line-parser-function)
  (let ((buffer (get-buffer-create (concat "*temp-" file " " (number-to-string (random)) "*"))))
	(with-current-buffer buffer
	  (unwind-protect
		  (progn (insert-file-contents file)
				 (goto-char (point-min))
				 (cl-loop until (>= (point) (point-max))
						  for line-num = 0 then (+ line-num 1)
						  do (progn (beginning-of-line)
									(set-mark (point))
									(end-of-line)
									(let ((line-str (buffer-substring (region-beginning) (region-end))))
									  (funcall line-parser-function line-str))
									(forward-line)
									line-num)))
		(kill-buffer buffer)))))

(defun ide-get-project-arg (use-buffer-context-as-default?)
  (let* ((projects (seq-uniq (ide-data-projects)))
		 (project-options (cl-loop for p in projects
								   for i from 1 to (+ (length projects) 1)
								   collect (list p i)))
		 ;; try to figure the project from the file, unless specified no to
		 ;; (then use the latest history entry if possible...
		 (default-project
		   (if (and (not use-buffer-context-as-default?)
					(boundp 'ide-project-history)
					(listp (eval 'ide-project-history)))
			   (car (eval 'ide-project-history))
			 (let* ((name (buffer-name (current-buffer)))
					(buffer-project (cl-loop for f in (ide-data-file-names)
											 for i from 0 to (ide-data-size)
											 if (string= f name) return (elt (ide-data-projects) i) end)))
			   (if (not buffer-project) (car projects) buffer-project)))))
	
	(let ((project-choice (completing-read (concat "project (default: " default-project "): ")
										   projects nil t "" 'ide-project-history)))
						 (if (string= project-choice "") default-project project-choice))))
;;(ide-get-project-arg nil)

(defun ide-read-with-last-value (id &optional options)
  (let* ((history-sym	(intern (concat "ide-compile-project-" id "-history")))
		 (history		(if (boundp history-sym) (eval history-sym) '()))
		 (last-value	(if (consp history) (car history) (if options (car options) "")))
		 (input-options	(cl-loop for option in options
								 for i from 1 to (+ (length options) 1)
								 collect (cons option i)))
		 (input			(if options
							(completing-read (concat id "(default: " last-value "): ") input-options nil t "" history-sym)
						  (read-string (concat id "(default: " last-value "): ") nil history-sym))))
	(if (not (string= input "")) input last-value)))

;;(ide-read-with-last-value "test")
;;(ide-read-with-last-value "testt" '("pomme" "patate" "celeri"))

(defun ide-add-to-history (history-sym value)
  (if (not (boundp history-sym))
	  (set history-sym '()))
  (add-to-history history-sym value))

;;(ide-add-to-history (make-symbol "undefined-history-symbol") 'hello)

(defun ide-message (&optional msg color)
  (interactive)
  (if (or (null msg)
		  (null color))
	  (error "fb-message has invalid/null arguments"))

  (message (propertize msg 'face `(:foreground ,color))))

;;(ide-message "hello" "green")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide visual studio functions

(defun ide-is-vs-solution (file)
  (string-suffix-p ".sln" file))

(defun is-line-vs-project? (line-str)
  (or (string-match ".vcproj" line-str)
	  (string-match ".vcxproj" line-str)))

(defun ide-get-line-vs-project (line-str)
  (let ((line-substrings (ide-get-substrings line-str))
		(project-file nil))
	(cl-loop until project-file
			 for line-substr in line-substrings
			 if (or (string-suffix-p ".vcproj" line-substr)
					(string-suffix-p ".vcxproj" line-substr))
			 do (setq project-file line-substr))
	project-file))

;;(ide-get-line-vs-project "dasfd \"hmmm.vsproj\"test \"hello.vcxproj\" \"test\"")

(defun ide-accumulate-vs-project-file (project-file-dir project-name project-full-path line-str)
  (if (or (string-match "ClInclude" line-str)
		  (string-match "ClCompile" line-str))
	  (let ((substrs (ide-get-substrings line-str)))
		(if (not substrs)
			(throw 'ide-error "invalid ClCompile / ClInclude line..."))
		(let* ((parsed-project-file (car substrs))
			   (project-file (if (file-name-absolute-p parsed-project-file)
								 parsed-project-file
							   (concat project-file-dir parsed-project-file)))
			   (absolute-project-file (expand-file-name project-file))
			   (project-file-name (file-name-nondirectory absolute-project-file)))
		  (ide-data-append-file-path absolute-project-file)
		  (ide-data-append-file-name project-file-name)
		  (ide-data-append-project-name project-name)
		  (ide-data-append-project-path project-full-path)))))

(defun ide-parse-vs-project (project-file)
  (if (not (file-exists-p project-file))
	  (throw 'ide-error "invalid project-file"))
  
  (let ((project-file-dir (file-name-directory project-file))
		(project-name (file-name-sans-extension (file-name-nondirectory project-file)))
		(project-full-path (expand-file-name project-file)))
   (ide-parse-file-by-line project-file
						   (lambda (line-str) (ide-accumulate-vs-project-file
											   project-file-dir project-name project-full-path line-str)))))

;;(ide-parse-vs-project "e:/UnrealEngine/Engine/Intermediate/ProjectFiles/UE4.vcxproj")
;;(ide-parse-vs-project "e:/UnrealEngine/Engine/Intermediate/ProjectFiles/BlankProgram.vcxproj")

(defun ide-parse-vs-solution (sln-file)
  (if (not (file-exists-p sln-file))
	  (throw 'ide-error "invalid vs solution file"))

  (let ((sln-path (file-name-directory sln-file)))
	(ide-parse-file-by-line sln-file
							(lambda (current-line)
							  (let ((relative-project (ide-get-line-vs-project current-line)))
								(if relative-project
									(ide-parse-vs-project (concat sln-path relative-project))))))))

;;(ide-parse-vs-solution "e:/UnrealEngine/UE4.sln")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide xcode functions

(defun ide-is-xcode-solution (file)
  nil)

(defun ide-parse-xcode-solution (ide-current-project)
  (throw 'ide-error "xcode solutions are not currently unsupported"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide text projects functions

(defun ide-try-to-parse-text-solution-internal (file-on-line line-num file-map)
  (let* ((file-symbol (make-symbol (file-name-nondirectory file-on-line)))
		   (file-map-cell (assoc file-symbol file-map)))
	  (if file-map-cell
		  (progn
			(setcdr file-map-cell (vconcat (vector line-num) (cdr file-map-cell)))
			file-map)
		(cons (cons file-symbol (vector line-num)) file-map))))

(defun ide-try-to-parse-text-solution-loop (is-valid? file-map file-paths line-num)
  (cl-loop until (not is-valid?)
		   until (>= (point) (point-max))
		   do (progn
				(beginning-of-line)
				(set-mark (point))
				(end-of-line)
				(let* ((file-on-line (buffer-substring (region-beginning) (region-end)))
					   (is-file-on-line-valid (file-exists-p file-on-line)))
				  (setq is-valid? (and is-valid? is-file-on-line-valid))
				  (if (not is-valid?)
					  (debug))
				  (forward-line)
				  (if is-valid?
					  (let ((new-file-map (ide-try-to-parse-text-solution-internal file-on-line line-num file-map)))
						(setq file-map new-file-map)
						(setq file-paths (cons file-on-line file-paths)) ;; needs to be reversed!
						(setq line-num (+ line-num 1)))))))
  (setq file-paths (nreverse file-paths))
  (vector is-valid? file-map file-paths))

(defun ide-try-to-parse-text-solution (sln-file)
  (let ((sln-path (file-name-directory sln-file)))
   (ide-parse-file-by-line
	sln-file
	(lambda (line-str)
	  (let* ((file (if (file-name-absolute-p line-str)
					   line-str
					 (concat sln-path line-str)))
			 (file-full-path (expand-file-name file))
			 (file-name (file-name-nondirectory file)))
		(if (not (file-exists-p file-full-path))
			(throw 'ide-error (concat "invalid text solution, unexistant file: " file-full-path)))
		(ide-data-append-file-path file-full-path)
		(ide-data-append-file-name file-name)))))
  t)

;;(ide-try-to-parse-text-solution "c:/Users/david/AppData/Roaming/UnrealEngine/test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide find file

(defun ide-find-file (&optional file-name-arg)
  "Will find a file in the solution. With no arguments, the minibuffer will provide completion of all the files in the solution. If file-name-arg is non-nil, it will be used to try to find that file in the solution."
  (interactive)
  (if (not ide-current-project)
	  (throw 'ide-error "not project set... use 'ide-change-solution' to set it first"))
  
  (let* ((file-name (if (null file-name-arg) "" file-name-arg))
		 ;; (files (with-current-buffer (ide-solution-files-buffer)
		 ;; 		  (split-string (buffer-string) "\n" t)))
		 (files (ide-data-file-paths))
		 (options (ide-data-file-names))
		 (choice (ido-completing-read "ide file: " options nil t file-name 'ide-files-history))

		 ;; (choice-idx (cl-position choice options :test (lambda (x y) (string= x y))))
		 ;; (file (nth choice-idx files))

		 (choice-results (cl-loop for option being the element of options using (index i)
                                  for file being the element of files
                                  if (string= choice option) collect (list i option file)))
		 (file (if (= (length choice-results) 1)
				   (cl-caddr (car choice-results))
				 (ido-completing-read "result: " (mapcar 'cl-caddr choice-results) nil t "" nil))))
	(find-file file)
	(message (concat "opened file: " (file-relative-name file (file-name-directory ide-current-project))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide open file

(defun ide-get-next-file (file)
  (if (and (boundp 'ide-custom-get-next-file)
		   ide-custom-get-next-file)
	  (funcall ide-custom-get-next-file file)

	;; default implementation
	(let* ((file-no-dir (file-name-nondirectory file))
		   (file-ext (file-name-extension file-no-dir))
		   (next-ext (if (string-match-p "cpp" file-ext)
						 (replace-regexp-in-string "cpp" "h" file-ext)
					   (if (string-match-p "h" file-ext)
						   (replace-regexp-in-string "h" "cpp" file-ext)
						 file-ext))))
	  (let ((next-file (concat (file-name-directory file) (file-name-base file-no-dir) "." next-ext)))
		next-file))))

;; (setq ide-custom-get-next-file (lambda (file) "allo.cpp"))
;; (setq ide-custom-get-next-file nil)
;; (ide-get-next-file "toto.h")
;; (ide-get-next-file "c:/temp/toto.cpp")

(defun ide-open-file (original-file file &optional should-create? call-depth)
  "Create file, if not existing already."

  (let* ((call-depth (if call-depth call-depth 0))
		 (current-ext (file-name-extension file))
		 (should-stop-looking-for-files? (and (> call-depth 0)
											  (string= original-file file))))

	(if should-stop-looking-for-files?

		;; if can't find a file, try to look into opened buffers
		(let* ((next-buffer-name (file-name-nondirectory (ide-get-next-file original-file)))
			   (already-opened-buffer (get-buffer next-buffer-name)))

		  (if already-opened-buffer
			  (switch-to-buffer already-opened-buffer)

			;; if we can't find it in the opened buffer, try to find it!
			(ide-find-file next-buffer-name)))

	  ;; try to open the desired file
	  (cond ((file-exists-p file)
			 (find-file file))

			((and should-create?
				  (y-or-n-p (concat file " does not exists, created it?")))
			 (progn
			   (find-file file)
			   ;;(insert (ide-get-file-default-text file))
			   ))
			(t
			 (progn
			   (ide-open-file original-file (ide-get-next-file file) should-create? (+ call-depth 1))))))))

(defun ide-find-other-file ()
  "Goes in between .cpp <-> .h for files in the current project"
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
	(ide-open-file file (ide-get-next-file file))))

(defun ide-find-and-create-other-file ()
  "Goes in between .cpp <-> .h <-> .uc for files in an UE3 project and asks for creating the next file if non-existant"
  (interactive)
  (let ((file (buffer-file-name (current-buffer))))
	(ide-open-file file (ide-get-next-file file) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide-grep-sln

(defun ide-current-word-or-region ()
  (if (use-region-p)
	  (buffer-substring (region-beginning) (region-end))
	(current-word)))

(defun ide-grep (searched-str files)
  (message "generating files cache...")
  (let ((files-cache-buffer (get-buffer-create (concat "*temp-" (number-to-string (random)) "*"))))
	(unwind-protect
		(with-current-buffer files-cache-buffer
		  (let ((temp-file-name "c:/temp/temp-files-cache"))   ;; ewww, need to make this portable...
			(cl-loop for f in files do (insert (concat f " ")))
			(write-file temp-file-name nil)
			(grep-find (concat "cat " temp-file-name " | xargs grep --color=always -i -nH -e \"" searched-str "\" "))))
	  (kill-buffer files-cache-buffer)))
  (select-window (next-window))
  (delete-other-windows))

(defun ide-grep-project (project searched-str)
    "grep expression in specified project files"
	(interactive
	 (let ((interactive-project (ide-get-project-arg t))
		   (interactive-searched-str (let* ((word (ide-current-word-or-region))
											(input (read-string (concat "Search solution for (default: \"" word "\"): ")
																nil 'ide-grep-history)))
									   (if (string= input "") word input))))
	   (list interactive-project interactive-searched-str)))

	(let ((project-files (cl-loop for f in (ide-data-file-paths)
								  for p in (ide-data-projects)
								  if (string= p project) collect f)))
	  (ide-grep searched-str project-files)))

;; (ide-grep-project "TestProject" "actor")
;; (ide-grep-project "UE4" "DrawDebugSphere")

(defun ide-grep-solution (searched-str)
    "grep expression in entire solution."
	(interactive (list (let* ((word (ide-current-word-or-region))
							  (input (read-string (concat "Search solution for (default: \"" word "\"): ")
												  nil 'ide-grep-history)))
						 (if (string= input "") word input))))

	(ide-grep searched-str (ide-data-file-paths)))

;; (ide-grep-solution "sphere")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide compilation

(defun ide-post-compile (cmd)
  (let ((need-to-manipulate-windows? (> (length (window-list)) 1))
		(is-in-compilation-buffer? (string= (buffer-name (current-buffer)) "*compilation*")))
	(when (and need-to-manipulate-windows?
			   (not is-in-compilation-buffer?))
	  (select-window (next-window)))
	(goto-char (point-max))
	(delete-other-windows))

  (message cmd))

(defun ide-compile-vs-internal (target param-config param-platform build-refs?)
  (let* ((config	(if param-config
						param-config
					  (concat "/p:Configuration=\""(ide-read-with-last-value "config" ide-vs-configurations) "\"")))
		 (platform	(if param-platform
						param-platform
					  (concat "/p:Platform="(ide-read-with-last-value "platform" ide-vs-platforms))))
		 (cmd		(concat ide-msbuild-path " " ide-current-project " " target " " config " " platform " "
							(concat "/p:BuildProjectReferences=" (if build-refs? "true" "false")))))
	(ide-add-to-history 'ide-compile-vs-target-history target)
	(ide-add-to-history 'ide-compile-cmd-history cmd)
	(compile cmd)
	(ide-post-compile cmd)))

(defun ide-compile-vs-solution ()
  (if (not ide-current-project)
	  (throw 'ide-error "Cannot compile when no solution is active. Please call 'ide-change-solution'"))
  (ide-compile-vs-internal "/t:Build" nil nil t))

(defun ide-compile-vs-project (prefix project)
  (if (not prefix)
	  (setq prefix
			(let* ((last-prefix (if (and (boundp 'ide-compile-project-prefix-history)
										 (listp ide-compile-project-prefix-history))
									(car ide-compile-project-prefix-history)))
				   (prefix-input (read-string (concat "VS prefix for " project "(" last-prefix "): ")
											  nil 'ide-compile-project-prefix-history)))
			  (if (not (string= prefix-input "")) prefix-input last-prefix))))

  (let ((target (concat "/t:" prefix project)))
	(ide-compile-vs-internal target nil nil nil)))

(defun ide-compile-xcode-solution ()
  (throw 'ide-error "compilation for xcode is unsupported for now..."))

(defun ide-compile-xcode-project (project)
  (throw 'ide-error "xcode compilation not supported for now..."))

(defun ide-compile-solution ()
  "Compiles the current solution."
  (interactive)
  (cond
   ((ide-is-vs-solution ide-current-project)	(ide-compile-vs-solution))
   ((ide-is-xcode-solution ide-current-project)	(ide-compile-xcode-solution))
   (t											(throw 'ide-error "can't compile text solutions"))))

(defun ide-compile-project (project)
  "Compiles only the specified project in the current solution."
  (interactive (list (ide-get-project-arg nil)))
  (cond
   ((ide-is-vs-solution ide-current-project)	(ide-compile-vs-project nil project))
   ((ide-is-xcode-solution ide-current-project)	(ide-compile-xcode-project project))
   (t											(throw 'ide-error "can't compile text solutions"))))

(defun ide-quick-compile ()
  "Perform the last compilation action again."
  (interactive)
  (if (not (boundp 'ide-compile-cmd-history))
	  (message "Normal compilation must be ran at least once before 'ide-quick-compile' can be used. Please use 'ide-compile-project' or 'ide-compile-solution' first.")
	(let ((cmd (car (eval 'ide-compile-cmd-history))))
	  (compile cmd)
	  (ide-post-compile cmd))))

(defun ide-compilation-finish-handler (buffer string)
  "handles the *compilation buffer and prints a colored message after compilation"
  (if (and
       (buffer-live-p buffer)
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string))
    (progn
      (delete-other-windows)
	  (if (string= "*compilation*" (buffer-name (current-buffer)))
		  (switch-to-next-buffer))
	  (bury-buffer "*compilation*")
	  (ide-message "compilation successfull" "green"))
	(progn
	  (ide-message "compilation failed" "red"))))

(add-hook 'compilation-finish-functions 'ide-compilation-finish-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide mode definition

(defvar ide-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<C-M-backspace>") 'ide-grep-solution)
	(define-key map (kbd "<C-M-return>") 'ide-grep-project)

	(define-key map (kbd "C-M-'") 'ide-find-file)
	(define-key map (kbd "M-o") 'ide-find-other-file)
	(define-key map (kbd "C-M-o") 'ide-find-and-create-other-file)

	(define-key map (kbd "<f7>")	'ide-quick-compile)
	(define-key map (kbd "M-<f7>")	'ide-compile-solution)
	(define-key map (kbd "C-<f7>")	'ide-compile-project)

	map)
  "ide-mode keymap.")

(define-minor-mode ide-mode
  "ide mode. Provides a convenient way to search for files in large projects defined in different format (.ide or text). Also support compiling projects, to a certain extent."
  :init-value nil
  :global t
  :lighter " ide"
  :keymap 'ide-mode-map)

(if (boundp 'ide-default-current-project)
	(ide-change-solution ide-default-current-project))

(provide 'ide)
