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
;;  - ide-solution: show the current solution
;;  - ide-reset-data: resets all the ide-mode data and exits ide-mode
;;	- ide-find-file: find file in project
;;	- ide-find-other-file: find the next file related to the current file (.h/.cpp)
;;  - ide-grep-project: grep within all files referenced in the specified project
;;  - ide-grep-solution: grep within all files referenced in the entire solution
;;  - ide-compile-project: compile the specified solution project
;;  - ide-compile-solution: compile the entire solution
;;  - ide-quick-compile: re-run the last compilation command
;;	- ide-create-tags: Create tags file from all files in the current solution.
;;
;; See the keymap below for more interesting functions and their shortcuts.

;; Can be customized to set a default solution to load
(defcustom ide-default-current-solution ""
  "The name of your default current solution file"
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

(defcustom ide-tags-generator "etags"
  "Defines the list of platforms available in the visual studio solution."
  :type 'string
  :group 'ide)

(defcustom ide-additionnal-source-paths nil
  "List of additionnal directories to parse all and add all the files of extension `ide-extensions`"
  :type 'sexp
  :group 'ide)

(defcustom ide-extensions '("cpp" "h" "inl" "js" "html" "py")
  "List of additionnal directories to parse all and add all the files of extension `ide-extensions`"
  :type 'sexp
  :group 'ide)

(defcustom ide-compile-directory-solution-pre ""
  "Manual compilation options before the all files are added"
  :type 'string
  :group 'ide)

(defcustom ide-compile-directory-solution-post ""
  "Manual compilation options afterthe all files are added"
  :type 'string
  :group 'ide)

(defcustom ide-compile-directory-solution-lambda nil
  "if non nil, this function will be used to compile the files"
  :type 'function
  :group 'ide)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide config

(defvar ide-configs)
(setq ide-configs nil)

(defun ide-config-create (config-name default-current-solution &rest optional-args)
  "Create a new config and append it to the list of available configs. Optional args are 
        :extensions
        :additionnal-source-paths
        :vs-configurations
        :vs-platforms
        :directory-solution-pre 
        :directory-solution-post
        :directory-solution-lambda
   and must be passed last in key value format eg: :key value"
  
  (let ((extensions                 (plist-get optional-args :extensions))
        (additionnal-source-paths   (plist-get optional-args :additionnal-source-paths))
        (vs-configurations          (plist-get optional-args :vs-configurations))
        (vs-platforms               (plist-get optional-args :vs-platforms))
        (directory-solution-pre     (plist-get optional-args :directory-solution-pre))
        (directory-solution-post    (plist-get optional-args :directory-solution-post))
        (directory-solution-lambda  (plist-get optional-args :directory-solution-lambda)))
    (setq ide-configs (cons (cons config-name (vector config-name default-current-solution extensions vs-configurations
                                                      vs-platforms additionnal-source-paths directory-solution-pre directory-solution-post
                                                      directory-solution-lambda))
                            ide-configs))))

(defun ide-config-name (config)
  (elt config 0))

(defun ide-config-default-current-solution (config)
  (elt config 1))

(defun ide-config-extensions (config)
  (elt config 2))

(defun ide-config-configurations (config)
  (elt config 3))

(defun ide-config-platforms (config)
  (elt config 4))

(defun ide-config-additionnal-source-paths (config)
  (elt config 5))

(defun ide-config-compile-directory-solution-pre (config)
  (elt config 6))

(defun ide-config-compile-directory-solution-post (config)
  (elt config 7))

(defun ide-config-compile-directory-solution-lambda (config)
  (elt config 8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide state manipulation

;; will hold the current solution file path (set by ide-change-solution)
(defvar ide-current-solution)
(setq ide-current-solution "")

(defvar ide-last-create-tags-process)
(setq ide-last-create-tags-process nil)

(defvar ide-last-create-tags-process-stat-time)
(setq ide-last-create-tags-process-stat-time (current-time))

;; will hold the internal data used by ide-mode
(defvar ide-data)

(defun ide-solution ()
  "Shows what is the current solution file."
  (interactive)
  (message
   (if (file-exists-p ide-current-solution)
	   ide-current-solution
	 "No solutions are active, set one with 'ide-change-solution'")))

(defun ide-current-solution-valid? ()
  (and (boundp 'ide-current-solution)
	   (let ((current-solution (eval 'ide-current-solution)))
		 (and (stringp current-solution)
			  (not (string= current-solution ""))
			  (file-exists-p current-solution)))))

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
  (setq ide-current-solution "")
  (setq ide-data (vector '() '() '() '()))
  (if (and (boundp 'ide-mode)
		   (eval 'ide-mode))
	  (ide-mode -1))
  (message "ide-data was reset..."))

(define-error 'ide-error "unhandled IDE error occured...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide use config

(defun ide-use-config (config-name)
  "Use one of the predefined configs and apply it."
  (interactive (list (completing-read "config name: " (mapcar 'car ide-configs) nil t "" 'ide-project-history)))
  (let* ((config-pair (assoc (intern config-name) ide-configs))
         (config (if config-pair (cdr config-pair) nil)))
    (if (not config)
        (signal 'ide-error (concat "invalid config name: " config-name))
      (progn
        (setq ide-default-current-solution (ide-config-default-current-solution config))
        (setq ide-extensions (ide-config-extensions config))
        (setq ide-vs-configurations (ide-config-configurations config))
        (setq ide-vs-platforms (ide-config-platforms config))
        (setq ide-additionnal-source-paths (ide-config-additionnal-source-paths config))
        (setq ide-compile-directory-solution-pre (ide-config-compile-directory-solution-pre config))
        (setq ide-compile-directory-solution-post (ide-config-compile-directory-solution-post config))
        (setq ide-compile-directory-solution-lambda (ide-config-compile-directory-solution-lambda config))
        (ide-change-solution ide-default-current-solution)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide change solution

(defun ide-change-solution (solution-file)
  "Will set the solution file as the current solution for this mode."
  (interactive (list	(if (ide-current-solution-valid?)
  							(let* ((current-solution	(eval 'ide-current-solution))
  								   (default-sln-name	(file-name-nondirectory current-solution))
  								   (default-sln-path)	(file-name-directory current-solution))
  							  (ido-read-file-name "solution file: " default-sln-path default-sln-name t default-sln-name))
  						  (let* ((buffer-file (buffer-file-name (current-buffer)))
								 (default-sln-path (if buffer-file
													   (file-name-directory buffer-file)
													 "")))
							(ido-read-file-name	"solution file: " default-sln-path "" t "")))))
  (condition-case ex
	  (progn
		(if (not (file-exists-p solution-file))
			(signal 'ide-error "invalid solution file, does not exists..."))

		(let ((absolute-solution-file (expand-file-name (ide-get-proper-solution-file solution-file))))
		  (ide-reset-data)
		  (setq ide-current-solution absolute-solution-file)
		  (ide-parse-current-solution)
          (ide-parse-additional-folders)
		  (let* ((file-count (ide-data-size))
				 (msg (concat "Now using solution file: " (file-name-nondirectory absolute-solution-file)
							  " [parsed " (number-to-string file-count) " files]")))
			(message msg)
			(ide-mode t)
			msg)))
	(ide-error
	 (ide-reset-data)
	 (ide-mode -1)
	 (ide-message (concat "ide-error: " (cdr ex)) "red"))))

;;(ide-change-solution "e:/UnrealEngine/toto")
;;(ide-change-solution "e:/UnrealEngine/UE4.sln")
;;(ide-change-solution "~/code/UnrealEngine/UE4.xcworkspace/contents.xcworkspacedata")

(defun ide-parse-current-solution ()
  "Will try to parse the current solution file, and extract all the referenced files in it."
  (if (not (ide-current-solution-valid?))
	  (signal 'ide-error "need to setup the current project first..."))

  (message "Parsing solution file...")
  (cond
   ((ide-is-vs-solution ide-current-solution)				(ide-parse-vs-solution ide-current-solution))
   ((ide-is-xcode-solution ide-current-solution)			(ide-parse-xcode-solution ide-current-solution))
   ((ide-is-directory-solution ide-current-solution)		(ide-parse-directory-solution ide-current-solution))
   ((ide-try-to-parse-text-solution ide-current-solution)	t)
   (t (signal 'ide-error (concat "unsupported solution type for project file: "
								 (file-name-nondirectory ide-current-solution))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide utils

(defun ide-get-proper-solution-file (file)
  "Returns the path of the solution to be used for the given solution file."
  (cond
   ((ide-is-xcode-solution file)	(directory-file-name (file-name-directory file))) ;; content file contain within the main xcworkspacedata folder
   (t file)))

;; (ide-get-proper-solution-file "~/code/UnrealEngine/UE4.xcworkspace/contents.xcworkspacedata")

(defun ide-get-substrings (str)
  "Helper that will return a list of all the internal strings of the provided string. 
Eg: '(allo \"yes\" bye \"no\") would return '(\"yes\" \"no\")"
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
  "Helper that will apply line-parser-function to each line of the provided file."
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
									  (funcall line-parser-function line-num line-str))
									(forward-line)
									line-num)))
		(kill-buffer buffer)))))

(defun ide-get-project-arg (use-buffer-context-as-default?)
  "Helper that will select one of the available projects in the current solution."
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
  "Helper that will read an input using the last value in it's history as default. If options is non-nil, will use it as a choice."
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
  "prints a colored message in the mini-buffer"
  (if (or (null msg)
		  (null color))
	  (error "ide-message has invalid/null arguments"))

  (message (propertize msg 'face `(:foreground ,color))))

;;(ide-message "hello" "green")

(defun ide-get-line-project (line-str project-extentions)
  "Will return the project contained in line-str that has one of the provided extentions"
  (let ((line-substrings (ide-get-substrings line-str)))
	(cl-loop for line-substr in line-substrings
			 if (seq-some (lambda (ext) (string-suffix-p ext line-substr)) project-extentions)
			 return line-substr)))

;;(ide-get-line-project "as;dfkljasflkjsf asdf \"adsfdsf\" lkjlkjlkj \"toto.vcxproj\" dsafsdf \"blub\"..." '("abc" "vcxproj"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; additionnal folder parsing

(defun ide-try-to-add-file (original-path file extension)
  "Tries to add the file into the solution data, if it has the right extension"
  (let ((path-name (file-name-nondirectory (directory-file-name original-path))))
    (cl-loop for ext in extension
             if (string-suffix-p ext file)
             do (progn (ide-data-append-file-path (expand-file-name file))
                       ;;(message (concat "adding file " file))
                       (ide-data-append-file-name (file-name-nondirectory file))
                       (ide-data-append-project-name path-name)
                       (ide-data-append-project-path original-path)))))

(defun ide-parse-folder (original-folder current-folder extensions)
  "Will try to gather all files of `extension` in `folder`"
  (cl-loop for file in (directory-files current-folder)
           if (and (not (string= file "."))
                   (not (string= file "..")))
           do (let ((file-path (concat (file-name-as-directory current-folder) file)))
                ;;(message (concat "checking file: " file-path))
                (cond ((file-directory-p file-path)   (ide-parse-folder original-folder file-path extensions))
                      ((file-regular-p file-path)     (ide-try-to-add-file original-folder file-path extensions))))))

(defun ide-parse-additional-folders ()
  "Will try to gather all files of extension in `ide-extensions` from the list of directories `ide-additionnal-source-paths`."
  (cl-loop for folder in ide-additionnal-source-paths
           if (and (file-exists-p folder)
                   (file-directory-p folder))
           do (progn (message (concat "parsing additionnal folder: " folder))
                     (ide-parse-folder folder folder ide-extensions))))

;; (let ((folder "w:/Main/external/technology-group/gear/")
;;       (ide-extensions '("cpp" "h" "inl")))
;;   (ide-parse-folder folder folder ide-extensions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide visual studio functions

(defun ide-is-vs-solution (file)
  (string-suffix-p ".sln" file))

(defun is-line-vs-project? (line-str)
  (or (string-match ".vcproj" line-str)
	  (string-match ".vcxproj" line-str)))

(defun ide-accumulate-vs-project-file (project-file-dir project-name project-full-path line-num line-str)
  (if (or (string-match "ClInclude" line-str)
		  (string-match "ClCompile" line-str)
		  (string-match "Compile" line-str)
		  (string-match "CustomBuild" line-str))
	  (let ((substrs (ide-get-substrings line-str)))
		(if substrs
			(let* ((parsed-project-file (car substrs))
				   (project-file (if (file-name-absolute-p parsed-project-file)
									 parsed-project-file
								   (concat project-file-dir parsed-project-file)))
				   (absolute-project-file (expand-file-name project-file))
				   (project-file-name (file-name-nondirectory absolute-project-file)))
			  (ide-data-append-file-path absolute-project-file)
			  (ide-data-append-file-name project-file-name)
			  (ide-data-append-project-name project-name)
			  (ide-data-append-project-path project-full-path))))))

(defun ide-parse-vs-project (project-file)
  "Will parse provided visual studio .sln file and accumulate all the source file referenced by that solution."
  (if (not (file-exists-p project-file))
	  (signal 'ide-error (concat "invalid project-file: " project-file)))
  
  (let ((project-file-dir (file-name-directory project-file))
		(project-name (file-name-sans-extension (file-name-nondirectory project-file)))
		(project-full-path (expand-file-name project-file)))
    (message (concat "Accumulating files for project " project-name))
	(ide-parse-file-by-line project-file
							(lambda (line-num line-str) (ide-accumulate-vs-project-file
														 project-file-dir project-name project-full-path line-num line-str)))))

;;(ide-parse-vs-project "e:/UnrealEngine/Engine/Intermediate/ProjectFiles/UE4.vcxproj")
;;(ide-parse-vs-project "e:/UnrealEngine/Engine/Intermediate/ProjectFiles/BlankProgram.vcxproj")

(defun ide-parse-vs-solution (sln-file)
  (if (not (file-exists-p sln-file))
	  (signal 'ide-error "invalid vs solution file"))

  (let ((sln-path (file-name-directory sln-file)))
	(ide-parse-file-by-line sln-file
							(lambda (line-num current-line)
							  (let ((relative-project (ide-get-line-project current-line '(".vcxproj" ".vcproj" ".csproj"))))
								(if relative-project
									(ide-parse-vs-project (concat sln-path relative-project))))))))

;;(ide-parse-vs-solution "e:/UnrealEngine/UE4.sln")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide xcode functions

(defun ide-is-xcode-solution (file)
  (or (and (string-suffix-p ".xcworkspacedata" file)
		   (file-exists-p file))
	  (and (string-suffix-p ".xcworkspace" file)
		   (file-directory-p file))))

;; (ide-is-xcode-solution "~/code/UnrealEngine/UE4.xcworkspace")
;; (ide-is-xcode-solution "~/code/UnrealEngine/UE4.xcworkspace/contents.xcworkspacedata")

;; example line from xcode project file
;;60B3E4D93D861A6F537745E8 /* Pawn.h */ = {isa = PBXFileReference; explicitFileType = sourcecode.c.h; name = "Pawn.h"; path = "../../Source/Runtime/Engine/Classes/GameFramework/Pawn.h"; sourceTree = SOURCE_ROOT; };

(defun ide-accumulate-xcode-project-file (project-file-dir project-name project-full-path line-num line-str)
  "Will accumulate all the source files in the project file."
  (if (and (string-match "isa = PBXFileReference" line-str)
		   (or (string-match "explicitFileType = sourcecode" line-str)
			   (string-match "explicitFileType = file.text" line-str))
		   (string-match "path = " line-str))
	  (let ((substrs (ide-get-substrings line-str)))
		(if (not substrs)
			(signal 'ide-error (concat "unexpected syntax in " project-full-path " on line(" (number-to-string line-num) "\): \"" line-str "\"")))
		(let* ((parsed-project-file (let ((file-str (cl-loop for str in substrs if (file-exists-p (concat project-file-dir str)) return str)))
									  (if (not file-str)
										  (signal 'ide-error (concat "could not parse valid file path on line: " line-str))
										file-str)))
			   (project-file (if (file-name-absolute-p parsed-project-file)
								 parsed-project-file
							   (concat project-file-dir parsed-project-file)))
			   (absolute-project-file (expand-file-name project-file))
			   (project-file-name (file-name-nondirectory absolute-project-file)))
		  (ide-data-append-file-path absolute-project-file)
		  (ide-data-append-file-name project-file-name)
		  (ide-data-append-project-name project-name)
		  (ide-data-append-project-path project-full-path)))))

(defun ide-parse-xcode-project (project-file)
  "Will parse the provided project file and add all it's content in the ide mode data."
  (let ((project-file-dir (file-name-directory project-file))
		(project-name (file-name-sans-extension (file-name-nondirectory project-file)))
		(project-full-path (expand-file-name project-file))
		(project-content-file (concat project-file "/project.pbxproj")))
	(ide-parse-file-by-line project-content-file
							(lambda (line-num line-str) (ide-accumulate-xcode-project-file
														 project-file-dir project-name project-full-path line-num line-str)))))

(defun ide-remove-xcode-group (project-line)
  (if project-line
	  (replace-regexp-in-string "group:" "" project-line)))

;;(ide-remove-xcode-group "group:Engine/Intermediate/ProjectFiles/UE4.xcodeproj")

(defun ide-parse-xcode-solution (current-solution)
  "Tries to parse an xcode solution. Expects the .xcworkspace directory."
  (let ((sln-path (file-name-directory current-solution))
		(sln-contents-file (concat current-solution "/contents.xcworkspacedata")))
	(ide-parse-file-by-line sln-contents-file
							(lambda (line-num current-line)
							  (let ((relative-project (ide-remove-xcode-group (ide-get-line-project current-line '(".xcodeproj")))))
								(if relative-project
									(ide-parse-xcode-project (concat sln-path relative-project))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directory solution

(defun ide-is-directory-solution (absolute-solution-file)
  (file-directory-p absolute-solution-file))

(defun ide-parse-directory-solution (solution-dir)
  "Will use all the files in the provided directory as files in the solution"
  (if (not (file-directory-p solution-dir))
	  nil
	(let ((solution-name (file-name-nondirectory solution-dir)))
	  (message (concat "Accumulating files for directory " solution-name))
	  (ide-data-append-project-name solution-name)
	  (ide-data-append-project-path solution-name)

	  (ide-parse-folder solution-dir solution-dir ide-extensions))))

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
  "Tries to parse a text solution file. Should be a file with one file per line, as if gotten from 'grep . -name \"*.cpp\" | file."
  (if (file-exists-p sln-file)
	  (let ((sln-path (file-name-directory sln-file)))
		(ide-parse-file-by-line sln-file
								(lambda (line-num line-str)
								  (let* ((file (if (file-name-absolute-p line-str)
												   line-str
												 (concat sln-path line-str)))
										 (file-full-path (expand-file-name file))
										 (file-name (file-name-nondirectory file)))
									(if (not (file-exists-p file-full-path))
										(signal 'ide-error (concat "invalid text solution on line " (number-to-string line-num) ", unexistant file: \"" file-full-path "\"")))
									(ide-data-append-file-path file-full-path)
									(ide-data-append-file-name file-name))))
		t)
	nil))

;;(ide-try-to-parse-text-solution "c:/Users/david/AppData/Roaming/UnrealEngine/test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide find file

(defun ide-find-file (&optional file-name-arg)
  "Will find a file in the solution. With no arguments, the minibuffer will provide completion of all the files in the solution. If file-name-arg is non-nil, it will be used to try to find that file in the solution."
  (interactive)
  (if (not (ide-current-solution-valid?))
	  (signal 'ide-error "not project set... use 'ide-change-solution' to set it first"))
  
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
	(message (concat "opened file: " (file-relative-name file (file-name-directory ide-current-solution))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide include file (c/cpp)

(defun ide-include-file ()
  "Will add a #include statement with chosen header file within the solution."
  (interactive)
  (if (not (ide-current-solution-valid?))
	  (signal 'ide-error "not project set... use 'ide-change-solution' to set it first"))
  
  (let* ((file-name "")
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
				 (ido-completing-read "result: " (mapcar 'cl-caddr choice-results) nil t "" nil)))
         (relative-file (file-relative-name file (file-name-directory ide-current-solution))))
    (progn ;;save-excursion
      (goto-char 0)
      (search-forward "#include" nil t 3)
      (beginning-of-line)
      (indent-for-tab-command)
      (let ((include-txt (concat "#include \"" relative-file "\"\n")))
        (insert include-txt)
        (forward-line -1)
        (message (concat "added include: '" include-txt "'"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide open file

(defun ide-get-next-file (file)
  "Returns the next file name, coming after file. Used to cycle through similar files (Eg: .h/.cpp)"
  (if (and (boundp 'ide-custom-get-next-file)
		   ide-custom-get-next-file)
	  (funcall ide-custom-get-next-file file)

	;; default implementation
	(let* ((file-no-dir (file-name-nondirectory file))
		   (file-ext (file-name-extension file-no-dir))
		   (next-ext (if (string-match-p "cpp" file-ext)
						 (replace-regexp-in-string "cpp" "h" file-ext)
					   (if (string-match-p "h" file-ext)
						   (replace-regexp-in-string "h" "inl" file-ext)
						 (if (string-match-p "inl" file-ext)
						   (replace-regexp-in-string "inl" "cpp" file-ext)
						 file-ext)))))
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
;; ide-grep

(defun ide-current-word-or-region ()
  "returns string aroudn the point, if no region is being defined, else will return the region content."
  (if (use-region-p)
	  (buffer-substring (region-beginning) (region-end))
	(current-word)))

(defun ide-grep (searched-str files)
  "Generic function that greps searched-str in the provided files"
  
  (message "generating files cache...")
  (let ((files-cache-buffer (get-buffer-create (concat "*temp-" (number-to-string (random)) "*"))))
	(unwind-protect
		(with-current-buffer files-cache-buffer
		  (let ((temp-file-name (make-temp-file "ide-mode-temp-file-cache")))
			(cl-loop for f in files do (insert (concat f " ")))
			(write-file temp-file-name nil)
			(grep-find (concat "cat " temp-file-name " | xargs -n 50 grep --color=always -i -nH -e \"" searched-str "\" "))))
	  (kill-buffer files-cache-buffer)))
  (select-window (next-window))
  (delete-other-windows))

(defun ide-grep-project (project searched-str)
  "grep expression in specified project files"
  (interactive
   (let ((interactive-searched-str (let* ((word (ide-current-word-or-region))
										  (input (read-string (concat "Search solution for (default: \"" word "\"): ")
															  nil 'ide-grep-history)))
									 (if (string= input "") word input)))
         (interactive-project (ide-get-project-arg t)))
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
  "Cleanup function called after a visual studio compilation"
  (let ((need-to-manipulate-windows? (> (length (window-list)) 1))
		(is-in-compilation-buffer? (string= (buffer-name (current-buffer)) "*compilation*")))
	(when (and need-to-manipulate-windows?
			   (not is-in-compilation-buffer?))
	  (select-window (next-window)))
	(goto-char (point-max))
	(delete-other-windows))

  (message cmd))

(defun ide-compile-vs-internal (target param-config param-platform build-refs?)
  "Internal function used to compile visual studio solution."
  (let* ((config	(if param-config
						param-config
					  (concat "/p:Configuration=\""(ide-read-with-last-value "config" ide-vs-configurations) "\"")))
		 (platform	(if param-platform
						param-platform
					  (concat "/p:Platform=\""(ide-read-with-last-value "platform" ide-vs-platforms) "\"")))
		 (cmd		(concat ide-msbuild-path " \"" ide-current-solution "\" " target " " config " " platform " "
							(concat "/p:BuildProjectReferences=" (if build-refs? "true" "false")))))
	(ide-add-to-history 'ide-compile-vs-target-history target)
	(ide-add-to-history 'ide-compile-cmd-history cmd)
    (compile cmd)
	(ide-post-compile cmd)))

(defun ide-compile-vs-solution ()
  (if (not (ide-current-solution-valid?))
	  (signal 'ide-error "Cannot compile when no solution is active. Please call 'ide-change-solution'"))
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

  (let ((target (concat "/t:\"" prefix project "\"")))
	(ide-compile-vs-internal target nil nil nil)))

(defun ide-compile-xcode-solution ()
  (signal 'ide-error "compilation for xcode is unsupported for now..."))

(defun ide-compile-xcode-project (project)
  (signal 'ide-error "xcode compilation not supported for now..."))

(defun ide-compile-directory-solution ()
  (let* ((files (ide-data-file-paths)))
    (if ide-compile-directory-solution-lambda
        (progn (funcall ide-compile-directory-solution-lambda files)
               (ide-post-compile "done"))
      (let ((cmd (concat (cl-reduce (lambda (acc x) (concat acc " \"" x "\"")) files :initial-value ide-compile-directory-solution-pre)
                         ide-compile-directory-solution-post)))
        (ide-add-to-history 'ide-compile-cmd-history cmd)
        (compile cmd)
        (ide-post-compile cmd)))))

(defun ide-compile-solution ()
  "Compiles the current solution."
  (interactive)
  (cond
   ((ide-is-vs-solution ide-current-solution)	(ide-compile-vs-solution))
   ((ide-is-xcode-solution ide-current-solution)	(ide-compile-xcode-solution))
   ((ide-is-directory-solution ide-current-solution)    (ide-compile-directory-solution))
   (t											(signal 'ide-error "can't compile text solutions"))))

(defun ide-compile-project (project)
  "Compiles only the specified project in the current solution."
  (interactive (list (ide-get-project-arg nil)))
  (cond
   ((ide-is-vs-solution ide-current-solution)           (ide-compile-vs-project nil project))
   ((ide-is-xcode-solution ide-current-solution)        (ide-compile-xcode-project project))
   (t											(signal 'ide-error "can't compile text solutions"))))

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
;; ide mode tags generation

(defun ide-get-tags-file ()
  "Returns the tags file for this solution."
  (if (not (ide-current-solution-valid?))
	  (signal 'ide-error "not project set... use 'ide-change-solution' to set it first"))
  
  (concat (file-name-directory ide-current-solution) "TAGS"))

(defun ide-create-tags ()
  "Create tags file from all files in the current solution."
  (interactive)
  (if (not (ide-current-solution-valid?))
	  (signal 'ide-error "not project set... use 'ide-change-solution' to set it first"))

  (let ((should-skip? (and ide-last-create-tags-process
						   (process-live-p ide-last-create-tags-process)
						   (not (y-or-n-p "ide-create-tags process already running, restart it?")))))
	(if should-skip?
		(message "Skipped restarting TAGS generation.")
	  (let* ((tags-file (ide-get-tags-file))
			 (temp-file-name (make-temp-file "ide-mode-temp-file-cache"))
			 (files-cache-buffer (get-buffer-create temp-file-name)))
		(if (file-exists-p tags-file)
			(delete-file tags-file))
		(unwind-protect
			(with-current-buffer files-cache-buffer
			  (let* ((files (ide-data-file-paths)))
				(cl-loop for f in files do (insert (concat f " ")))
				(write-file temp-file-name nil)
				(message (concat "generating " tags-file "..."))
				(setq ide-last-create-tags-process-stat-time (current-time))
				(setq ide-last-create-tags-process
					  (start-process-shell-command "ide-create-tags"
												   (make-temp-name "ide-create-tags")
												   (concat "cat " temp-file-name " | xargs -n 50 " ide-tags-generator " -f " tags-file " -a ")))
				(set-process-sentinel ide-last-create-tags-process 'ide-create-tags-process-sentinel)))
		  (kill-buffer files-cache-buffer))))))

(defun ide-create-tags-process-sentinel (process event)
  "Sentinel called when a ide-create-tags process changes status."
  (let* ((duration		(time-subtract (current-time) ide-last-create-tags-process-stat-time))
		 (duration-str	(format-time-string "%M minutes %S secs" duration)))
	(cond ((string= event "finished\n") (progn (visit-tags-table (ide-get-tags-file))
											   (ide-message (concat "'ide-create-tags' completed in " duration-str ".") "green")))
		  (t (ide-message (concat "'ide-create-tags' failed after " duration-str " with event: " event) "red")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide mode definition

(defvar ide-mode-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "<C-M-backspace>") 'ide-grep-solution)
	(define-key map (kbd "<C-M-return>") 'ide-grep-project)

	(define-key map (kbd "C-M-'") 'ide-find-file)
	(define-key map (kbd "M-o") 'ide-find-other-file)
	(define-key map (kbd "C-M-o") 'ide-find-and-create-other-file)

	(define-key map (kbd "C-M-i")   'ide-include-file)
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

(if (and (boundp 'ide-default-current-solution)
		 (stringp ide-default-current-solution)
		 (not (string= ide-default-current-solution "")))
	(ide-change-solution ide-default-current-solution))

(require 'ide-browser)
(provide 'ide)
