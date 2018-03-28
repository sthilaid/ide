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
;;	- ide-change-project: sets your current project
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide state manipulation

;; will hold the current project file path (set by ide-change-project)
(defvar ide-current-project)

;; will hold the internal data used by ide-mode
(defvar ide-data)

(defun ide-data-file-paths ()
  (elt ide-data 0))

(defun ide-data-file-names ()
  (elt ide-data 1))

(defun ide-data-projects ()
  (elt ide-data 2))

(defun ide-data-append-file-path (path)
  (aset ide-data 0 (cons path (ide-data-file-paths))))

(defun ide-data-append-file-name (file)
  (aset ide-data 1 (cons file (ide-data-file-names))))

(defun ide-data-append-project-name (project)
  (aset ide-data 2 (cons project (ide-data-projects))))

(defun ide-data-size ()
  (length (ide-data-file-names)))

(defun ide-reset-data ()
  (setq ide-current-project nil)
  (setq ide-data (vector '() '() '()))
  (message "ide-data was reset..."))

(ide-reset-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide change project

(defun ide-change-project (solution-file)
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
	 (ide-mode nil)
	 (message (cadr ex)))))

;;(ide-change-project "e:/UnrealEngine/UE4.sln")

(defun ide-parse-current-solution ()
  (if (or (not (boundp 'ide-current-project))
		  (not ide-current-project))
	  (throw 'ide-error "need to setup the current project first..."))

  (message "Parsing solution file...")
  (cond
   ((ide-is-vs-solution ide-current-project)				(ide-parse-vs-solution ide-current-project))
   ((ide-is-xcode-solution ide-current-project)				(ide-parse-xcode-solution ide-current-project))
   ((ide-try-to-parse-text-solution ide-current-project)	t)
   (t (throw 'ide-data (concat "unsupported solution type for project file: "
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

(defun ide-accumulate-vs-project-file (project-file-path project-name line-str)
  (if (or (string-match "ClInclude" line-str)
		  (string-match "ClCompile" line-str))
	  (let ((substrs (ide-get-substrings line-str)))
		(if (not substrs)
			(throw 'ide-data "invalid ClCompile / ClInclude line..."))
		(let* ((parsed-project-file (car substrs))
			   (project-file (if (file-name-absolute-p parsed-project-file)
								 parsed-project-file
							   (concat project-file-path parsed-project-file)))
			   (absolute-project-file (expand-file-name project-file))
			   (project-file-name (file-name-nondirectory absolute-project-file)))
		  (ide-data-append-file-path absolute-project-file)
		  (ide-data-append-file-name project-file-name)
		  (ide-data-append-project-name project-name)))))

(defun ide-parse-vs-project (project-file)
  (if (not (file-exists-p project-file))
	  (throw 'ide-data "invalid project-file"))
  (let ((project-file-path (file-name-directory project-file))
		(project-name (file-name-sans-extension (file-name-nondirectory project-file))))
   (ide-parse-file-by-line project-file
						   (lambda (line-str) (ide-accumulate-vs-project-file project-file-path project-name line-str)))))

;;(ide-parse-vs-project "e:/UnrealEngine/Engine/Intermediate/ProjectFiles/UE4.vcxproj")
;;(ide-parse-vs-project "e:/UnrealEngine/Engine/Intermediate/ProjectFiles/BlankProgram.vcxproj")

(defun ide-parse-vs-solution (sln-file)
  (if (not (file-exists-p sln-file))
	  (throw 'ide-data "invalid vs solution file"))

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
  (throw 'ide-data "xcode solutions are not currently unsupported"))

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
			(throw 'ide-data (concat "invalid text solution, unexistant file: " file-full-path)))
		(ide-data-append-file-path file-full-path)
		(ide-data-append-file-name file-name)))))
  t)

;;(ide-try-to-parse-text-solution "c:/Users/david/AppData/Roaming/UnrealEngine/test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide find file

(defun ide-find-file (&optional file-name-arg)
  (interactive)
  (if (not ide-current-project)
	  (throw 'ide-data "not project set... use ide-change-project to set it first"))
  
  (let* ((file-name (if (null file-name-arg) "" file-name-arg))
		 ;; (files (with-current-buffer (ide-solution-files-buffer)
		 ;; 		  (split-string (buffer-string) "\n" t)))
		 (files (ide-data-file-paths))
		 (options (ide-data-file-names))
		 (choice (ido-completing-read "ide file: " options nil t file-name 'ide-files-history))

		 ;; (choice-idx (cl-position choice options :test (lambda (x y) (string= x y))))
		 ;; (file (nth choice-idx files))

		 (choice-results (loop for option being the element of options using (index i)
							   for file being the element of files
							   if (string= choice option) collect (list i option file)))
		 (file (if (= (length choice-results) 1)
				   (caddr (car choice-results))
				 (ido-completing-read "result: " (mapcar 'caddr choice-results) nil t "" nil))))
	(find-file file)
	(message (concat "opened file: " (file-relative-name file (file-name-directory ide-current-project))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide open file

(defun ide-get-next-file (file)
  (if (boundp 'ide-custom-get-next-file)
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
;; (makunbound 'ide-custom-get-next-file)
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
    "rgrep expression in specified project files"
	(interactive
	 (let ((interactive-project
			(let* ((projects (seq-uniq (ide-data-projects)))
				  (project-options (cl-loop for p in projects
											for i from 1 to (+ (length projects) 1)
											collect (list p i)))
				  (default-project
					(let* ((name (buffer-name (current-buffer)))
						   (buffer-project (cl-loop for f in (ide-data-file-names)
													for i from 0 to (ide-data-size)
													if (string= f name) return (elt (ide-data-projects) i) end)))
					  (if (not buffer-project) (car projects) buffer-project))))
			 (let ((project-choice (completing-read (concat "project (default: " default-project "): ")
													projects
													nil t "" 'ide-project-history)))
			   (if (string= project-choice "") default-project project-choice))))
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
    "rgrep expression in entire solution."
	(interactive (list (let* ((word (ide-current-word-or-region))
							  (input (read-string (concat "Search solution for (default: \"" word "\"): ")
												  nil 'ide-grep-history)))
						 (if (string= input "") word input))))

	(ide-grep searched-str (ide-data-file-paths)))

;; (ide-grep-solution "sphere")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide mode definition

(defvar ide-mode-map
  (let ((map (make-sparse-keymap)))
	;; (define-key map (kbd "<M-return>") 'ide-compile)
	;; (define-key map (kbd "<S-M-return>") 'ide-compile-project)
	;; (define-key map (kbd "<C-return>") 'ide-gensln)

	(define-key map (kbd "<C-M-backspace>") 'ide-grep-solution)
	(define-key map (kbd "<C-M-return>") 'ide-grep-project)

	(define-key map (kbd "C-M-'") 'ide-find-file)
	(define-key map (kbd "M-o") 'ide-find-other-file)
	(define-key map (kbd "C-M-o") 'ide-find-and-create-other-file)

	;; (define-key map (kbd "<f5>") 'ide-run-default)
	;; (define-key map (kbd "S-<f5>") 'ide-kill)
	;; (define-key map (kbd "<f7>") 'ide-compile-project-default)
	;; (define-key map (kbd "<f9>") 'ide-kill-compile-run)

	map)
  "ide-mode keymap.")


(define-minor-mode ide-mode
  "ide mode. Provides a convenient way to search for files in large projects defined in different format (.ide or text)."
  :init-value nil
  :global t
  :lighter " ide"
  :keymap 'ide-mode-map)

(if (boundp 'ide-default-current-project)
	(ide-change-project ide-default-current-project))

(provide 'ide)
