(defcustom ide-default-current-project ""
  "The name of your default current project file"
  :type 'string
  :group 'ide)

(defvar ide-current-project)
(defvar ide-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide state manipulation

(defun ide-data-file-paths (data)
  (car ide-data))

(defun ide-data-file-names ()
  (cdr ide-data))

(defun ide-data-append-file-path (path)
  (setcar ide-data (cons path (car ide-data))))

(defun ide-data-append-file-name (file)
  (setcdr ide-data (cons file (cdr ide-data))))

(defun ide-data-size ()
  (length (car ide-data)))

(defun ide-reset-data ()
  (setq ide-current-project nil)
  (setq ide-data (cons '() '()))
  (message "ide-data was reset..."))

(ide-reset-data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide change project

(defun ide-change-project (solution-file)
  (interactive (list (if (and (not (string= ide-default-current-project ""))
							  (file-exists-p ide-default-current-project))
						 ide-current-project
					   (let ((default-path (if ide-current-project
											   (file-name-directory ide-current-project)
											 (file-name-directory (buffer-file-name (current-buffer)))))
							 (default-project (if ide-current-project
												  (file-name-nondirectory ide-current-project)
												"")))
						 (ido-read-file-name "solution file: " default-path default-project t default-project)))))
  (condition-case ex
	  (progn
		(if (not (file-exists-p solution-file))
			(error "invalid solution file, does not exists..."))
		
		(let ((absolute-solution-file (expand-file-name solution-file)))
		  (setq ide-current-project absolute-solution-file)
		  (ide-parse-current-solution)
		  (let ((file-count (ide-data-size)))
			(message (concat "Now using project file: " (file-name-nondirectory absolute-solution-file)
							 " [parsed " (number-to-string file-count) " files]")))))
   ('error (ide-reset-data)
		   (message (cadr ex)))))

;;(ide-change-project "c:/Users/david/AppData/Roaming/UnrealEngine/UE4.sln")

(defun ide-parse-current-solution ()
  (if (not (boundp 'ide-current-project))
	  (error "need to setup the current project first..."))

  (setq ide-data (cons '() '()))
  (cond
   ((ide-is-vs-solution ide-current-project)				(ide-parse-vs-solution ide-current-project))
   ((ide-is-xcode-solution ide-current-project)				(ide-parse-xcode-solution ide-current-project))
   ((ide-try-to-parse-text-solution ide-current-project)	t)
   (t (error (concat "unsupported solution type for project file: " (file-name-nondirectory ide-current-project))))))

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
		(kill-buffer buffer)
		))))

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

(defun ide-accumulate-vs-project-file (project-file-path line-str)
  (if (or (string-match "ClInclude" line-str)
		  (string-match "ClCompile" line-str))
	  (let ((substrs (ide-get-substrings line-str)))
		(if (not substrs)
			(error "invalid ClCompile / ClInclude line..."))
		(let* ((project-file (car substrs))
			   (absolute-project-file (expand-file-name (concat project-file-path project-file)))
			   (project-file-name (file-name-nondirectory absolute-project-file)))
		  (ide-data-append-file-path absolute-project-file)
		  (ide-data-append-file-name project-file-name)))))

(defun ide-parse-vs-project (project-file)
  (if (not (file-exists-p project-file))
	  (error "invalid project-file"))
  (let ((project-file-path (file-name-directory project-file)))
   (ide-parse-file-by-line project-file (lambda (line-str) (ide-accumulate-vs-project-file project-file-path line-str)))))

;;(ide-parse-vs-project "c:/Users/david/AppData/Roaming/UnrealEngine/Engine/Intermediate/ProjectFiles/UE4.vcxproj")
;;(ide-parse-vs-project "c:/Users/david/AppData/Roaming/UnrealEngine/Engine/Intermediate/ProjectFiles/BlankProgram.vcxproj")

(defun ide-parse-vs-solution (sln-file)
  (if (not (file-exists-p sln-file))
	  (error "invalid vs solution file"))

  (let ((sln-path (file-name-directory sln-file)))
	(ide-parse-file-by-line sln-file
							(lambda (current-line)
							  (let ((relative-project (ide-get-line-vs-project current-line)))
								(if relative-project
									(ide-parse-vs-project (concat sln-path relative-project))))))))

;;(ide-parse-vs-solution "c:/Users/david/AppData/Roaming/UnrealEngine/UE4.sln")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide xcode functions

(defun ide-is-xcode-solution (file)
  nil)

(defun ide-parse-xcode-solution (ide-current-project)
  (error "xcode solutions are not currently unsupported"))

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
			(error (concat "invalid text solution, unexistant file: " file-full-path)))
		(ide-data-append-file-path file-full-path)
		(ide-data-append-file-name file-name)))))
  t)

;;(ide-try-to-parse-text-solution "c:/Users/david/AppData/Roaming/UnrealEngine/test")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ide find file

(defun ide-find-file (&optional file-name-arg)
  (interactive)
  (if (not ide-current-project)
	  (error "not project set... use ide-change-project to set it first"))
  
  (let* ((file-name (if (null file-name-arg) "" file-name-arg))
		 ;; (files (with-current-buffer (ide-solution-files-buffer)
		 ;; 		  (split-string (buffer-string) "\n" t)))
		 (files (car ide-data))
		 (options (cdr ide-data))
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
;; ide mode definition

(defvar ide-mode-map
  (let ((map (make-sparse-keymap)))
	;;(define-key map (kbd "<f9>") 'ide-kill-compile-run)
	map)
  "ide-mode keymap.")

;; (add-hook 'ide-mode-hook (lambda ()
;; (if ide-mode
;; (define-key dired-mode-map (kbd "M-o") 'ide-dired-open-marked-files)
;; (define-key dired-mode-map (kbd "M-o") nil))))

(define-minor-mode ide-mode
  "ide mode. Provides a convenient way to search for files in large projects defined in different format (.ide or text)."
  :init-value nil
  :global nil
  :lighter " ide"
  :keymap 'ide-mode-map
  )

(provide 'ide)
