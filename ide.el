(defcustom ide-default-current-project ""
  "The name of your default current project file"
  :type 'string
  :group 'ide)

;; temp, testing
(custom-set-variables '(ide-default-current-project "ide.ell"))

(defvar ide-current-project)
(setq ide-current-project "")

(defvar ide-data)
(setq ide-data (cons '() '()))

(defun ide-change-project (solution-file)
  (interactive (list (if (and (not (string= ide-default-current-project ""))
							  (file-exists-p ide-default-current-project))
						 ide-current-project
					   (ido-read-file-name "solution file: "))))

  (setq ide-current-project solution-file)
  (ide-parse-current-solution)
  (message (concat "Now using project file: " (file-name-nondirectory solution-file))))

(ide-change-project "c:/Users/david/AppData/Roaming/UnrealEngine/UE4.sln")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide functions

(defun ide-is-vs-solution (file)
  (string-suffix-p ".sln" file))

(defun is-line-vs-project? (line-str)
  (or (string-match ".vcproj" line-str)
	  (string-match ".vcxproj" line-str)))

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

;;(setq testline "Project(\"{8BC9CEB8-8B4A-11D0-8D11-00A0C91BC942}\") = \"UE4\", \"Engine\\Intermediate\\ProjectFiles\\UE4.vcxproj\", \"{C9E3F46F-50B8-4EEF-B6BE-2EA8E7F03F3E}\"")

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

(defun ide-accumulate-vs-project-file (project-file-path line-str)
  (if (or (string-match "ClInclude" line-str)
		  (string-match "ClCompile" line-str))
	  (let ((substrs (ide-get-substrings line-str)))
		(if (not substrs)
			(error "invalid ClCompile / ClInclude line..."))
		(let* ((project-file (car substrs))
			   (absolute-project-file (if (file-name-absolute-p project-file)
										  project-file
										(concat project-file-path project-file))))
		 (setcar ide-data (cons absolute-project-file (car ide-data)))
		 (print absolute-project-file))
		)))

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

(defun ide-is-xcode-solution (file)
  nil)

(defun ide-parse-xcode-solution (ide-current-project)
  (error "xcode solutions are currently unsupported"))

(defun ide-data-file-map (data)
  (car data))

(defun ide-data-file-paths (data)
  (cdr data))

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

(defun ide-try-to-parse-text-solution (file)
  (let ((buffer (get-buffer-create (concat "*temp" (number-to-string (random)) "*"))))
	(with-current-buffer buffer
	  (unwind-protect
		  (progn (insert-file-contents file)
				 (goto-char (point-min))
				 (let* ((loop-result (ide-try-to-parse-text-solution-loop 't '() '() 0))
						(is-valid? (elt loop-result 0))
						(file-map (elt loop-result 1))
						(file-paths (elt loop-result 2)))
				   (if is-valid?
					   (setq ide-data (cons file-map file-paths))
					 (setq ide-data nil))))
		(kill-buffer buffer)))
	ide-data))

;;(ide-try-to-parse-text-solution "~/code/UnrealEngine/engine-files")

(defun ide-parse-current-solution ()
  (if (not (boundp 'ide-current-project))
	  (error "need to setup the current project first..."))
  (cond
   ((ide-is-vs-solution ide-current-project)				(ide-parse-vs-solution ide-current-project))
   ((ide-is-xcode-solution ide-current-project)				(ide-parse-xcode-solution ide-current-project))
   ((ide-try-to-parse-text-solution ide-current-project)	t)
   (t (error (concat "unsupported solution type for project file: " (file-name-nondirectory ide-current-project))))))

(defun ide-find-file (&optional file-name-arg)
  (interactive)
  (let* ((file-name (if (null file-name-arg) "" file-name-arg))
		 ;; (files (with-current-buffer (ide-solution-files-buffer)
		 ;; 		  (split-string (buffer-string) "\n" t)))
		 (files (car ide-data))
		 (options (mapcar (lambda (f) (file-name-nondirectory f)) files))
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
	(message (concat "opened file: " file))))

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
