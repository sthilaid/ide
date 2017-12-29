
(defcustom ide-default-current-project ""
  "The name of your default current project file"
  :type 'string
  :group 'ide)

;; temp, testing
(custom-set-variables '(ide-default-current-project "ide.ell"))

(defvar 'ide-current-project)
(setq ide-data (cons '() '()))

(defun ide-change-project (solution-file)
  (interactive (list (if (and (not (string= ide-default-current-project ""))
							  (file-exists-p ide-default-current-project))
						 ide-current-project
					   (ido-read-file-name "solution file: "))))

  (setq ide-current-project solution-file)
  (ide-parse-current-solution)
  (message (concat "Now using project file: " (file-name-nondirectory solution-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide functions

(defun ide-is-vs-solution (file)
  nil)

(defun ide-is-xcode-solution (file)
  nil)

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
  (if (not is-valid?)
	  (vector nil '() '() 0)
	(if (>= (point) (point-max))
		(vector t file-map file-paths line-num)
	  (progn
		(beginning-of-line)
		(set-mark (point))
		(end-of-line)
		(let* ((file-on-line (buffer-substring (region-beginning) (region-end)))
			   (is-file-on-line-valid (file-exists-p file-on-line)))
		  (forward-line)
		  (if (not is-file-on-line-valid)
			  (vector nil '() '() 0)
			(let ((new-file-map (ide-try-to-parse-text-solution-internal file-on-line line-num file-map)))
			  (if (consp file-paths)
				  (setcdr file-paths (cons file-on-line '()))
				(setq file-paths (list file-on-line)))
			  (ide-try-to-parse-text-solution-loop is-valid? new-file-map file-paths (+ line-num 1)))))))))

(defun ide-try-to-parse-text-solution (file)
  (let ((buffer (get-buffer-create (concat "*temp" (number-to-string (random)) "*"))))
	(with-current-buffer buffer
	  (unwind-protect
		  (progn (insert-file-contents file)
				 (beginning-of-buffer)
				 (let* ((loop-result (ide-try-to-parse-text-solution-loop 't '() '() 0))
						(is-valid? (elt loop-result 0))
						(file-map (elt loop-result 1))
						(file-paths (elt loop-result 2)))
				   (if is-valid?
					   (setq ide-data (cons file-map file-paths))
					 (setq ide-data nil))))
		(kill-buffer buffer)))
	ide-data))

(ide-try-to-parse-text-solution "~/code/UnrealEngine/engine-files")

(defun ide-parse-current-solution ()
  (if (not (boundp 'ide-current-project))
	  (error "need to setup the current project first..."))
  (cond
   ((ide-is-vs-solution ide-current-project)				(ide-parse-vs-solution ide-current-project))
   ((ide-is-xcode-solution ide-current-project)				(ide-parse-xcode-solution ide-current-project))
   ((ide-try-to-parse-text-solution ide-current-project)	t)
   (t (error (concat "unsupported solution type for project file: " (file-name-nondirectory ide-current-project))))))


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
