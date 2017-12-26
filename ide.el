
(defcustom ide-default-current-project ""
  "The name of your default current project file"
  :type 'string
  :group 'ide)

;; temp, testing
(custom-set-variables '(ide-default-current-project "ide.ell"))

(defvar 'ide-current-project)

(defun ide-change-project (solution-file)
  (interactive (list (if (and (not (string= ide-default-current-project ""))
							  (file-exists-p ide-default-current-project))
						 ide-current-project
					   (ido-read-file-name "solution file: "))))

  (setq ide-current-project solution-file)
  (message (concat "Now using project file: " (file-name-nondirectory solution-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal ide functions

(defun ide-is-vs-solution (file)
  nil)

(defun ide-is-xcode-solution (file)
  nil)

(defun ide-is-text-solution (file)
  (let ((valid? t)
		(buffer (get-buffer-create (concat "*temp" (number-to-string (random)) "*"))))
       (with-current-buffer buffer
         (unwind-protect
             (progn (insert-file-contents file)
					(beginning-of-buffer)
					(while (and valid? (< (point) (point-max)))
					  (beginning-of-line)
					  (set-mark (point))
					  (end-of-line)
					  (let ((file-on-line (buffer-substring (region-beginning) (region-end))))
						(setq valid? (and valid? (file-exists-p file-on-line)))
						;(if (not valid?) (error (concat "invalid file: " file-on-line)))
						)
					  (forward-line)))
		   (kill-buffer buffer)))
	   valid?))

(defun ide-parse-text-solution (file)
  )

(defun ide-parse-current-solution ()
  (if (not (boundp 'ide-current-project))
	  (error "need to setup the current project first..."))
  (cond
   ((ide-is-vs-solution ide-current-project)	(ide-parse-vs-solution ide-current-project))
   ((ide-is-xcode-solution ide-current-project) (ide-parse-xcode-solution ide-current-project))
   ((ide-is-text-solution ide-current-project)	(ide-parse-text-solution ide-current-project))
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
