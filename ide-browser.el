(defun ide-browser-change-sort-key ()
  (interactive)
  (let* ((columns   '("name" "path" "project"))
         (default   (if (and (boundp 'ide-browser-sort-key-history)
                             (listp ide-browser-sort-key-history))
                        (car ide-browser-sort-key-history)
                      (car columns)))
         (input     (completing-read (concat "sort key(" default "): ") columns nil t "" 'ide-browser-sort-key-history))
         (key       (if (string= input "") default input)))
    (setq tabulated-list-sort-key (cons key (y-or-n-p "invert?")))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun ide-browser-find-file ()
  (interactive)
  (let ((entry (tabulated-list-get-entry)))
    (if entry
        (find-file (expand-file-name (elt entry 1) ide-current-solution)))))

(define-derived-mode
  ide-browser-mode tabulated-list-mode "ide browser mode"
  "Major mode browsing file in current solution"
  (let ((file-names   (ide-data-file-names))
        (file-paths   (mapcar (lambda (x) (file-relative-name x ide-current-solution)) (ide-data-file-paths)))
        (projects     (ide-data-projects)))
    (setq tabulated-list-entries
          (cl-loop for name in file-names
                   for path in file-paths
                   for project in projects
                   collect (list name (vector name path project))))

    (setq tabulated-list-format (vector (list "name"    (cl-loop for x in file-names    maximize (length x)) t)
                                        (list "path"    (cl-loop for x in file-paths    maximize (length x)) t)
                                        (list "project" (cl-loop for x in projects      maximize (length x)) t))))
  (setq tabulated-list-sort-key (cons "name" nil))
  (tabulated-list-init-header)
  (use-local-map (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map tabulated-list-mode-map)
                   (define-key map (kbd "s") 'ide-browser-change-sort-key)
                   (define-key map (kbd "<return>") 'ide-browser-find-file)
                   map)))

(defun ide-browse ()
  "Browse the files of the current solution"
  (interactive)
  (if (= (ide-data-size) 0)
      (error "no solutions are loaded... (load with ide-use-config or ide-change-solution)")
    (with-current-buffer (get-buffer-create "*ide-browser*")
      (ide-browser-mode)
      (tabulated-list-print)
      (switch-to-buffer (current-buffer)))))

(provide 'ide-browser)
