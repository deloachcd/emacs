(setq denote-directory "~/Sync/Documents/org/denote")

;; let's try out denote
(use-package denote
  :general
  ('normal org-mode-map "SPC m d" 'denote-link))

;; I didn't really like this package, so I made my own solution below
;;(use-package consult-notes
;;  :config
;;  (setq consult-notes-sources (list (list "Denote" ?d denote-directory)))
;;  (when (locate-library "denote") (consult-notes-denote-mode)))

(defun denote-get-org-note-title (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title\\s-*:\\ *" nil t 1)
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun denote-get-file-tags (file)
  ;; use regex to record match-end, and skip forward to the tags
  (string-match "[0-9]+T[0-9]+--.*__" file)
  ;; get the first tag
  (let* ((tag-regexp "_[a-zA-Z0-9]+")
         (first-tag-start (string-match tag-regexp file (- (match-end 0) 1)))
         (tag-list '()))
    ;; add the first tag
    (add-to-list 'tag-list (substring file (+ first-tag-start 1) (match-end 0)))
    ;; loop through remaining tags
    (while (string-match tag-regexp file (match-end 0))
      (add-to-list 'tag-list (substring file (+ (match-beginning 0) 1)
                                        (match-end 0))))
    tag-list))

(defun denote-get-org-note-pair (file)
  "returns a pair to be parsed by completing-read, containing the title and tags"
  (let ((denote-title (denote-get-org-note-title file))
        (denote-tags (denote-get-file-tags file)))
    (cons (format "%s %s" denote-title
                  (mapcar (lambda (s) (concat "#" s)) denote-tags))
          file)))

(defun denote-search-org-notes ()
  (interactive)
  (let* ((files (directory-files denote-directory t "[0-9]+T[0-9]+--.*_.*\.org"))
         (title-pair-list (mapcar #'denote-get-org-note-pair files))
         (titles (mapcar #'car title-pair-list)))
    (find-file (cdr (assoc (completing-read "Find Note: " titles) title-pair-list)))))

(general-create-definer denote-bindings
  :prefix "SPC d"
  :states '(normal emacs)
  :keymaps 'override)

(denote-bindings
  "" '(nil :which-key "denote")
  "n" 'denote
  "d" 'denote-search-org-notes)
