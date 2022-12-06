;; Configuration related to org-mode, org-roam, etc.
(require 'general-keybinds-layer)

;; Where we'll store all our org documents
(setq org-root "~/Sync/Documents/org")

;; Automagically mix variable and monospace fonts
(use-package mixed-pitch
  :defer
  :init (setq mixed-pitch-set-height t))

(defun org-mode-setup ()
  ;;(setq org-hide-emphasis-markers t)
  ;;(mixed-pitch-mode 1)
  (visual-line-mode 1)
  (org-indent-mode 1))

(defun org-evil-add-checkbox-item ()
  (interactive)
  (with-current-buffer (current-buffer)
    (end-of-line)
    (org-meta-return)
    (insert "[ ] ")
    (evil-insert-state)))

;; this should really be a built-in...
(defun org-table-recalculate-all ()
  (interactive)
  (org-table-recalculate 'iterate)) 

(use-package org
  :defer
  :hook (org-mode . org-mode-setup)
  :config
  (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-ellipsis " ▾")
  (set-face-underline 'org-ellipsis nil)
  ;; make sure no theme makes our titles overly large
  (set-face-attribute 'org-document-title nil :inherit 'variable-pitch
                      :weight 'bold :height 1.1)
  :general
  ('normal org-mode-map "SPC m s" 'org-insert-src-block)
  ('normal org-mode-map "SPC m e" 'org-babel-execute-src-block)
  ('normal org-mode-map "SPC m r" 'org-table-recalculate-all)
  ('normal org-mode-map "SPC m c" 'org-evil-add-checkbox-item))

;; presentations from emacs
(use-package org-tree-slide
  :config
  (setq org-image-actual-width nil))

;; old value: '("♣" "♠" "♦" "♥")
(use-package org-bullets
  :defer
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("✸" "✱" "❖" "♦")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "roam" org-root))
  (org-roam-dailies-directory "journal/")
  (org-roam-db-location
   (expand-file-name (concat "org-roam." (system-name) ".db")
                     org-roam-directory))
  :config
  (org-roam-db-autosync-enable)
  (setq org-M-RET-may-split-line nil)
  :general
  ('normal org-mode-map "SPC m i" 'org-roam-node-insert))

(use-package evil-org
  :defer
  ;;:after org
  :hook (org-mode . evil-org-mode)
  :config
  (progn
    (require 'evil-org-agenda)
    (evil-org-set-key-theme '(textobjects
                              insert
                              navigation
                              additional
                              shift
                              todo
                              heading))
    (evil-org-agenda-set-keys)))

(setq org-agenda-files
      (list (expand-file-name "agenda/tasks.org" org-root)
            (expand-file-name "agenda/dates.org" org-root)))

;; Hopefully, fix messed up indentation in source code blocks
(setq org-src-fontify-natively t
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t)

;; Source: http://wenshanren.org/?p=334
(defun org-insert-src-block (src-code-type)
  "Insert a `src-code-type' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "yaml")))
     (list (completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+begin_src %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+end_src\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun org-agenda-edit-tasks ()
  (interactive)
  (find-file (expand-file-name "agenda/tasks.org" org-root)))

(defun org-agenda-edit-dates ()
  (interactive)
  (find-file (expand-file-name "agenda/dates.org" org-root)))

;; -- new simple daily goal tracking system
(setq org-dailies-dir (concat org-root "/dailies"))

(defun org-dailies-edit-today ()
  (interactive)
  (let ((filename (expand-file-name (format-time-string "%Y-%m-%d.org")
                                    org-dailies-dir))
        (template (expand-file-name "template.org" org-dailies-dir)))
    (if (or (file-exists-p filename) (not (file-exists-p template)))
        (find-file filename)
      (progn
        (copy-file template filename)
        (find-file filename)))))

(defun org-dailies-edit-backlog ()
  (interactive)
  (find-file (expand-file-name "backlog.org" org-dailies-dir)))

(defun org-dailies-edit-template ()
  (interactive)
  (find-file (expand-file-name (format-time-string "template.org")
                               org-dailies-dir)))

(defun iterate-org-dailies-file-date (org-dailies-dated-file iterator)
  (let* ((date-string (replace-regexp-in-string "\.org$" "" org-dailies-dated-file))
         (calc-new-date (parse-time-string
                         (calc-eval (format "<%s> + %d" date-string iterator))))
         (changed-date-file (format "%d-%02d-%02d.org"
                                    (nth 5 calc-new-date)
                                    (nth 4 calc-new-date)
                                    (nth 3 calc-new-date))))
    changed-date-file))

(defun org-dailies-edit-previous-day ()
  (interactive)
  (let* ((time-travel-days 0)
         (time-travel-limit 5)
         (previous-day-file (iterate-org-dailies-file-date
                             (format-time-string "%Y-%m-%d.org") -1)))
    ;; loop until we find a previous day's entry, or bottom out after a year
    (while (and (< time-travel-days time-travel-limit)
                (not (file-exists-p (expand-file-name previous-day-file
                                                      org-dailies-dir))))
      (setq time-travel-days (+ time-travel-days 1))
      (setq previous-day-file (iterate-org-dailies-file-date previous-day-file -1)))
    (if (= time-travel-days time-travel-limit)
        (message "Couldn't find any daily notes files from the last year!")
      ;; if we get here, we found a previous file
      (find-file (expand-file-name previous-day-file org-dailies-dir)))))

(setq org-dailies-memory-months 3)
(defun org-dailies-forget ()
  (interactive)
  (let ((days-to-forget
         (calc-eval (format "%d * 31" org-dailies-memory-months)))
        (files-to-delete
         (shell-command-to-string
          (format "find %s -type f -mtime +%d"
                  (file-truename org-dailies-dir)
                  org-dailies-memory-months))))
    (if (not (string= files-to-delete ""))
        (when (or (yes-or-no-p (format
                                "Forget everything not read in the last %d month(s)?"
                                org-dailies-memory-months))
                  (ignore (message "Aborted")))
          (shell-command
           (format "find -type f -mtime +%d -delete"
                   (file-truename org-dailies-dir)
                   org-dailies-memory-months)))
      (message "No files old enough to forget!"))))

(defun org-dailies-push-unfinished-to-backlog ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "- \\[[ ]\\]" nil t 1)
        (progn
          (move-end-of-line nil)
          ;; point+1 contains the newline
          (append-to-file (match-beginning 0) (+ (point) 1)
                          (format "%s/backlog.org" org-dailies-dir))
          (kill-whole-line)))))
;; --

;; keybinds
(general-create-definer org-bindings
  :prefix "SPC o"
  :states '(normal emacs)
  :keymaps 'override)

;; org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))
(setq org-confirm-babel-evaluate nil)

;; org babel scratch buffer
(setq inhibit-startup-message nil)
;;(setq initial-major-mode 'org-mode)
;;(setq initial-scratch-message
;;      (with-temp-buffer
;;        (let* ((rnum (number-to-string (+ (random 3) 1)))
;;               (banner-file (concat "res/banners/" rnum ".txt")))
;;          (insert-file-contents (expand-file-name "res/org/scratch.org"
;;                                                  user-emacs-directory))
;;          (insert-file-contents (expand-file-name banner-file
;;                                                  user-emacs-directory))
;;          (insert "#+begin_src python")
;;          (insert "\n")
;;          (insert "\"\"\"")
;;          (insert "\n")
;;          (buffer-string))))

(org-bindings
  "" '(nil :which-key "org")
  ;; org-roam
  "r" '(nil :which-key "roam")
  "r f" 'org-roam-node-find
  "r t" 'org-roam-dailies-goto-today
  ;; org-agenda
  "a" '(nil :which-key "agenda")
  "a a" 'org-agenda
  "a l" 'org-agenda-list
  "a t" 'org-agenda-edit-tasks
  "a d" 'org-agenda-edit-dates
  ;; org-dailies
  "d" '(nil :which-key "dailies")
  "d t" 'org-dailies-edit-today
  "d T" 'org-dailies-edit-template
  "d p" 'org-dailies-edit-previous-day
  "d P" 'org-dailies-push-unfinished-to-backlog
  "d b" 'org-dailies-edit-backlog
  )

(use-package org-tree-slide)
