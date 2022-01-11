;; Configuration related to org-mode, org-roam, etc.
(require 'general-keybinds-layer)

;; Where we'll store all our org documents
(setq org-root "~/Sync/Documents/org")

;; Automagically mix variable and monospace fonts
(use-package mixed-pitch
  :defer
  :init (setq mixed-pitch-set-height t))

(defun org-mode-setup ()
  (setq org-hide-emphasis-markers t)
  (mixed-pitch-mode 1)
  (visual-line-mode 1)
  (org-indent-mode 1))

(defun org-font-setup ()
  (dolist (face '((org-document-title . 1.5)
                  (org-level-1 . 1.25)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil
                        :inherit 'variable-pitch :weight 'bold :height (cdr face)))

  ;; TODO: maybe fork the theme and set this there?
  (if 'doom-homage-white-active
      (dolist (face '(org-block-begin-line
                      org-block-end-line))
        (set-face-attribute face nil :foreground "#7c7c7c"))))

(use-package org
  :defer
  :hook (org-mode . org-mode-setup)
  :config
  (progn
    (setf org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
    (setq org-ellipsis " ▾")
    (set-face-underline 'org-ellipsis nil)
    (org-font-setup)))

;; old value: '("♣" "♠" "♦" "♥")
(use-package org-bullets
  :defer
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("✸" "✱" "❖" "♦")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-root "/roam"))
  (org-roam-dailies-directory "journal/")
  (org-roam-db-location
   (expand-file-name (concat "org-roam." (system-name) ".db")
                     org-roam-directory))
  :config
  (org-roam-db-autosync-enable)
  (setq org-M-RET-may-split-line nil))

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
      (list (concat org-root "/agenda/tasks.org")
            (concat org-root "/agenda/dates.org")))

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
            "scheme" "sqlite")))
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
  (find-file (concat org-root "/agenda/tasks.org")))

(defun org-agenda-edit-dates ()
  (interactive)
  (find-file (concat org-root "/agenda/dates.org")))

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
(setq inhibit-startup-message t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
      (with-temp-buffer
        (let* ((rnum (number-to-string (+ (random 3) 1)))
               (banner-file (concat "/res/banners/" rnum ".txt")))
            (insert-file-contents (concat user-emacs-directory "/res/org/scratch.org"))
            (insert-file-contents (concat user-emacs-directory banner-file))
            (insert "#+begin_src python")
            (insert "\n")
            (insert "\"\"\"")
            (insert "\n")
            (buffer-string))))

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
  )
