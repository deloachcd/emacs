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

;; keybinds
(general-create-definer org-agenda-bindings
  :prefix "SPC a"
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

(org-agenda-bindings
  "" '(nil :which-key "agenda")
  ;; org-agenda
  "a" 'org-agenda
  "l" 'org-agenda-list
  "t" 'org-agenda-edit-tasks
  "d" 'org-agenda-edit-dates)

(use-package org-tree-slide)
