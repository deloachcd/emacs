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
  ('normal org-mode-map "SPC m e" 'org-babel-execute-src-block))

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
  (find-file (expand-file-name "agenda/tasks.org" org-root)))

(defun org-agenda-edit-dates ()
  (interactive)
  (find-file (expand-file-name "agenda/dates.org" org-root)))

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
  )

(use-package org-tree-slide)

;; let's try out denote
(use-package denote
  :custom
  (denote-directory (expand-file-name "denote" org-root)))
