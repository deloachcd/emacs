;; Configuration related to the general behavior of emacs as a
;; text editor for both text files and source code files, making
;; heavy use of evil-mode because I can't live in a text editor
;; without vim bindings.
;;
;; Much of the ideas here are taken from Witchmacs, by snackon:
;; https://github.com/snackon/Witchmacs

;; start directly to scratch buffer
(setq inhibit-startup-message t)

;; Scroll one line at a time, like it should be
(setq scroll-conservatively 100)

;; Indentation

;; Prefer spaces over tabs, standard width of 4, because
;; not every editor is smart enough to render tabs like
;; this out of the box.
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode nil)
(setq c-basic-offset tab-width)
(setq backward-delete-char-untabify-method 'hungry)

;; Only indent the line when at BOL or in a line's indentation. Anywhere else,
;; insert literal indentation.
(setq-default tab-always-indent nil)

;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

;; Show mark for indentation via tabs, as they can be
;; inconsistent
(setq whitespace-style '(tab-mark))

;; It seems like neither of these conflict with each other,
;; so I can just enable both of them in prog modes.
(use-package dtrt-indent
  :hook (prog-mode . dtrt-indent-mode))
(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))

(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

;; Configuration of electric pairs and electric indent happens
;; in another layer now
(require 'electric-layer)

(defun add-hooks (mode-hook list-of-hooks)
  (dolist (a-hook list-of-hooks)
    (add-hook mode-hook a-hook)))

(setq prog-mode-minor-modes '(display-line-numbers-mode
                              hl-line-mode
                              show-paren-mode
                              electric-pair-mode
                              whitespace-mode))
(setq text-mode-minor-modes '(hl-line-mode
                              show-paren-mode))

(add-hooks 'prog-mode-hook prog-mode-minor-modes)
(add-hooks 'text-mode-hook text-mode-minor-modes)

;; Don't litter every working directory with backups
(defvar backup-dir (concat user-emacs-directory "backups"))
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Copypasting outside of emacs
(setq x-select-enable-clipboard t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Prettify symbols globally
(global-prettify-symbols-mode t)

;; Type "y" or "n" instead of "yes" or "no" in minibuffer
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always just jump to the real file, don't ask
(setq vc-follow-symlinks t)

;; It's nice to have this available for when we want to see
;; which functions we're using
(use-package command-log-mode
  :defer t)

;; ensure line number width doesn't change as we scroll down
(setq display-line-numbers-width-start t)

;; NOTE I don't think I actually need this...
;;(use-package all-the-icons
;;  :config
;;  (let ((has-fonts (= (shell-command
;;                       "test -e ~/.local/share/fonts/all-the-icons.ttf")
;;                      0)))
;;    (unless has-fonts
;;      (all-the-icons-install-fonts))))

;; I <3 lifting code from DOOM emacs
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :hook (yaml-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold))))

;; Visually distinguish bracket pairs by color
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; doom-quit because why not?
(defun doom-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or (yes-or-no-p (format "%s" (or prompt "Really quit Emacs?")))
      (ignore (message "Aborted"))))

(defvar +doom-quit-messages
  '(;; from Doom 1
    "Please don't leave, there's more demons to toast!"
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. UNIX is much worse."
    "Don't leave yet -- There's a demon around that corner!"
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; from Doom 2
    "Don't go now, there's a dimensional shambler waiting at the shell prompt!"
    "Get outta here and go back to your boring programs."
    "If I were your boss, I'd deathmatch ya in a minute!"
    "Look, bud. You leave now and you forfeit your body count!"
    "You're lucky I don't smack you for thinking about leaving.")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun +doom-quit-fn (&rest _)
  (doom-quit-p
   (format "%s\n%s"
           (propertize (nth (random (length +doom-quit-messages))
                            +doom-quit-messages)
                       'face '(italic default))
           "Really quit Emacs?")))

(setq confirm-kill-emacs #'+doom-quit-fn)
