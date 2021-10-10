;; Configuration related to the general behavior of emacs as a
;; text editor for both text files and source code files, making
;; heavy use of evil-mode because I can't live in a text editor
;; without vim bindings.
;;
;; Much of the ideas here are taken from Witchmacs, by snackon:
;; https://github.com/snackon/Witchmacs
(provide 'editing-layer)

;; Vim-like bindings
(use-package evil
  :init
  (progn
    (setq evil-want-keybinding nil)
    (setq evil-overriding-maps nil
          evil-intercept-maps nil))
  :config (evil-mode t))

;; Make vim-like bindings play nice everywhere
(use-package evil-collection
  :config (evil-collection-init))

(use-package evil-surround)

;; Scroll one line at a time, like it should be
(setq scroll-conservatively 100)

;; Indentation
(setq-default tab-width 4)
(setq-default standard-indent 4)
(setq-default indent-tabs-mode t)
(setq backward-delete-char-untabify-method 'hungry)
(setq c-basic-offset tab-width)

(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

;; Bracket pair-matching
(setq electric-pair-pairs '((?\{ . ?\})
                            (?\[ . ?\])))

;; Hook for all programming language editing major modes
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (hl-line-mode)
                            (show-paren-mode)
                            (company-mode)
                            (evil-surround-mode)))

;; Hook for text editing major modes, mainly org-mode
(add-hook 'text-mode-hook (lambda ()
                            (hl-line-mode)
                            (show-paren-mode)
                            (evil-surround-mode)))

;; Don't litter every working directory with backups
(defvar backup-dir "~/.emacs.d/backups")
(setq backup-directory-alist (list (cons "." backup-dir)))

;; Copypasting outside of emacs
(setq x-select-enable-clipboard t)

;; Disable ring-bell
(setq ring-bell-function 'ignore)

;; Prettify symbols globally
(global-prettify-symbols-mode t)

;; Type "y" or "n" instead of "yes" or "no" in minibuffer
(defalias 'yes-or-no-p 'y-or-n-p)

;; It's nice to have this available for when we want to see
;; which functions we're using
(use-package command-log-mode)
