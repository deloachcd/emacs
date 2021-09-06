;; This layer provides SPC bindings for many common "emacs system"
;; bindings prefixed by C-x, along with general.el for defining
;; keybindings related to a specific configuration layer's
;; provided functionality.
;;
;; Every configuration layer that defines its own bindings with
;; general.el should `require' this layer
(provide 'general-keybinds-layer)

;; Keybindings not in this file:
;; org-mode and org-roam bindings are defined in org-layer.el
;; magit bindings are defined in magit-layer.el

;; Bail out of prompts with ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; General provides a unified interface for key definitions,
;; perfect for spacemacs-style bindings with evil mode
(use-package general)

(general-create-definer root-bindings
  :prefix "SPC"
  :states '(normal emacs visual)
  :keymaps 'override)

(root-bindings
  "SPC" 'execute-extended-command)

;; TODO: SPC e as general eval layer, not just elisp
(general-create-definer elisp-bindings
  :prefix "SPC e"
  :states '(normal emacs visual)
  :keymaps 'override)

(elisp-bindings
  "b" 'eval-buffer
  "s" 'eval-last-sexp
  "r" 'eval-region
  "i" 'indent-pp-sexp)

(general-create-definer buffer-management-bindings
  :prefix "SPC b"
  :states '(normal emacs)
  :keymaps 'override)
(buffer-management-bindings
  "s" 'ivy-switch-buffer
  "n" 'next-buffer
  "p" 'previous-buffer
  "k" 'kill-buffer)

(defun dotfile-reload ()
  "Reloads emacs configuration from init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(general-create-definer file-bindings
  :prefix "SPC f"
  :states '(normal emacs)
  :keymaps 'override)
(file-bindings
  "f" 'find-file
  "dR" 'dotfile-reload)

(general-create-definer help-bindings
  :prefix "SPC h"
  :states '(normal emacs)
  :keymaps 'override)
(help-bindings
  "k" 'describe-key
  "f" 'describe-function
  "v" 'describe-variable
  "a" 'apropos)

(general-create-definer shell-bindings
  :prefix "SPC s"
  :states '(normal emacs visual)
  :keymaps 'override)
(shell-bindings
  "m" 'sh-mode
  "e" 'eshell
  "s" 'shell)

;; NOTE: indent-whole-buffer defined in editing layer
(general-create-definer indentation-bindings
  :prefix "SPC i"
  :states '(normal emacs visual)
  :keymaps 'override)
(shell-bindings
  "r" 'indent-region
  "b" 'indent-whole-buffer)

;; these are here for the sake of consistency - Alt bindings
;; ought to be more commonly used for their convenience
(general-create-definer window-management-bindings
  :prefix "SPC w"
  :states '(normal emacs)
  :keymaps 'override)
(window-management-bindings
  "/" 'split-window-right
  "-" 'split-window-below
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right)
