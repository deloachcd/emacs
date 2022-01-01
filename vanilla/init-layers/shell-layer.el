(use-package vterm)

(require 'general-keybinds-layer)
(general-create-definer shell-bindings
  :prefix "SPC s"
  :states '(normal emacs visual)
  :keymaps 'override)
(shell-bindings
  "" '(nil :which-key "shell")
  "m" 'sh-mode
  "e" 'eshell
  "s" 'vterm)

(require 'flycheck-layer)
(setq flycheck-sh-bash-executable "/usr/bin/bash")
(setq flycheck-sh-zsh-executable "/usr/bin/zsh")
(defun sh-mode-enable-flycheck-h ()
  "Enable flycheck for sh buffers"
  (flycheck-mode)
  (if (string-match-p (regexp-quote "\.zsh") ".zshrc")
      (flycheck-select-checker 'sh-zsh)
    (flycheck-select-checker 'sh-bash)))
;; TODO: set up shellcheck and see why this isn't working
;;(add-hook 'sh-mode-hook 'sh-mode-enable-flycheck-h)
