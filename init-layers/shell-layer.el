(require 'general-keybinds-layer)

(general-create-definer shell-bindings
  :prefix "SPC s"
  :states '(normal emacs visual)
  :keymaps 'override)
(shell-bindings
  "" '(nil :which-key "shell")
  "e" 'eshell
  "s" 'shell)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))
