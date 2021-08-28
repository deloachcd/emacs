;; Configuration related to git integration with magit.
(require 'general-keybinds-layer)
(provide 'magit-layer)

(use-package magit)

(general-create-definer magit-bindings
  :prefix "SPC g"
  :states '(normal emacs)
  :keymaps 'override)

(magit-bindings
  "s" 'magit-status
  "c" 'with-editor-finish)
