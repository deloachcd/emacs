;; Configuration related to git integration with magit.
(require 'general-keybinds-layer)
(provide 'magit-layer)

(use-package magit
  :defer)

(general-create-definer magit-bindings
  :prefix "SPC g"
  :states '(normal emacs)
  :keymaps 'override)

(magit-bindings
  "g" 'magit-status
  "c" 'with-editor-finish)
