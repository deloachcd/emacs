;; Configuration for `helpful', a help buffer extension
;; for emacs that provides more contextual information
(require 'general-keybinds-layer)

(use-package helpful)

(help-bindings
  "f" 'helpful-callable
  "v" 'helpful-variable
  "k" 'helpful-key)
