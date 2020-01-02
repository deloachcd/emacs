(provide 'help-layer)

(general-create-definer help
  :prefix "SPC h"
  :states '(normal emacs)
  :keymaps 'override)
(help
  "k" 'describe-key
  "f" 'describe-function)
