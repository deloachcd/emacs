(provide 'global-layer)

(general-create-definer globals
  :prefix "SPC"
  :states '(normal emacs)
  :keymaps 'override)
(globals
 "SPC" 'execute-extended-command)

