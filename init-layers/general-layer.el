(provide 'general-layer)

;; custom theme
(load-theme 'badger t)

;; mode switches
(counsel-mode 1)
(menu-bar-mode -1)

;; global keybindings
(general-create-definer globals
  :prefix "SPC"
  :states '(normal emacs)
  :keymaps 'override)
(globals
 "SPC" 'execute-extended-command)

