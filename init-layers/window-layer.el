(provide 'window-layer)

(general-create-definer windows
  :prefix "SPC w"
  :states '(normal emacs)
  :keymaps 'override)
(windows
  "/" 'split-window-right
  "-" 'split-window-below
  "h" 'windmove-left
  "j" 'windmove-down
  "k" 'windmove-up
  "l" 'windmove-right)
