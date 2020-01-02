(provide 'buffer-layer)

(general-create-definer buffers
  :prefix "SPC b"
  :states '(normal emacs)
  :keymaps 'override)
(buffers
 "b" 'ivy-switch-buffer
 "n" 'next-buffer
 "p" 'previous-buffer
 "k" 'kill-buffer)

