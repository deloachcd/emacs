(provide 'file-layer)

(defun dotfile-reload ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(general-create-definer files
  :prefix "SPC f"
  :states '(normal emacs)
  :keymaps 'override)
(files
  "f" 'counsel-find-file
  "dR" 'dotfile-reload)
