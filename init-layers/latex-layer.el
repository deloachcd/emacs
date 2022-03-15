(defun xelatex-buffer-to-pdf ()
  "Compile current buffer's XeLaTeX contents into PDF"
  (interactive)
  (shell-command (concat "xelatex " buffer-file-name)))

(general-define-key :states 'normal :keymaps 'latex-mode-map
                    "SPC m x" 'xelatex-buffer-to-pdf)
