(provide 'sh-layer)

(defun ensure-newline (string)
  "Ensure string ends in a newline."
  (if (string= (substring string -1) "\n")
      string
    (concat string "\n")))

(defun sh-eval-buffer ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (buffer-string))))

(defun sh-eval-region ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (buffer-substring
					(region-beginning) (region-end)))))

(defun sh-eval-line ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (thing-at-point 'line))))

(general-create-definer sh
  :prefix "SPC s"
  :states '(normal emacs visual)
  :keymaps 'override)

(sh
 "m" 'sh-mode
 "se" 'eshell
 "ss" 'shell
 "eb" 'sh-eval-buffer
 "el" 'sh-eval-line
 "er" 'sh-eval-region)
