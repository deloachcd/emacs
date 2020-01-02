(provide 'eval-layer)

(defun ensure-newline (string)
  "Ensure string ends in a newline."
  (if (string= (substring string -1) "\n")
      string
    (concat string "\n")))

(defun eval-buffer-sh ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (buffer-string))))

(defun eval-region-sh ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (buffer-substring
					(region-beginning) (region-end)))))

(defun eval-line-sh ()
  (interactive)
  (process-send-string "*shell*"
		       (ensure-newline (thing-at-point 'line))))

(general-create-definer lang-eval
  :prefix "SPC e"
  :states '(normal emacs visual)
  :keymaps 'override)
(lang-eval
  ;; elisp
  "eb" 'eval-buffer
  "el" 'eval-last-sexp
  "er" 'eval-region
  ;; bash
  "bb" 'eval-buffer-sh
  "br" 'eval-region-sh
  "bl" 'eval-line-sh
  )
