(provide 'doomlike-detect-indent)

(defvar-local doomlike-inhibit-indent-detection nil
  "A buffer-local flag that indicates whether `dtrt-indent' should try to detect
indentation settings or not. This should be set by editorconfig if it
successfully sets indent_style/indent_size.")

(use-package editorconfig)

(use-package dtrt-indent
  :after editorconfig
  :hook (prog-mode . doomlike-detect-indentation-h)
  :init
  ;; Reduced from the default of 5000 for slightly faster analysis
  (setq dtrt-indent-max-lines 2000)
  :config
  (defun doomlike-detect-indentation-h ()
    (when buffer-file-name
      (editorconfig-mode +1)
      (editorconfig-apply)
      (if (boundp 'editorconfig-properties-hash)
          (unless (or (gethash 'indent_style editorconfig-properties-hash)
                      (gethash 'indent_size editorconfig-properties-hash))
            (dtrt-indent-mode +1))
        (message "`editorconfig-apply' called but no table exists, some shit went wrong!")))))
