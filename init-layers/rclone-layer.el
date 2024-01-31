;; This layer enables automatic note syncing via rclone and an S3 bucket.

(defcustom rclone-bin-path nil
  "Path to a working rclone binary for syncing files to the cloud.")

(cond ((eq system-type 'gnu/linux)
       (setq-default rclone-bin-path "/usr/bin/rclone"))
      ((eq system-type 'windows-nt)
       (setq-default rclone-bin-path "~/.local/bin/rclone.exe")))

(use-package firestarter
  :config
  (firestarter-mode)
  (add-to-list 'safe-local-eval-forms
               '(setq-local firestarter
                           (concat rclone-bin-path
                                   " sync . s3:emacs-org-notes"))))
