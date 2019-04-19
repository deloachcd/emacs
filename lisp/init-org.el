;;; package --- init-org.el
;;;
;;; Commentary:
;;; This is my file for configuring org-mode's behavior
;;;
;;; Code:

(require 'org)

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-log-done t)

(provide 'init-org)
;;; init-org ends here
