;;; package --- init-flycheck
;;;
;;; Commentary:
;;; Set up configuration for flycheck
;;; 
;;; Code:

;; Enable globally
(global-flycheck-mode)

;; Configure syntax checkers
(defvar flycheck-python-flake8-executable "/usr/bin/flake8")

(provide 'init-flycheck)
;;; init-flycheck ends here
