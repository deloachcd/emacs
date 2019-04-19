;;; package --- init-helm
;;;
;;; Commentary:
;;; Set up configuration for helm
;;; 
;;; Code:

;; Enable globally
(require 'helm-config)

;; Keybinds
(require 'helm-config)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(provide 'init-helm)
;;; init-helm ends here
