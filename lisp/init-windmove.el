;;; package --- init-windmove
;;;
;;; Commentary:
;;; Set up windmove-related keybinds
;;; 
;;; Code:

(global-set-key (kbd "C-c k") 'kill-buffer) ; remap this to allow hjkl windmove navigation

;; Navigate windows with hjkl
(global-set-key (kbd "C-x h") 'windmove-left)
(global-set-key (kbd "C-x j") 'windmove-down)
(global-set-key (kbd "C-x k") 'windmove-up)
(global-set-key (kbd "C-x l") 'windmove-right)

(provide 'init-windmove)
;;; init-windmove ends here
