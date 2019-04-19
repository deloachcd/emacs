;;; package --- init-evil
;;;
;;; Commentary:
;;; Set up configuration for evil-mode
;;; 
;;; Code:

; Enable evil-mode
(evil-mode 1)

; Keybinds
(global-set-key (kbd "C-x h") 'switch-to-next-buffer)
(global-set-key (kbd "C-x l") 'switch-to-prev-buffer)

(provide 'init-evil)
;;; init-evil ends here
