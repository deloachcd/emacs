;;; package --- init-indent-settings
;;;
;;; Commentary:
;;; Set up indent-related settings
;;; 
;;; Code:

;; make indentation commands use space only (never tab character)
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
(setq-default evil-shift-width tab-width)

;; Make backspace delete whole tabs, not just one space at a time.
(setq backward-delete-char-untabify-method 'hungry)

;; If I ever want to use tabs that aren't 4 spaces in a language for
;; some reason, I can do it here

(provide 'init-indent-settings)
;;; init-indent-settings ends here
