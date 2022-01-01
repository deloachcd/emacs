;; Configuration related to ivy/swiper/counsel, and which-key.

;; vertico provides primary autocompletion
(use-package vertico 
  :config
  (vertico-mode 1))

;; additional commands, like consult-buffer
(use-package consult)

;; orderless completion in vertico
(use-package orderless
  :init
  (setq completion-styles '(orderless)))

;; Extension to make find-file act like it's supposed to
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)))

(use-package company
  :defer
  :hook (prog-mode . company-mode))
