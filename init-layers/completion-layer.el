;; vertico provides primary autocompletion
(use-package vertico 
  :config
  (vertico-mode 1))

;; orderless completion in vertico
(use-package orderless
  :init
  (setq completion-styles '(orderless)))
