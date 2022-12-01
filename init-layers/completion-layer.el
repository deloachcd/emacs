;; Configuration related to ivy/swiper/counsel, and which-key.

;; vertico provides primary autocompletion
(use-package vertico 
  :config
  (vertico-mode 1))

;; additional commands, like consult-buffer
(use-package consult)

(general-create-definer consult-bindings
  :prefix "SPC c"
  :states '(normal emacs visual)
  :keymaps 'override)

(consult-bindings
  "" '(nil :which-key "consult")
  "f" 'consult-find
  "g" 'consult-grep)

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

(use-package marginalia
  :init
  (marginalia-mode))

(use-package company
  :defer
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  ;; minimal company config -- 'tab and go' should solve issue
  ;; with unwanted autocompletions for variables like 'nil'
  (company-tng-configure-default)
  (setq company-backends '((company-capf company-dabbrev-code))))
