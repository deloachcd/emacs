;; Provide helpful functionality for dealing with various types of config files
(provide 'conf-layer)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\.j2\\'" . yaml-mode))
(add-hooks 'yaml-mode-hook prog-mode-minor-modes)

;; nginx config files
(require 'nginx-mode)

;; Dockerfiles
(use-package dockerfile-mode
  :config
  (require 'dockerfile-mode))
