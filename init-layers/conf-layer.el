;; Provide helpful functionality for dealing with various types of config files
(provide 'conf-layer)

;; YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))

;; nginx config files
(require 'nginx-mode)
