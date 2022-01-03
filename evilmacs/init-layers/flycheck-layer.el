(provide 'flycheck-layer)

(use-package flycheck
  :init
  (setq flycheck-indication-mode 'right-fringe)
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [16 48 112 240 112 48 16] nil nil 'center))

;; Languages we want:
;; - Python (probably via its own executable)
;; - C/C++ (probably via clang)
;; - bash (probably via its own executable)
