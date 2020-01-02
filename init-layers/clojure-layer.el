(provide 'clojure-layer)

(general-create-definer clojure
  :prefix "SPC j"
  :states '(normal emacs visual)
  :keymaps 'override)

(clojure
 "j" 'cider-jack-in
 "d" 'cider-jack-in-default
 "m" 'cider-mode
 "eb" 'cider-eval-buffer
 "el" 'cider-eval-last-sexp
 "er" 'cider-eval-region)
