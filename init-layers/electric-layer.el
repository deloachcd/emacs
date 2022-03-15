;; This is its own layer to provide a system like DOOM emacs has, where
;; lines are automagically indented based on a list of keywords for each
;; mode without having to hit "enter" and create a newline.
;;
;; Leaps and bounds better than the default behavior for bash scripts.
(provide 'electric-layer)

(defvar-local electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")

;; I might not need to return a lambda to do this, but who cares, this
;; works like it does in DOOM emacs and that's what I want.
(defun get-electric-hook ()
  "Returns a function to hook into 'electric-indent-functions, which allows lines to autoindent on encountering each word in electric-indent-words."
  (lambda (chars)
    (when (and (eolp) electric-indent-words)
      (save-excursion
        (backward-word)
        (looking-at-p (concat "\\<" (regexp-opt electric-indent-words)))))))

;; Bracket pair-matching
(setq electric-pair-pairs '((?\( . ?\))
                            (?\{ . ?\})
                            (?\[ . ?\])))
