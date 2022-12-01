(setq denote-directory "~/Sync/Documents/org/denote")

;; let's try out denote
(use-package denote
  :general
  ('normal org-mode-map "SPC m d" 'denote-link))

(use-package consult-notes
  :config
  (setq consult-notes-sources (list (list "Denote" ?d denote-directory)))
  (when (locate-library "denote") (consult-notes-denote-mode))
  )

(general-create-definer denote-bindings
  :prefix "SPC d"
  :states '(normal emacs)
  :keymaps 'override)

(denote-bindings
  "" '(nil :which-key "denote")
  "n" 'denote
  "d" 'consult-notes)
