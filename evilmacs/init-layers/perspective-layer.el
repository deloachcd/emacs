(use-package perspective
  :config
  (persp-mode))

(general-create-definer perspective-bindings
  :prefix "SPC r"
  :states '(normal emacs)
  :keymaps 'override)

(perspective-bindings
  ""   '(nil :which-key "perspective")
  "r" 'persp-switch
  "n" 'persp-next
  "p" 'persp-prev)