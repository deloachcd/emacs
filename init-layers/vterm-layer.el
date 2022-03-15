(require 'general-keybinds-layer)

(use-package vterm
  :config
  ;; fix invisible progress text in apt commands due to green background
  (when (string= (car custom-enabled-themes) "modus-operandi")
    (set-face-attribute 'vterm-color-black nil :foreground "#000000")))

(use-package multi-vterm
  :config
  (general-create-definer vterm-bindings
    :prefix "SPC v"
    :states '(normal emacs visual)
    :keymaps 'override)
  (defun multi-vterm-project-or-new ()
    "create a vterm buffer based on current project if in one, otherwise create new session"
    (interactive)
    (if (project-current)
        (multi-vterm-project)
      (multi-vterm)))
  (vterm-bindings
    "" '(nil :which-key "vterm")
    "v" 'multi-vterm-project-or-new
    "n" 'multi-vterm
    "d" 'multi-vterm-dedicated-toggle)
  (general-def 'normal vterm-mode-map "SPC m n" 'multi-vterm-next)
  (general-def 'normal vterm-mode-map "SPC m p" 'multi-vterm-prev))

;; useful for bailing out of nano, when git merge pulls it up
(general-def 'normal vterm-mode-map "X" 'vterm-send-C-x)

;; Integrate vterm with emacs's man page display functionality
(add-to-list 'vterm-eval-cmds '("man" man))
