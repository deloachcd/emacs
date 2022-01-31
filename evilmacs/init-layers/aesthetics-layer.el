;; This layer is for disabling GUI elements, loading our preferred color theme
;; and scaling fonts properly before package refresh.

;; Slight padding for content in frame
(set-fringe-mode 5)

;; Load themes from ~/.emacs.d/themes
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes" user-emacs-directory))
;; modus theme configuration
(progn
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-syntax '(faint))
  (load-theme 'modus-operandi t))

;; This function is used to set the fixed and variable pitch fonts, with font
;; size being set relative to display resolution. There's no formula for this
;; at this point, so all resolutions are hardcoded (which will probably look
;; better anyway)
(defun set-fonts-from-display-resolution (fixed-pitch-font variable-pitch-font)
  (progn
    (defun get-display-resolution-windows ()
      "TODO: actually implement this"
      "1920x1080")
    (defun get-display-resolution-linux ()
      (shell-command-to-string "xrandr | grep '*' | head -n 1 | awk '{ printf $1 }'"))

    (defun set-fonts-from-heights (fixed-pitch-height variable-pitch-height)
      (set-face-attribute 'default nil
                          :font fixed-pitch-font :height fixed-pitch-height)
      (set-face-attribute 'fixed-pitch nil
                          :font fixed-pitch-font :height fixed-pitch-height)
      (set-face-attribute 'variable-pitch nil
                          :font variable-pitch-font :height variable-pitch-height)
      (add-to-list 'default-frame-alist (cons 'font 'default))
      (set-frame-font 'default nil t))

    (let ((resolution (cond ((string-equal system-type "gnu/linux")
                             (get-display-resolution-linux))
                            ((string-equal system-type "windows-nt")
                             (get-display-resolution-windows)))))

      (cond (;; 4k
             (string-equal resolution "3840x2160")
             (set-fonts-from-heights 135 125))

            ;; 1080p
            ((string-equal resolution "1920x1080")
             (set-fonts-from-heights 130 120))

            ;; Default case - same as 1080p for now
            (t (set-fonts-from-heights 120 130))))))

;; This function sets default params for a frame, and resizes the current frame
;; to that size
(defun set-frame-defaults (frame-width frame-height)
  (if (display-graphic-p)
      (let ((frame-size-params '((width . frame-width) (height . frame-height))))
        (setq initial-frame-alist frame-size-params)
        (setq default-frame-alist frame-size-params)
        (when window-system (set-frame-size (selected-frame) frame-width frame-height)))))

;; We call our functions to apply their changes here
(set-fonts-from-display-resolution "Ubuntu Mono" "Noto Sans")
(set-frame-defaults 88 36)

(use-package mood-line
  :init
  (setq mood-line-show-encoding-information t)
  (setq mood-line-show-eol-style t)
  :config
  (mood-line-mode))

(use-package dashboard
  :init
  (setq dashboard-center-content t)
  (setq dashboard-set-footer nil)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)
                          (projects . 5))))

;; tab bar aesthetic configuration
(setq tab-bar-new-button-show nil)
(setq tab-bar-close-button-show nil)
(setq tab-bar-button-margin 4)
(defun padded-tab-name-function ()
  "Function that just adds spaces surrounding the name of the current buffer."
  (concat " " (buffer-name (window-buffer (minibuffer-selected-window))) " "))
(setq tab-bar-tab-name-function 'padded-tab-name-function)
