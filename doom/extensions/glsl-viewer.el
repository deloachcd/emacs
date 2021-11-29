(provide 'glsl-viewer)

(defvar glsl-viewer-path
  "/usr/bin/glslViewer"
  "Path to the glslViewer executable.")

(defun glsl-viewer-run ()
  "Run a GLSL shader and edit it with hot-reloading."
  (interactive)
  (cond ( ;; ensure we're running linux or macOS for now
         (not (or (string-equal system-type "gnu/linux") (string-equal system-type "darwin")))
         (message "Error: this package doesn't support non-Linux operating systems yet."))

          ;; ensure binary exists on configured path
        ((not (file-exists-p glsl-viewer-path))
         (message "Error: glslViewer binary not found! (is glsl-viewer-path set correctly?)"))

          ;; main logic
        (t
         (let* ((current-directory (file-name-directory buffer-file-name))
                (glsl-viewer-spec-file (concat current-directory ".viewerspec"))
                (glsl-viewer-spec (if (file-exists-p glsl-viewer-spec-file)
                                      (shell-command-to-string
                                       (concat "cat " glsl-viewer-spec-file " | xargs"))))
                (shell-init-command (concat glsl-viewer-path " "
                                            (or glsl-viewer-spec buffer-file-name) "\n")))
           (progn
             (shell "*glslViewer*")
             (comint-send-string "*glslViewer*" shell-init-command))))))
