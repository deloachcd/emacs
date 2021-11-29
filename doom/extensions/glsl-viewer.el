(provide 'glsl-viewer)

(defgroup glsl-viewer nil
  "Run a GLSL shader and edit it with hot-reloading.")

(defvar glsl-viewer-path
  "~/.local/bin/glslViewer"
  "Path to the glslViewer executable.")

;; run: $PATH/glslViewer {current_buffer_filename} $(cat $PWD/.glsl-viewer | xargs)
;; buffer-file-name
(defun glsl-viewer-run ()
  "Run a GLSL shader and edit it with hot-reloading."
  (interactive)
  (cond ( ;; ensure we're running linux for now
         (not (string-equal system-type "gnu/linux"))
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
                                       (concat "cat " glsl-viewer-spec-file " | xargs")))))
           (shell-command (concat glsl-viewer-path " "
                                  (or glsl-viewer-spec buffer-file-name)))))))
