;; Load user extension from GitHub for syntax highlighting, etc.
(require 'glsl-mode)

;; Load my own extension for launching glslViewer from emacs
(require 'glsl-viewer)
(setq glsl-viewer-path "~/.local/bin/glslViewer")
(general-def 'normal glsl-mode-map "SPC m r" 'glsl-viewer-run)
