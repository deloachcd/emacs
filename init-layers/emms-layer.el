(use-package emms
  :init
  (setq emms-source-file-default-directory "~/Music")
  :config
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))


;; DEADBEEF dependencies:
;; - libdispatch0 / libdispatch-dev
;; - libblockruntime0 / libblocksruntime-dev
;; - yasm
;; - intltool
;; - autoconf
;; - automake
;; - libtool
;; - autopoint
