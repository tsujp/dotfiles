;; -*- lexical-binding: t -*-

; Stop package.el loading packages prior to init.el running. We use straight.el for
;   package installation anyway.
(setq package-enable-at-startup nil)


(setq
 ;; Startup speed, supress annoying warnings.
 gc-cons-threshold 100000000 ; increase consing before gc runs
 byte-compile-warnings '(not obsolete)
 warning-suppress-log-types '((comp) (bytecomp))
 native-comp-async-report-warnings-errors 'silent

 ;; Silence startup message.
 inhibit-startup-echo-area-message (user-login-name)

 ;; Default (graphical) frame configuration.
 frame-resize-pixelwise t
 default-frame-alist '((fullscreen . maximized)
                       (background-color . "#000000")
                       (ns-appearance . dark)
                       (ns-transparent-titlebar .t))
)

(tool-bar-mode -1)

(provide 'early-init)
