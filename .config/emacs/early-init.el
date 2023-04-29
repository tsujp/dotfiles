;; -*- lexical-binding: t -*-

; Stop package.el loading packages prior to init.el running. We use straight.el for
;   package installation anyway.
(setq package-enable-at-startup nil)

(provide 'early-init)
