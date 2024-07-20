;;; early-init.el -*- lexical-binding: t -*-

;;; Commentary

;; The first file Emacs reads when starting up and before a frame is produced is early-init.el. Its contents should not depend on any package or the proportions of any Emacs frame because even in-built packages are not-yet loaded and no frame yet exists.

;; Once early-init.el has completed, Emacs defers to init.el for initial frame creation and the rest of startup.

;;; Code

;;;; Configuration macros

;; Both tsujp/req and tsujp/prov are arguably over-engineered.

;; Said macros provide a way to specify my config modules using short names without potential feature-name collision. For instance, a feature named just `os` is pretty generic so having that automatically become `tsujp/os` removes any feature order loading and shadowing and even having to check in the first place if there is such a collision.
(defmacro tsujp/req (feat-symbol)
  `(require (intern (concat tsujp/custom-config-dir "/" (symbol-name ,feat-symbol))) (concat (symbol-name ,feat-symbol) ".el")))

(defmacro tsujp/prov (feat-symbol)
  `(provide (intern (concat tsujp/custom-config-dir "/" (symbol-name ,feat-symbol)))))

;;;; Disable eager package loading

;; Disable default (in-built) package.el from loading packages. Using Elpaca for packages instead.
(setq package-enable-at-startup nil)


;;;; Garbage collection

;; Increase the amount of consing before garbage collection runs; this should result in the garbage collector interrupting us less often and a smoother Emacs experience.
(setq gc-cons-threshold (* 1000 1000 8))


;;;; Use plists for LSP

;; plists allow for faster deserialisation than hash-tables for lsp-mode.
(setenv "LSP_USE_PLISTS" "true")


;;;; Frame configuration

;; Default frame configuration; tiling window managers will override this which is fine.

;; This starts Emacs maximised and with default dark coloured backgrounds to avoid flashes of light (e.g. load in light-mode and then swap to dark-mode).

;; The use of ~default-frame-alists~ makes these defaults for all frames; if only the initial frame is desired then ~initial-frame-alist~ should be used.

;; Don't resize non-fullscreen frames when we change font size etc so as to keep same column/row dimensions.
(setq frame-inhibit-implied-resize t)

(setq frame-resize-pixelwise t)
      ;; default-frame-alist '((fullscreen . maximized)
      ;;                       (background-color . "#000000")
      ;;                       (ns-appearance . dark)
      ;;                       (ns-transparent-titlebar . t)))

;; Disable certain graphical modes like showing toolbar icons as well as removing startup messages we don't care about.
(setq inhibit-startup-echo-area-message user-login-name)

;; Said graphical modes being disabled like this disabled them via frame parameters which skips their minor modes completely (TODO: I believe).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (tool-bar-mode -1)
;; (menu-bar-mode -1)
;; (scroll-bar-mode -1)

;; TODO: Change 'native-comp-eln-load-path to XDG_CACHE location (eln-cache).


;;;; Startup time

(add-hook 'after-init-hook (lambda () (message "init done: %s" (emacs-init-time))))
