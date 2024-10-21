;;; early-init.el -*- lexical-binding: t -*-

;;; Commentary

;; The first file Emacs reads when starting up and before a frame is produced is early-init.el. Its contents should not depend on any package or the proportions of any Emacs frame because even in-built packages are not-yet loaded and no frame yet exists.

;; Once early-init.el has completed, Emacs defers to init.el for initial frame creation and the rest of startup.

;;; Code

;;; Disable eager package loading

;; Disable default (in-built) package.el from loading packages. Using Elpaca for packages instead.
(setq package-enable-at-startup nil)
;; (setq use-package-compute-statistics t)


;;; Garbage collection

;; Increase the amount of consing before garbage collection runs; this should result in the garbage collector interrupting us less often and a smoother Emacs experience.
(setq gc-cons-threshold (* 1000 1000 100))
(setq gc-cons-percentage 0.3)


;;; Use plists for LSP

;; plists allow for faster deserialisation than hash-tables for lsp-mode.
(setenv "LSP_USE_PLISTS" "true")


;;; Frame configuration

;; Default frame configuration; tiling window managers will override this which is fine.

;; This starts Emacs maximised and with default dark coloured backgrounds to avoid flashes of light (e.g. load in light-mode and then swap to dark-mode).

;; The use of ~default-frame-alists~ makes these defaults for all frames; if only the initial frame is desired then ~initial-frame-alist~ should be used.

;; Don't resize non-fullscreen frames when we change font size etc so as to keep same column/row dimensions.
(setq frame-inhibit-implied-resize t)

;; `ns-use-native-fullscreen` must be set here before default-frame-alist or terrible rendering issues happen on macOS.
(setq ns-use-native-fullscreen nil)
(setq frame-resize-pixelwise t)
(push '(width . 162) default-frame-alist)
(push '(height . 60) default-frame-alist)
(push '(internal-border-width . 5) default-frame-alist)
;; default-frame-alist '((fullscreen . maximized)
;;                       (background-color . "#000000")
;;                       (ns-appearance . dark)
;;                       (ns-transparent-titlebar . t))

;; Disable certain graphical modes like showing toolbar icons as well as removing startup messages we don't care about.
(setq inhibit-startup-echo-area-message user-login-name)

;; Said graphical modes being disabled like this disabled them via frame parameters which skips their minor modes completely (TODO: I believe).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; TODO: Change 'native-comp-eln-load-path to XDG_CACHE location (eln-cache).



;;; Startup time

;; Avoiding inline lambdas in hooks hence a function (so hook can be removed without having to restart emacs).
(defun tsujp/startup-time-printout ()
  ;; (with-current-buffer (get-buffer-create "*scratch*")
  ;;   (insert (format
  ;; ";; Loaded in: %s (%s)
  ;; ;; Packages: %s
  ;; "
  ;;            (emacs-init-time)
  ;;            gc-elapsed
  ;;            (number-to-string (length package-activated-list))))))
  (message "init done: %s (gc time: %s)" (emacs-init-time) gc-elapsed))

(add-hook 'after-init-hook #'tsujp/startup-time-printout)
