;;; early-init.el -*- lexical-binding: t -*-

;; (profiler-cpu-start 1000000)
;; (trace-function 'message)
;; (profiler-cpu-start profiler-sampling-interval)
;; (trace-function 'display-startup-echo-area-message)

;;; Commentary

;; The first file Emacs reads when starting up and before a frame is produced is early-init.el.
;; Its contents should not depend on any package or the proportions of any Emacs frame because
;; even in-built packages are not-yet loaded and no frame yet exists.

;; Once early-init.el has completed, Emacs defers to init.el for initial frame creation and the
;; rest of startup.

;; Useful blogpost: https://joyeecheung.github.io/blog/2025/01/11/executable-loading-and-startup-performance-on-macos/
;; Upper bound of startup speed on macOS with use-package macros and Elpaca is about 0.4 seconds.
;; Could add (kill-emacs) to the end of init.el and use hyperfine benchmark Emacs startup:
;;   # hyperfine /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
;; Keeping that same (kill-emacs) at the end of init.el can also get a trace with xctrace:
;;   # xctrace record --output . --template "Time Profiler"  --launch -- /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs
;; Then open that trace:
;;   # open Launch_Emacs...

;;; Code

;;; Disable eager package loading

;; Disable default (in-built) package.el from loading packages. Using Elpaca for packages instead.
(setq package-enable-at-startup nil)
;; (setq use-package-compute-statistics t)

;;; Garbage collection

;; Increase the amount of consing before garbage collection runs; this should result in the
;; garbage collector interrupting us less often and a smoother Emacs experience.
(setq gc-cons-threshold (* 1000 1000 100))
(setq gc-cons-percentage 0.3)
;; (setq gc-cons-threshold most-positive-fixnum)


;;; Use plists for LSP

;; plists allow for faster deserialisation than hash-tables for lsp-mode.
;; TODO: Delete this? Equivalent for Eglot?
(setenv "LSP_USE_PLISTS" "true")


;;; Frame configuration

;; Default frame configuration; tiling window managers will override this which is fine. The
;; use of ~default-frame-alists~ makes these defaults for all frames; if only the initial frame
;; is desired then ~initial-frame-alist~ should be used.

;; Don't resize non-fullscreen frames when we change font size etc so as to keep same column/row dimensions.
(setq frame-inhibit-implied-resize t)

;; `ns-use-native-fullscreen` must be set here before default-frame-alist or terrible rendering issues happen on macOS.
(setq ns-use-native-fullscreen nil)
(setq frame-resize-pixelwise t)
(push '(width . 162) default-frame-alist)
(push '(height . 60) default-frame-alist)
(push '(internal-border-width . 5) default-frame-alist)
(push '(inhibit-double-buffering . t) default-frame-alist)
;; (push '(background-color . "#000000") default-frame-alist)

;; TODO: I can't remember why I needed to set this.
(setq-default left-margin-width 1)

;; Disable certain graphical modes like showing toolbar icons as well as removing startup
;; messages we don't care about.

;; Said graphical modes being disabled like this disabled them via frame parameters which skips their minor modes completely (TODO: I believe).
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; bug#13208 https://lists.gnu.org/archive/html/bug-gnu-emacs/2012-12/msg00954.html
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))

;; TODO: Change 'native-comp-eln-load-path to XDG_CACHE location (eln-cache).

;; No X resources, NS (macOS) defaults, or Windows Registry settings.
(setq inhibit-x-resources t)

(unless (eq system-type 'darwin)
  (setq command-line-ns-option-alist nil))


;;; Startup messaging and buffer display

(setq inhibit-startup-message t ; Alias for inhibit-startup-screen / inhibit-splash-screen
      initial-buffer-choice nil
      initial-startup-buffer-menu t)



;; ---------------------------------- yoinked from minimal-emacs, let's see
;; TODO: Not sure why I'd need to set this versus using the coding system stuff I already have.
;; (set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

;; ;; TODO: I don't know if that's true, (trace-function 'display-startup-screen) at the top of early-init.el without this shows nothing still.. so this feels unneeded.
;; ;; Suppress the vanilla startup screen completely. We've disabled it with
;; ;; `inhibit-startup-screen', but it would still initialize anyway.
;; ;; (advice-add #'display-startup-screen :override #'ignore)

;; (defun minimal-emacs--reset-inhibited-vars-h ()
;;         ;; (setq-default inhibit-redisplay nil) ; Can cause artifacts
;;         (setq-default inhibit-message nil)
;;         (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibited-vars-h))

;; (add-hook 'post-command-hook
;;           #'minimal-emacs--reset-inhibited-vars-h -100)
;; -------------------------------- end yoink


;; Wrapped toggle-frame-fullscreen to re-position Emacs' frame when fullscreened.
;; This is here so it can be used immediately in init.el

(defun tjp/display-top-offset ()
  "Return top offset (in pixels) required for fullscreen Emacs position
on known displays. If a display isn't known 0 is returned, which
results in no offset being applied."
  ;; TODO: The .workarea is compared as the default when toggle-frame-fullscreen is set, so on 16" mbp non-fullscreen its 0 38 1728 1079 but with default toggle-frame-fullscreen it is 0 32 1728 1085.
  (let ((disp (car (display-monitor-attributes-list))))
    (or
     (let-alist disp
       (cond
        ((string= .source "NS")
         ;; 16" macbook display with notch, offset from top is 8.
         (when (and (equal .workarea '(0 32 1728 1085)))
           8))))
     ;; Default is 0, no offset.
     0)))

(defun tjp/toggle-frame-fullscreen ()
  "Wrapper around `#'toggle-frame-fullscreen' which positions Emacs
appropriately on displays that need it (e.g. Macbook with a display
notch at the top)."
  (interactive)
  (let ((frame (selected-frame)))
    (toggle-frame-fullscreen frame)
    (sleep-for 0.01) ; Any non-zero value is okay, fixes some terrible race condition Emacs has with modifying frame parameters. TODO: File this race condition as a bug report.
    (if (eq (frame-parameter frame 'fullscreen) 'fullboth)
        (let ((offset (tjp/display-top-offset))
              (initial-height (frame-inner-height)))
          (modify-frame-parameters frame
                                   `((top . ,offset)
                                     (height . (text-pixels . ,(- initial-height offset)))))))))

;;; ispell completions

;; Do not add ispell to completion-at-point. This might be improving performance.
(setopt text-mode-ispell-word-completion nil)


;;; Startup time

;; Avoiding inline lambdas in hooks hence a function (so hook can be removed without having to
;; restart emacs).
(defun tjp/startup-time-printout ()
  ;; TODO: With Elpaca do I want to somehow get a count of loaded packages after init?
  (message "init done: %s (gc time: %s)" (emacs-init-time) gc-elapsed))

(add-hook 'after-init-hook #'tjp/startup-time-printout)

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:



;; Crude benchmarks show this reduces startup from about 0.5s to 0.43 to 0.44 so.. keep for now I guess?

(defun minimal-emacs--reset-inhibit-redisplay ()
  "Reset inhibit redisplay."
  (setq-default inhibit-redisplay nil)
  (remove-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay))

(when (and t
           (not (daemonp))
           (not noninteractive))
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t)
  (add-hook 'post-command-hook #'minimal-emacs--reset-inhibit-redisplay -100))
