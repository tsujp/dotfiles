;;; -*- lexical-binding: t -*-

;;; All packages associated with completions frameworks.

;;; Configuration

;;;; Vertico
;;;; Vertical completion framework using built-in emacs completion engine. Other
;;;;   packages can interface with Vertico simply by using emacs' default
;;;;   completion engine since that is what Vertico is using unlike Helm or Ivy.
;;;;
;;;; docs: https://github.com/minad/vertico
(use-package vertico
  :config
  (vertico-mode 1))

;; TODO marginalia, orderless etc.

;;;; Corfu
;;;; Shows completion candidates the current point in a popup either below or
;;;;   above the point. Uses in-built Emacs completion facilities (so ties in
;;;;   with Vertico).
;;;;
;;;; docs: https://github.com/minad/corfu
(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

;;;;; Uncomment to debug Corfu (from their docs).
(setq debug-on-error t)

(defun force-debug (func &rest args)
  (condition-case e
      (apply func args)
    ((debug error) (signal (car e) (cdr e)))))

(advice-add #'corfu--post-command :around #'force-debug)
;;;; End debug Corfu.

(provide 'i-completions)
