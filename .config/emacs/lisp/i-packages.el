;;; -*- lexical-binding: t -*-

;;; Configures setup for installing packages.

;;; Alternative package _installer_ straight.el installation boilerplate.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Package _configurer_ use-package.el, install it using straight.el.
(straight-use-package 'use-package)

;;; Set use-package.el to always receive `:straight t` which means it will install
;;;   the package (via straight.el) if it's not already instaled.
(setq straight-use-package-by-default t)

;;; Notes on use-package keywords

;;;; :init
;;;; Executes code before the package is loaded.

;;;; :config
;;;; Executes code after the package has loaded (including if lazy loaded).

(provide 'i-packages)
