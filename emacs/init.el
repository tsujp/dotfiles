;;; init.el --- full emacs configuration -*- lexical-binding: t -*-
;;; Commentary:

;;
;;       ::::::::::   :::   :::       :::      ::::::::   ::::::::
;;      :+:         :+:+: :+:+:    :+: :+:   :+:    :+: :+:    :+:
;;     +:+        +:+ +:+:+ +:+  +:+   +:+  +:+        +:+
;;    +#++:++#   +#+  +:+  +#+ +#++:++#++: +#+        +#++:++#++
;;   +#+        +#+       +#+ +#+     +#+ +#+               +#+
;;  #+#        #+#       #+# #+#     #+# #+#    #+# #+#    #+#
;; ########## ###       ### ###     ###  ########   ########
;;

;; config largely taken/inspired/built-upon from:
;;   * https://github.com/purcell/emacs.d
;;   * https://github.com/aaronbieber/dotfiles

;;; Code:

;; (let ((default-directory user-emacs-directory)
;;       (file-name-handler-alist nil)
;;       (gc-cons-percentage .6)
;;       (gc-cons-threshold most-positive-fixnum)
;;       (read-process-output-max (* 1024 1024)))

;;   ;; mark safe variables early so that tangling won't break
;;   (put 'after-save-hook 'safe-local-variable
;;        (lambda (value) (equal value '(org-babel-tangle t))))
;;   (put 'display-line-numbers-width 'safe-local-variable 'integerp)

;;   ;; tangle and compile if necessary only, then load the configuration
;;   (let* ((.org "emacs.org")
;;          (.el (concat (file-name-sans-extension .org) ".el"))
;;          (modification-time
;;           (file-attribute-modification-time (file-attributes .org))))
;;     (require 'org-macs)
;;     (unless (org-file-newer-than-p .el modification-time)
;;       (require 'ob-tangle)
;;       (org-babel-tangle-file .org .el "emacs-lisp")
;;       (byte-compile-file .el))
;;     (load-file .el))

;;   ;; set the working directory to home regardless of where Emacs was started from
;;   (cd "~/")

;;   ;; collect garbage when all else is done
;;  (garbage-collect))
(org-babel-load-file "~/.config/emacs/emacs.org")
(cd "~/")
;;; init.el ends here
