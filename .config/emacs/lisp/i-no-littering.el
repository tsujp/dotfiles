;;; -*- lexical-binding: t -*-

;;; Stop polluting directory tree with ~ and # files everywhere.

;; Configuration files default: no-littering-etc-directoy as `etc/` under user-emacs-directory
;; Persistent data files default: no-littering-var-directory as `var/` under user-emacs-directory

    (use-package no-littering
      :init
        (setq no-littering-etc-directory
              (expand-file-name "config/" user-emacs-directory))
        (setq no-littering-var-directory
              (expand-file-name "data/" user-emacs-directory))
        :config
        ;; Store auto-save files under the no-littering-var-directory.
        (setq auto-save-file-name-transforms
              `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(provide 'i-no-littering)
