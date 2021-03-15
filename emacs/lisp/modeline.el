;;; init-modeline.el --- minimal modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default mode-line-format
                  (list
           ;; value of `mode-name'
           "%m"
           ;; value of current buffer name
           "%b, "
           ;; line number and column
           "%l,%c "))

(provide 'init-modeline)
;;; init-modeline.el ends here
