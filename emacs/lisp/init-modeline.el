;;; init-modeline.el --- minimal modeline -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default mode-line-format
                  (list
           ;; value of `mode-name'
           "%m: "
           ;; value of current buffer name
           "buffer %b, "
           ;; line number and column
           "%l:%c "
           "-- user: "
           ;; value of user
           (getenv "USER")))

(provide 'init-modeline)
;;; init-modeline.el ends here
