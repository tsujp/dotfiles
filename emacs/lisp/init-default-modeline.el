;;(setq mode-line-format nil)
;;(setq mode-line-format
;;      (list
 ;;      '(:propertize "%b" 'help-echo (buffer-file-name))
  ;;     (when (buffer-modified-p)
   ;;      (propertize "*")
    ;;     )
     ;;  ))
    (setq mode-line-format
          (list
           ;; value of `mode-name'
           "%m: "
           ;; value of current buffer name
           "buffer %b, "
           ;; value of current line number
           "line %l "
           "-- user: "
           ;; value of user
           (getenv "USER")))

(provide 'init-default-modeline)
