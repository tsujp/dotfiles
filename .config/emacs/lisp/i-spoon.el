;;; -*- lexical-binding: t -*-

(define-minor-mode spoon-local-mode
  :init-value nil
  :lighter (:eval (spoon-modeline-string))
  :keymap nil
  (if (not spoon-local-mode)
      (progn
        (spoon-set-state 'spoon-off-state))
    (setq spoon-mode-map-alist
          (list (cons 'spoon-select-state)
                (cons 'spoon-insert-state)))
    (spoon-set-natural-state)))

(defun spoon-initialize ()
  (unless (minibufferp)
    (spoon-local-mode 1)))

(defun turn-on-spoon-mode ()
  "Turn on Spoon in the current buffer."
  (interactive)
  (spoon-local-mode 1))

(defun turn-off-spoon-mode ()
  "Turn off Spoon in the current buffer."
  (interactive)
  (spoon-local-mode -1))

(provide 'i-spoon)
