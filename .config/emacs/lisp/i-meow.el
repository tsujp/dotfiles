;; -*- lexical-binding: t -*-

(defmacro tsujp/meow-hoc (fn-name meow-cmds-list)
  "Creates an interactive function which interactively-calls each Meow command in the list."
  `(defun ,fn-name ()
     (interactive)
     (mapc #'call-interactively ,meow-cmds-list)))

;; Inserts at the start of the existing text on the line, if you want the actual
;;   start of the line do `meow-line - meow-insert` (default here: x-i) instead.
(tsujp/meow-hoc tsujp/insert-start-text-of-line '(meow-join meow-append))

(tsujp/meow-hoc tsujp/append-end-of-line '(meow-line meow-append))

(defun meow-setup-helix ()
  (setq meow-use-cursor-position-hack t)
  (setq meow-use-enhanced-selection-effect nil)
  ;; Meow fallbacks for when a selection-only command does not have a selection.
  (setq meow-selection-command-fallback
        '((meow-change . meow-change-char)
          (meow-kill . meow-delete) ; Change default meow-C-k to delete char at point.
          (meow-cancel-selection . keyboard-quit)
          (meow-pop-selection . meow-pop-grab)
          (meow-beacon-change . meow-beacon-change-char)))
  ;; Meow keymaps.
  ;;; Motion.
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  ;;; Normal.
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("h" . meow-left)
   '("v" . meow-right-expand) ; puts you into a selection mode that also allows free movement.
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("w" . meow-mark-word)
   '("W" . meow-next-word) ; meow-next-symbol instead?
   '("J" . meow-next-expand)
   '("x" . meow-line)
   '("i" . meow-insert)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("d" . meow-kill)
   '("I" . tsujp/insert-start-text-of-line)
   '("a" . meow-append)
   '("A" . tsujp/append-end-of-line)
   '("o" . meow-open-below)
   '("O" . meow-open-above)
   '("u" . meow-undo)
   '("y" . meow-save)
   '("p" . meow-yank)
   '("c" . meow-change)
   ;;;; Select until char (inclusive and exclusive)
   '("f" . meow-find) ; inclusive.
   '("F" . meow-find-expand)
   '("t" . meow-till) ; exclusive.
   '("T" . meow-till-expand)
   '("'" . repeat)
   '("<escape>" . meow-cancel-selection)))

(defun tsujp/meow-cursor ()
  (setq meow-use-cursor-position-hack nil
        ; TODO: Maybe enhanced selection to true? Is it adding that fading at the end?
        meow-use-enhanced-selection-effect nil))

;; TODO: Thing for non-whitespace? It's on the docs example too: C-h f meow-thing-register RET
(defun tsujp/meow-things ()
  (meow-thing-register 'angle
                       '(pair ("<") (">"))
                       '(pair '("<") (">")))
  (add-to-list 'meow-char-thing-table
               '(?a . angle)))

(defun meow-word ()
  "Expand word/symbol under cursor."
  (interactive)
  (if (and (use-region-p)
           (equal (car (region-bounds))
                  (bounds-of-thing-at-point 'word)))
      (progn
        (when (string-match-p
               (or (car regexp-search-ring) "")
               (buffer-substring (region-beginning) (region-end)))
          (pop regexp-search-ring))
        (meow-mark-symbol 1))
    (progn
      (when (and (mark)
                 (equal (car (region-bounds))
                        (bounds-of-thing-at-point 'symbol)))
        (meow-pop-selection))
      (meow-mark-word 1))))

(defun tsujp/meow-ergo-keys ()
  ;; NORMAL
  (meow-define-keys 'normal
    ;; Expansion
    '("0" . meow-expand-0)
    '("1" . meow-expand-1)
    '("2" . meow-expand-2)
    '("3" . meow-expand-3)
    '("4" . meow-expand-4)
    '("5" . meow-expand-5)
    '("6" . meow-expand-6)
    '("7" . meow-expand-7)
    '("8" . meow-expand-8)
    '("9" . meow-expand-9)
    '("'" . meow-reverse)

    ;; Movement
    '("k" . meow-prev)
    '("l" . meow-right)
    '("j" . meow-next)
    '("h" . meow-left)

    ;; Expansion
    '("o" . meow-word)))

(defun meow-setup ()
  (progn
  (tsujp/meow-cursor)
  '(tsujp/meow-things)
  (tsujp/meow-ergo-keys)))

(use-package meow
  :config
  (meow-setup)
  (meow-setup-indicator) ; Modeline.
  (meow-global-mode 1))

(provide 'i-meow)
