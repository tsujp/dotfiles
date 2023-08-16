;;; -*- lexical-binding: t -*-

;;; Evil and evil-associated package configuration.

;;; Utility functions.

;; `:wq` to save and kill in-focus buffer.
(defun tsujp/save-and-kill-this-buffer ()
  "Save and then kill current buffer."
  (interactive)
  (save-buffer)
  (kill-this-buffer))

;;; Configuration.

;;;; Evil Leader
;;;; Vim leader key in evil mode. Should be loaded before evil so initial buffers (e.g. scratch) have leader functionality.
;;;;
;;;; docs: https://github.com/cofi/evil-leader
(use-package evil-leader
  :init
  (setq evil-want-integration t) ; default.
  (setq evil-want-keybinding nil)
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ","))

;; Associative list of Evil state names to their cursor and mode line properties.
;;   - If no colours given mode line tag's foreground inherits cursor's colour.
;;   - If one colour given that colour becomes the mode line tag's foreground.
;;   - If two colours given they become the mode line tag's {fore,back}ground.
;; XXX: Trying out non-theme cursor colours so cursor is distinct (visual grokking).
(setq evil-state-presentations
      ;; Normal mode.
      '((normal . ((box "#FFFF00") ; Yellow (electric).
                   "NORMAL"))
        ;; Insert mode.
        (insert . (((bar . 3) "#FFFFFF") ; White.
                   "INSERT"))
        ;; Visual mode.
        (visual . ((box "#00FFFF") ; Cyan (electric).
                   "VISUAL"))
        ;; Motions available but no editing.
        (motion . ((box "red") ; TODO: Colours.
                   "MOTION"
                   ("red" "black")))
        ;; Like insert but only replaces.
        (replace . ((box "red") ; TODO: Colours.
                    "REPLACE"
                    ("red" "black")))
        ;; After entering operator but before entering motion or text object.
        (operator . ((box "#FF00FF") ; Magenta.
                     "OPERATOR"))
        ;; Mimics Emacs default by setting all Evil bindings (except C-z) to
        ;;   return to normal mode.
        (emacs . ((box "red") ; TODO: Colours.
                  "EMACS"
                  ("red" "black")))))


;;;; Evil
;;;;
;;;; docs: https://github.com/emacs-evil/evil
(use-package evil
  :after (evil-leader catppuccin-theme) ; Using Catppuccin's colours below.
  :init
  (setq
   global-visual-line-mode t ; Turn it on everywhere so Evil can respect it.
   evil-respect-visual-line-mode t ; Movement commands move by visual, not logical, line.
   blink-cursor-mode nil ; Never blink cursor (0 for blink forever).
   evil-want-minibuffer t ; Enable evil in minibuffer.
   ;; evil-normal-state-cursor (list 'box (catppuccin-get-color 'peach)) ; Normal mode.
   ;; evil-normal-state-cursor '(box "#F9CC6C") ; Normal mode.
   ;; evil-normal-state-cursor '(box "#CCFF00") ; Normal mode (fluro yellow).
   ;; evil-visual-state-cursor '(box "#85DACC") ; Visual mode.
   ;; evil-visual-state-cursor '(box "#FF00CC") ; Visual mode.
   )

   ;; (dolist (ele evil-state-colours)
   ;;   (message "Key: %s -- Val: %s" (car ele) (cdr ele)))

   ;; ;; Works! Just colours though e.g. (insert . "#FFFFFF")
   ;; (dolist (ele evil-state-colours)
   ;;   (set (intern (format "evil-%s-state-cursor" (car ele))) (list 'box (cdr ele)))
   ;;   (message "Key: %s -- Val: %s" (car ele) (cdr ele)))

   ;; Customise mode line and cursors with Evil.
   (dolist (ele evil-state-presentations)
     (let ((state-name (car ele))
           (cursor-properties (nth 0 (cdr ele)))
           (tag-text (nth 1 (cdr ele)))
           (mode-line-colours (nth 2 (cdr ele))))
     ;; Cursor properties.
     (set
      (intern (format "evil-%s-state-cursor" state-name))
      cursor-properties)
     ;; Mode line tag properties.
     (set
      (intern (format "evil-%s-state-tag" state-name))
      (propertize tag-text
                  'face
                  (let ((mlc-length (length mode-line-colours)))
                     (cond
                      ((eql 0 mlc-length)
                       `(:foreground ,(car (last cursor-properties))))
                      ((eql 1 mlc-length)
                       `(:foreground ,(nth 0 mode-line-colours)))
                      ((eql 2 mlc-length)
                       `(:foreground ,(nth 0 mode-line-colours)
                         :background ,(nth 1 mode-line-colours)))
                      (t (:foreground "red")) ; TODO: Error colour.
                      ))))))
  :config
  (evil-mode 1))

;;;; Evil Collection
;;;; Adds evil keybindings to places evil misses.
;;;;
;;;; docs: https://github.com/emacs-evil/evil-collection
(use-package evil-collection
    :after evil
    :custom (evil-collection-setup-minibuffer t) ; evil keybindings in minibuffer.
    :config
    (setq evil-collection-mode-list
    '(ag dired magit mu4e which-key))
    (evil-collection-init))

;;;; Evil Commentary
;;;; tpope's vim-commentary but in emacs (commenting code).
;;;;
;;;; docs: https://github.com/linktohack/evil-commentary
   (use-package evil-commentary
    :after evil
    :config (evil-commentary-mode 1))

;;;; Evil Surround
;;;; tpope's vim-surround but in emacs (surrounding text with stuff).
;;;;
;;;; docs: https://github.com/emacs-evil/evil-surround
(use-package evil-surround
    :after evil
    :config
    (global-evil-surround-mode 1))

;;;; Evil Snipe
;;;; 2-character motions move around text fast (like both vim-seek/vim-sneak).
;;;;
;;;; docs: https://github.com/hlissner/evil-snipe
   (use-package evil-snipe
    :after evil
    :config
    (setq evil-snipe-scope 'visible
          evil-snipe-spillover-scope 'buffer)
    (evil-snipe-mode))

;;;; Evil Goggles
;;;; Flashes a colour when performing evil things.
;;;;
;;;; docs: https://github.com/edkolev/evil-goggles
   (use-package evil-goggles
    :after evil
    :config
    ;; Duration of "blocking" hints like a deletion hint. The hint displays and
    ;;   then the action proceeds so keep this short or the delay will be very
    ;;   very annoying. Default is nil which defers to `evil-goggles-duration`
    ;;   default of 0.200 which is a base global for this and the async hint
    ;;   duration setting.
    (setq evil-goggles-blocking-duration 0.050)
    ;; This can be as long or short as you'd like as it does not delay the action
    ;;   being performed e.g. a shift indentation. The action and hint happen
    ;;   at the same time.
    (setq evil-goggles-async-duration 0.500)
    (evil-goggles-mode))

(provide 'i-evil)
