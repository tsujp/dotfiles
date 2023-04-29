;;; -*- lexical-binding: t -*-

;; (add-hook 'window-selection-change-functions (lambda ()
;;                                            (message "window change lambda triggered")))





;; FROM SCRATCH

;; (window-buffer (active-minibuffer-window))


;; (window-state-get (active-minibuffer-window))

;; ;; (((min-height . 4) (min-width . 10) (min-height-ignore . 1) (min-width-ignore . 4) (min-height-safe . 1) (min-width-safe . 2) (min-pixel-height . 19) (min-pixel-width . 80) (min-pixel-height-ignore . 19) (min-pixel-width-ignore . 32) (min-pixel-height-safe . 19) (min-pixel-width-safe . 16)) leaf (last . t) (pixel-width . 718) (pixel-height . 209) (total-width . 89) (total-height . 11) (normal-height . 1.0) (normal-width . 1.0) (parameters (clone-of . #<window 9 on  *Minibuf-1*>)) (buffer #<buffer  *Minibuf-1*> (selected) (hscroll . 0) (fringes 8 8 nil nil) (margins nil) (scroll-bars nil 0 t nil 0 nil nil) (vscroll . 0) (dedicated) (point . #<marker at 33 in  *Minibuf-1*>) (start . #<marker at 1 in  *Minibuf-1*>))))

;; (window-parameter fringes (active-minibuffer-window))

;; (get '(window-state-get (active-minibuffer-window)) 'fringes)

;; (let (mbb (window-state-get (active-minibuffer-window)))
;;   (message "==> %s" mbb))

;; (symbol-plist (window-state-get (active-minibuffer-window)))

;; (get (window-buffer (active-minibuffer-window)) 'fringes)

;; (type-of (window-buffer (active-minibuffer-window)))

;; (cdr (window-state-get (active-minibuffer-window) nil))

;; (defun xx/thing ()
;;   (message "==> %s" (frame-focus-state mini-frame-frame)))

;; (run-with-timer 3 nil #'xx/thing)

;; END FROM SCRATCH


  ;; (add-to-list 'window-selection-change-functions #'tsujp/miniframe-focus-indicator))
  ;; (add-to-list 'window-state-change-functions #'tsujp/another-try))

;; (defun tsujp/another-juan (frame)
;;   (message "==> %s" (frame-focus-state mini-frame-frame))
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window))
;;                     (with-current-buffer (window-buffer w)
;;                       (message "do? ==> %s" w))))))

;; (add-to-list 'window-state-change-functions #'tsujp/another-juan)


;; (message "x ==> %s" (window-state-get (active-minibuffer-window))))


;; Tried everything, I don't think this is possible with current emacs lol.
;; Might be a macOS thing I don't know, the second you scroll another
;; window or frame or buffer the subsequent input event goes poof.

; at least correctly reports t and nil
(defun tsujp/what-is-this ()
  (message "==> foo")
  (message "  f: %s" mini-frame-frame)
  (message "  cur w: %s" (selected-window))
  (message "  active?: %s" (minibuffer-window-active-p (selected-window)))
  (message "  x: %s" (frame-focus-state mini-frame-frame))
  ;; (message "  y: %s" (frame-focus-state wat))
  )

;;(add-hook 'buffer-list-update-hook #'tsujp/what-is-this)
;; (add-hook 'window-configuration-change-hook #'tsujp/what-is-this)
;;






;;; ; at least correctly reports t and nil
;; (defun tsujp/what-is-this ()
;;   (message "==> foo")
;;   (message "  cur w: %s" (selected-window))
;;   (message "  active?: %s" (minibuffer-window-active-p (selected-window)))
;;   )

;; (add-to-list 'window-state-change-hook #'tsujp/what-is-this)
;; ;;



;; Input focus
;; At any time only one frame in emacs is the _selected frame_ and the
;;   _selected window_ always resides within that frame.


;; `window-selection-change-functions` does _not_ trigger _focus_ changes which
;;   I had conflated as being closely related to _selection_ changes. MF puts the
;;   minibuffer in its own frame and for all (that I tested) keyboard
;;   interactions like `C-x 5 o` or `C-x o` (go to other frame and window
;;   respectively) or closing the minibuffer `C-g` treating a _selection_ change
;;   as a loss of input focus for border colour works fine -- HOWEVER if using
;;   a mouse you (currently in any other frame, say frame B) hover over frame A
;;   and scroll you will scroll that frame's (B) window's buffer BUT INPUT FOCUS
;;   WILL NOT BE LOST IN THE "ORIGINAL" FRAME A. Even though frame B is selected
;;   frame A has input focus such that if you continue to type you will be
;;   typing into frame A.


(setq msgxxx 0)
(defun tsujp/another-try (frame)
  (message "-- %s" msgxxx)
  (setq msgxxx (1+ msgxxx))
 
  (message "  received arg: %s" frame)
  (message "  mini frame's frame: %s" mini-frame-frame)
  ;; (message "  active minibuffer window: %s" (active-minibuffer-window))
  (message "  selected frame: %s" (selected-frame))

  ;; (message "  current buffer: %s" (current-buffer))
  ;; (message "  window params: %s" (window-parameters (active-minibuffer-window)))
  ;; (message "  minibuffer's buffer: %s" (window-buffer (active-minibuffer-window)))
  ;; (message "    --> selected: %s" (buffer-local-variables (window-buffer (active-minibuffer-window))))

  ;; Can look at `buffer` property `selected`, when NOT selected it is
  ;;   (selected) and when selected it is (selected . t)
  
  ;; (message "  window state get: %s" (window-state-get (active-minibuffer-window)))

  ;; We only care about frame events relating to Mini Frame's (MF) frame.
  ;; (if (eq frame mini-frame-frame)
  ;;     (message "  CONCERS MF")
  ;;     )

  ;; By default MF does not wrap the Eval minibuffer so we need to further
  ;;   discriminate but this is a good start vs. checking the frame first.
  (when (and (active-minibuffer-window) (eq frame mini-frame-frame))
    (message "  MF frame active AND update concerns it")
    ;; TODO this doesn't work if you scroll another window in the same
    ;;   frame, even though input cursor remains in (same frame) other window.
    (message "  focus is in minibuffer?: %s" (eq frame (selected-frame)))
    (message "  received frame focus state: %s" (frame-focus-state frame))
    ;; (message "  frame selected window: %s" (frame-selected-window frame))
    ;; (message "  window state get: %s" (window-state-get (active-minibuffer-window)))
    )
)


(setq msgnumx 0)
(defun tsujp/try-again (frame)
  (message "-- %s" msgnum)
  (setq msgnum (1+ msgnum))

  (message "  received arg: %s" frame)
  (message "  mini frame's frame: %s" mini-frame-frame)
  (message "  active minibuffer window: %s" (active-minibuffer-window))
  (message "  selected frame: %s" (selected-frame))

  ;; We only care about frame events relating to Mini Frame's (MF) frame.
  ;; (if (eq frame mini-frame-frame)
  ;;     (message "  CONCERS MF")
  ;;     )

  ;; By default MF does not wrap the Eval minibuffer so we need to further
  ;;   discriminate but this is a good start vs. checking the frame first.
  (when (and (active-minibuffer-window) (eq frame mini-frame-frame))
    (message "  MF frame active AND update concerns it")
    ;; TODO this doesn't work if you scroll another window in the same
    ;;   frame, even though input cursor remains in (same frame) other window.
    (message "  focus is in minibuffer?: %s" (eq frame (selected-frame)))
    (message "  received frame focus state: %s" (frame-focus-state frame))
    )
  )


(setq msgnum 0)
(defun tsujp/miniframe-focus-indicator (frame)
  (message "-- %s" msgnum)
  (setq msgnum (1+ msgnum))

  ;; We only care about changes relating to the mini-frame frame which the
  ;;   package conveniently stores as variable `mini-frame-frame`.
  ;; (when (eq mini-frame-frame frame)
  ;;     (message "  SOMETHING WITH MINIFRAME CHANGED"))

  ;; (when (eq mini-frame-frame frame) (eq mini-frame-frame (selected-frame))
  ;;      (message "  SOMETHING WITH MINIFRAME CHANGED"))
  ;;)
  
  ;;(message "  eq?: %s" (eq mini-frame-frame (selected-frame)))

  (message "  frame-selected-window %s" frame)
  (message "  xx: %s" (selected-window))
  ;; We only care about changes relating to the mini-frame frame which the
  ;;   package conveniently stores as variable `mini-frame-frame`.
  (when (eq mini-frame-frame frame)
    (cond
     ((eq mini-frame-frame (selected-frame)) ; gained selection (colour border).
      (message "  in mini-frame")
      (set-face-attribute 'child-frame-border mini-frame-frame
                      :background (catppuccin-get-color 'yellow)))
     (t ; lost selection (revert to default border).
      (message "  not in mini-frame")
      (set-face-attribute 'child-frame-border mini-frame-frame
                          :background (catppuccin-get-color 'overlay2)))))
  )
      ;; (redraw-frame mini-frame-frame)
      ;; (redraw-display))))

  ;; (redraw-frame mini-frame-frame)
  ;; (redraw-display)
  ;; (redraw-frame)
  ;; (force-mode-line-update)
  ;; (redisplay)
  
  ;; works
  ;; (if (eq mini-frame-frame (selected-frame))
  ;;     (message "  foobar"))

  ;; cond attempt
  ;; (cond ((eq mini-frame-frame (selected-frame)) (message "  in mini-frame"))
  ;;       (t (message "  not in mini-frame")))



;; LIKELY GARBAGE DELETE IT WHEN DONE.
;;  (message "  miniframe selected window: %s" mini-frame-frame)
;;  (message "  minibuffer-window-active-p?: %s" (minibuffer-window-active-p (minibuffer-window frame)))
;;  (message "  selected-window %s" (selected-window))
;;  (message "  frame-selected-window %s" frame)
  ;;(message "  selected-frame %s" (selected-frame))


;;; Mini Frame
;;; Moves the minibuffer to the top and provides other geometric customisations.
;;;
;;; docs: https://github.com/muffinmad/emacs-mini-frame
(use-package mini-frame
  :config
  ;; Implicitly shared frame colour.
  ;; (set-face-foreground 'line-number-current-line (catppuccin-get-color 'peach))
  ;; (set-face-attribute 'child-frame-border mini-frame-frame
  ;;                     :background (catppuccin-get-color 'overlay2))
  ;; The input frame.
  (setq mini-frame-show-parameters
    '((top . 20)
      (width . 0.85)
      (left . 0.5)
      ;; (set-face-background 'child-frame-border "white")
      ;; (child-frame-border "white")
      ;; (internal-border-color "white")
      ;; (mini-frame-internal-border-color "yellow")
      (child-frame-border-width . 1)))
  ;; The completion frame (if any).
  (setq mini-frame-completions-show-parameters
        '((height . 0.25)
          (width . 0.85)
          (left . 0.5)))
  ;; TODO Disable line number showing in this special frame
  ;; TODO add a 1px border or something?
  ;; TODO disable modeline in this frame?

  ;; Prefer outer-border I think?
  ;; docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html#index-outer-border

  ;; The background of the face `child-frame-border` is used to colour the
  ;;   borders here.
  ;; See heading `Internal Border` on https://www.gnu.org/software/emacs/manual/html_node/elisp/Frame-Layout.html

  ;; Frame `mini-frame-frame` is the input.
  ;; (modify-frame-parameters mini-frame-frame
  ;;                          '((child-frame-border-width . 5)
  ;;                            (child-frame-border "cyan")
  ;;                            (set-face-background 'child-frame-border "red" mini-frame-frame)))
  ;; Frame `mini-frame-completions-frame` is the... completions container frame.
  ;; (modify-frame-parameters mini-frame-completions-frame
  ;;                          '((child-frame-border-width . 5)
  ;;                            (internal-border-color "purple")))
  ;; (setq mini-frame-internal-border-color "red")
  ;; (set-face-background 'child-frame-border "green")
  ;; :custom-face
  ;; (child-frame-border ((t (:background "yellow"))))
  (mini-frame-mode 1)
  ;; (add-function :after after-focus-change-function #'tsujp/after-focus-test)
  ;; (add-to-list 'window-selection-change-functions #'tsujp/another-juan)
  ;; (add-to-list 'window-selection-change-functions #'tsujp/miniframe-focus-indicator))
  ;; (add-to-list 'window-state-change-functions #'tsujp/another-try))
  )

(provide 'i-mini-frame)
