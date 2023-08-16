;;; -*- lexical-binding: t -*-

;;; General (and appearing first) emacs configuration.

;;; TEMPORARY FIX WITH MACOS NATIVE COMP ERRORS UNTIL EMACS 28.3 OR 29
(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))

;;; Decent window frame upon startup.
    (if (display-graphic-p)
        (progn
          (setq default-frame-alist
                '(
                  (width . 104)
                  (height . 55)))))

;;; Thin window edges.
(set-window-fringes nil 0 0)
(set-window-margins nil 0 0)

;;; Typography.
;; TODO appears emacs isn't using `medium` correctly, so normal weight will be
;;      semibold instead when using LG monitor
;; TODO when on retina screen go back to medium for the weights? Do this
;;      programatically? Likely with hammerspoon sending a message to emacs?
;;      can emacs detect retina? ppi?
(when (and (display-graphic-p) (eq system-type 'darwin)
  (set-face-attribute 'default nil
		      :family "Zed Mono Extended"
                      :height 150
                      :weight 'semibold)))

;;; Defaults.
(setq-default
     ;; custom (generated) lisp code location
     custom-file (expand-file-name "custom.el" user-emacs-directory)
     fill-column 80                          ; 80 char wide babbyyy
     delete-by-moving-to-trash t             ; delete by moving to trash
     select-enable-clipboard t               ; unify emacs and system clipboard
     sentence-end-double-space nil           ; single space after fullstop
     indent-tabs-mode nil                    ; sane whitespace, please
     initial-scratch-message ";; Scratch"    ; initial scratch message
     inhibit-splash-screen t                 ; hide welcome screen
     inhibit-startup-echo-area-message t     ; no echo area message
     buffer-file-coding-system 'utf-8-unix   ; utf8 encoding
     locale-coding-system 'utf-8-unix        ; utf8 encoding
     require-final-newline t                 ; add newline on buffer save
     display-line-numbers-type 'visual       ; set line numbers to relative
     display-line-numbers-grow-only t
     gc-cons-threshold 100000000             ; increase consing before gc runs
     read-process-output-max (* 1024 1024)   ; increase max bytes read per chunk
     display-raw-bytes-as-hex t              ;
     tab-always-indent 'complete             ; indentation & completion with TAB
     completion-cycle-threshold 3            ; TAB wraps if completion list small
     ;; hide commands in M-x not applicable to current mode
     read-extended-command-predicate #'command-completion-default-include-p
)

    (set-default-coding-systems 'utf-8-unix) ; utf8
    (prefer-coding-system 'utf-8-unix)       ; utf8
    (global-display-line-numbers-mode)       ; enable relative line numbers
    (global-hl-line-mode)                    ; enable hilight current line
;    (scroll-bar-mode -1)                     ; hide scroll
    (menu-bar-mode -1)                       ; hide menu bar
    (tool-bar-mode -1)                       ; hide tool bar
    (fset 'yes-or-no-p 'y-or-n-p)            ; yes/no -> y/n
(global-font-lock-mode 1)                ; force-enable font-face
(display-time-mode -1) ; disable time in modeline
(display-battery-mode -1) ; disable battery in modeline
(display-battery-mode t)

;;; Rule.
(setq display-fill-column-indicator-character ?\u2502) ; unused atm
(add-hook 'org-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'markdown-mode-hook #'display-fill-column-indicator-mode)

;;; Scrolling.
(setq
 scroll-conservatively 1000                   ; only 'jump' when moving this far
 scroll-margin 6                              ; scroll N lines to screen edge
 scroll-step 1                                ; keyboard scroll one line at a time
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; mouse scroll N lines
 mouse-wheel-progressive-speed nil            ; don't accelerate scrolling
 fast-but-imprecise-scrolling t               ; redraw immediately when scrolling (v)
 jit-lock-defer-time 0)

(provide 'i-general)
