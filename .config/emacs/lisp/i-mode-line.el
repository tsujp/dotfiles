;;; -*- lexical-binding: t -*-

;;; Custom mode line.

;; docs: https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Format.html

;; `mode-line-format` buffer local variable which holds a _mode line construct_ -- a template controlling what is displayed in that buffer's mode line.
;; Mode line constructs are lists, strings, symbols, and numbers kept in buffer-local variables. When used in a mode line construct each of these data types has a specific behaviour.
;; Behaviour: https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Data.html

;; (use-package emacs
;;   :after evil
;;   :config
;;   (setq evil-normal-state-tag   (propertize "NO" 'face '((:background "green" :foreground "black")))
;;       evil-insert-state-tag   (propertize "IN" 'face '((:background "red")))
;;       evil-visual-state-tag   (propertize "VI" 'face '((:background "grey80" :foreground "black")))
;;       evil-motion-state-tag   (propertize "MO" 'face '((:background "blue")))
;;       evil-emacs-state-tag    (propertize "EM" 'face '((:background "orange" :foreground "black")))
;;       evil-operator-state-tag (propertize "OP" 'face '((:background "purple")))))


;; (setq-default mode-line-format
;;               (list
;;                " "
;;                "%l,%c"
;;                " "
;;                "%o"
;;                " "
;;                "%b"
;;                " "
;;                (when (boundp 'boon) boon-state-string)
;;                ))
;;(setq mode-line-position-column-line-format (list "%l,%c"))
;;(setq mode-line-percent-position (list "%o "))


















;; TODO: That bottom-left modeline thing. `C-h v` and `mode-line-format` will
;;       explain the current modeline format. Default appears to be:
;;
;;("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
 ;;(vc-mode vc-mode)
;;"  " mode-line-modes mode-line-misc-info mode-line-end-spaces)
;;
;; Can then look up those to get some understanding.

;; By default it looks like -:---
;;
;; Annoyingly I think it is _not_ 1 character = 1 representation of state, nor
;; is it 1 character = controlled by 1 function, you'll see what I mean. One
;; last thing, I'll now be referring to these 5 "characters" as positions.
;;
;; Here are the position numbers so they are easier to refer to.
;;
;;           1   2   3   4   5
;;           -   :   -   -   -
;;
;; Positions 1 and 2 are controlled by `mode-line-mule-info` which by default
;; looks like `-:`.
;;   1. The buffer's coding system.
;;
;;      Emacs has its own internal representation of a buffers contents so any
;;      time a buffers contents, in part or whole, has enter or leave Emacs it
;;      needs to be decoded and encoded respectively. If you open a file you must
;;      decode the file to see its contents. If you save a file you must encode
;;      its contents. If you send part of a buffer to a subprocess like a shell
;;      or language service that part of the buffer must be encoded for that
;;      external process.
;;
;;      The strategy used for encoding/decoding is the coding system. Ones you'll
;;      be familiar with are ASCII or UTF-8.
;;
;;      Some coding systems force a certain line ending, some don't. So we also
;;      have the option to set a preference for a certain line ending convention
;;      which is displayed at position 2. Anyway, for position 1 some common
;;      coding systems have special aliases, lots of others are binned under
;;      `*`.

;; Positions can have mnemonics I found this out by `C-h v mode-line-mule-info RET` which mentions the word `mnemonic`. Following the bindings.el link didn't reveal much regarding coding system mnemonics (but it did for others I'll mention later). It turns out each coding system defines it's own mnemonic ([link](https://github.com/emacs-mirror/emacs/blob/master/lisp/international/mule.el#L679)) and we can use function `coding-system-mnemonic` to return it ([link](https://github.com/emacs-mirror/emacs/blob/master/lisp/international/mule.el#L997)). The mnemonic is also shown as the first character if you describe a specific coding system e.g. via `M-x describe-coding-system RET`.

;; You can list all mnemonics used in the message buffer with:

;; ```elisp
;; (dolist (cs coding-system-list)
;;   (message "%c" (coding-system-mnemonic cs)))
;; ```

;;
;;      
;;   2. The buffer's line ending style, represented by up to 3 strings. Confusingly one
;;    of these strings is a single character, the rest are multi-character:
;;      2a. `:` LF (Unix).
;;      2b. `(DOS)` CRLF.
;;      2c. `(Mac)` CR.

;; Position 3 and 4 describe the permissions you have over the buffer (read/write) and the buffers cleanliness (edited or not).

(provide 'i-mode-line)
