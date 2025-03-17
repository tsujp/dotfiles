;;; init.el -*- lexical-binding: t -*-

;;; Commentary

;; This is the main initialisation point for Emacs; default packages are available now so any
;; (using default packages) shared configuration is done here (e.g. a macro). We then load
;; individual package modules which include their configurations.

;;; Code

;;;; Meta information

;; Compute these once instead of over-and-over at each callsite.
(defconst tsujp/modules-dir "modules")
(defconst tjp/sitelisp-dir "site-lisp")
(defconst tsujp/is-gui (display-graphic-p))
(defconst tsujp/is-mac (eq system-type 'darwin))

;;;; Load path

;; Directory created (if necessary) and added to load-path.
(add-to-list 'load-path (directory-file-name (expand-file-name (locate-user-emacs-file tsujp/modules-dir))))
(add-to-list 'load-path (directory-file-name (expand-file-name (locate-user-emacs-file tjp/sitelisp-dir))))

;;;; Env Vars / XDG directories

;; Set environment variables in Emacs by sourcing them from a proper shell session.

;; Added exec-path-from-shell package to site-lisp, then require and execute it here to ensure XDG variables are set before Emacs uses those variables for various packages later. Was getting multiple values for (e.g.) cache directories depending on when a subsequent package was loaded. This appears to resolve that.
;; XXX: This (might) not affect any "pre-loaded" packages Emacs has already "set up" before here; if that's ever a problem look at it then since that gets complicated and might involve custom temacs stuff.
(require 'exec-path-from-shell)
;; (setq exec-path-from-shell-debug t) ; When debugging uncomment.
(setq exec-path-from-shell-arguments '("-l"))
;; Only take the following environment variables into Emacs.
(dolist (var '("SSH_AUTH_SOCK" "LANG" "LC_CTYPE" "GNUPGHOME" "XDG_DATA_HOME" "XDG_CONFIG_HOME" "XDG_VIDEOS_DIR" "XDG_PICTURES_DIR" "XDG_DOWNLOAD_DIR" "XDG_MUSIC_DIR" "XDG_CACHE_HOME" "XDG_DESKTOP_DIR" "XDG_DOCUMENTS_DIR"))
  (add-to-list 'exec-path-from-shell-variables var))
(exec-path-from-shell-initialize)

;;;; Built-ins

;;;;; General baseline

;; Baseline configuration, anything more specific goes into it's own area even if it also uses
;; the emacs package because that way adding :disabled can be used to easily selectively
;; disable areas of configuration.

;; XXX: It looks like `startup--load-user-init-file' loads this init file within the scope of it's let-binding for `inhibit-null-byte-detection' therefore any change we make to inhibit-null-byte-detection will be undone (once the let-binding scope ends) and inhibit-null-byte-detection will revert back to it's default value of nil. This function being added to `emacs-startup-hook' will then execute outside of the let-binding scope from `startup--load-user-init-file' and result in configuration values persisting.
;; XXX: You can verify this by adding `(debug-on-variable-change 'inhibit-null-byte-detection)' to this init file just above (making sure to comment out where this function is added to the emacs-startup-hook below) and noting that when startup--load-user-init-file triggers the debugger (you can C-h v the value) the value is t, once the trigger for the end of the let binding scope is stepped over the value is nil (the default).
(defun tjp/ignore-null-byte ()
  (setq-default inhibit-null-byte-detection t))

(use-package emacs
  :ensure nil
  :demand t
  :hook ((prog-mode . display-fill-column-indicator-mode)
         ((prog-mode text-mode) . display-line-numbers-mode)
         ((prog-mode text-mode) . visual-line-mode))
  :init
  (add-hook 'emacs-startup-hook #'tjp/ignore-null-byte)
  :custom
  ;; No second case-insensitive pass over `auto-mode-alist'.
  (auto-mode-case-fold nil)

  ;; Remove case sensitivity from general completions.
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (case-fold-search t)
  (read-file-name-completion-ignore-case t)

  ;; TODO: Custom faces for battery status and time so its a little clearer to read. Also proper spacing instead of adding literal whitespace?
  (display-time-default-load-average 1)          ; 5-minute load average
  (display-time-format "%m-%d  %a  %I:%M %p   ") ; see `format-time-string'
  (display-time-interval 15)                     ; update time every N seconds
  (display-time-day-and-date t)
  ;; `battery-update-functions' for more complex information.
  (battery-mode-line-format "%b%p% %t    ")

  :config
  (setq-default
   ;; Subprocesses -------------------------------------------------------------
   read-process-output-max (* 8 1024 1024) ; increase max-size we can read at a time
   process-adaptive-read-buffering nil	   ; process output ASAP instead of waiting for more

   ;; Fill column --------------------------------------------------------------
   fill-column 95
   display-fill-column-indicator-character ?\u250A ; 2506 or 250A

   ;; Fringes, margins, and associates -----------------------------------------
   fringes-outside-margins nil
   left-margin-width 1
   display-line-numbers-type t 		   	; absolute line numbers
   display-line-numbers-grow-only t
   display-line-numbers-width 3      	; default width of line numbers
   indicate-buffer-boundaries 'left     ; show buffer top/bottom in margin
   ;; indicate-buffer-boundaries t			; show buffer end in margin

   ;; Host system integration --------------------------------------------------
   select-enable-clipboard t			; unify emacs and system clipboard
   use-dialog-box nil					; prompt in minibuffer instead of system dialog
   use-file-dialog nil					; ditto for file dialogs

   ;; Indentation and completion -----------------------------------------------
   indent-tabs-mode nil					; sane whitespace, please
   ;; TODO: Why was -1 not setting for indent-tabs-mode?
   tab-width 4							; smoller than 8
   tab-always-indent 'complete			; indentation & completion with TAB
   ;; TODO: See tab-first-completion (from doc string of tab-always-indent)
   completion-cycle-threshold 3			; TAB wraps if completion list small

   ;; General buffer, and file handling ----------------------------------------
   delete-by-moving-to-trash t
   sentence-end-double-space nil		; single space after fullstop
   require-final-newline t				; add newline on buffer save
   ;; display-line-numbers-type 'visual     ; set line numbers to relative
   display-raw-bytes-as-hex t
   create-lockfiles nil					; here there be dragons
   remote-file-name-inhibit-locks t     ; here there be remote dragons
   remote-file-name-access-timeout 10   ; seconds
   help-window-keep-selected t			; re-use current help buffer if viewing more help

   ;; Fontification ------------------------------------------------------------
   fast-but-imprecise-scrolling t		   ; redraw immediately when scrolling vertically
   redisplay-skip-fontification-on-input t ; user input always interrupts fontification
   treesit-font-lock-level 4			   ; max fontification

   ;; Parentheses and pairs ----------------------------------------------------
   delete-pair-blink-delay 0
   show-paren-delay 0					; show matching parenthesis quickly
   ;; blink-matching-paren nil

   ;; Scratch buffer defaults --------------------------------------------------
   initial-scratch-message ";; Scratch" ; initial scratch message
   initial-major-mode 'fundamental-mode ; a "nothing" mode for scratch buf

   ;; Bidirectionality ---------------------------------------------------------
   bidi-display-reordering 'left-to-right  ; disable bi-directional text rendering by default
   bidi-paragraph-direction 'left-to-right ; also required
   bidi-inhibit-bpa t					   ; no bidirectional paren searching

   ;; Misc ---------------------------------------------------------------------
   blink-cursor-mode nil				 ; do not blink cursor
   use-short-answers t                   ; yes/no -> y/n
   save-interprogram-paste-before-kill t ; do not overwrite existing clipboard text on kill
   kill-do-not-save-duplicates t
   load-prefer-newer t 					; load newer bytecode over old
   echo-keystrokes 0.1 					; immediately echo unfinished commands (feedback)
   ring-bell-function #'ignore			; no beeping

   vc-handled-backends '(Git)
   ;; TODO: Maybe `vc-git-log-edit-summary-max-len` and `vc-git-log-edit-summary-target-len`.

   ;; hide commands in M-x not applicable to current mode
   read-extended-command-predicate #'command-completion-default-include-p
   ) ; setq-default closed here.

  ;; Native compilation --------------------------------------------------------
  (if (and (featurep 'native-compile)
		   (fboundp 'native-comp-available-p)
		   (native-comp-available-p))
	  ;; Make sure native compilation is on, and immediately native-compile packages upon their
	  ;; installation instead of when they are first used.
	  (setq native-comp-jit-compilation t
			package-native-compile t
			;; Silent native compilation please.
			native-comp-async-report-warnings-errors 'silent))

  ;; Coding system -------------------------------------------------------------
  (setq-default  buffer-file-coding-system 'utf-8 ; utf8
    			 locale-coding-system 'utf-8)	  ; utf8
                 ;; inhibit-null-byte-detection t)   ; XXX: See function tjp/ignore-null-byte
  (set-default-coding-systems 'utf-8)			  ; utf8
  (prefer-coding-system 'utf-8)					  ; utf8
  (set-coding-system-priority 'utf-8)             ; utf8
  (set-language-environment "UTF-8")              ; utf8

  ;; Show matching parentheses and pairs, auto insert matching pairs
  (show-paren-mode)
  (electric-pair-mode)

  ;; Highlight current line in buffer
  (global-hl-line-mode)

  ;; Force enable font-face
  (global-font-lock-mode 1)

  ;; Display column number in minibuffer
  (column-number-mode)

  ;; Visual wrap with context-aware prefix everywhere
  (global-visual-wrap-prefix-mode)

  ;; Set default appearance of fringes on frames (left/right pixel widths)
  (fringe-mode '(5 . 6))

  ;; History
  (savehist-mode) 						; minibuffer command history
  (save-place-mode)						; location of point in visited files
  (recentf-mode)						; list of recently opened files

  ;; TODO: If on a laptop (currently only do this for macos variant).
  (display-battery-mode t)
  (display-time-mode)

  ;; Be normal and delete selected text when inserting further characters
  (delete-selection-mode)

  ;; Disable "fancy" (frame-based) tooltips
  (tooltip-mode -1)

  ;; (global-visual-line-mode)                ; visual-line-mode everywhere (TODO: This and below only
  ;; in prog and text?)
  ;; (global-display-line-numbers-mode)       ; enable display of line numbers at margin
  ;; (winner-mode)                         ; window layout tracking (incase we need to undo)
  ;; TODO: Reenable later after bug report #75730 is resolved.
  )


;;;;; Whitespace

;; Remove empty lines at start/end of file, trailing whitespace on lines, and ensure a newline at end of file.
;; This happens BEFORE the file is saved; so use with external formatters won't bork things.

(use-package emacs
  :ensure nil
  :hook ((prog-mode text-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face empty trailing missing-newline-at-eof))
  (whitespace-action '(cleanup auto-cleanup))
  :config
  ;; newline or newline-mark
  (setq whitespace-display-mappings '((newline-mark ?\n [?⏎ ?\n] [?$ ?\n]))))

;;;;; Font

;; TODO: Can this _sanely_ go into a use-package? Or maybe I don't bother as I end up using Fontaine.
;; TODO: If on macOS and gui AND on a display below retina ppi use a font weight of medium (at time of writing) if the defaults value AppleFontSmoothing is 0, otherwise use (whatever it needs to be) if it's 1. Then the same logic for if using a display above retina ppi (i.e. the laptops display). Because font weights shift around and it's all fucky.
;; defaults write org.gnu.emacs applefontsmoothing -int 0

(when tsujp/is-gui
  (set-face-attribute 'default nil
					  :family "tsujp"
					  :weight 'medium
					  ;; :height 140 ; On that 1080p samsung display.
					  :height 160))     ; 150 or 160 on mac display.

;;;;; Mouse

(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  (setq-default
   scroll-conservatively 101    ; scroll just enough to bring point back into view
   scroll-margin 1              ; padding at top/bottom of window which counts as scroll region
   scroll-step 1                ; keyboard scroll one line at a time
   mouse-wheel-follow-mouse t   ; mouse wheel scrolls window mouse is hovering over
   mouse-wheel-progressive-speed nil    ; don't accelerate scrolling
   ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
   ;; jit-lock-defer-time 0
   )

  (when tsujp/is-gui
    (context-menu-mode))                ; right-click to show context menu

  (when (and tsujp/is-gui tsujp/is-mac)
	(setq-default
	 ns-use-mwheel-momentum nil			; no momentum scrolling (extra required on macOS)
	 ns-mwheel-line-height 10)))		; higher = more sensitive, lower = less

;;;;; Auto revert

;; When the file backing a buffer changes (perhaps edited by an external program e.g. git pull)
;; auto-revert-mode can automatically refresh the buffers contents to match. This refresh is
;; called a revert.

(use-package autorevert
  :ensure nil
  ;; JORDAN TODO: It seems enabling global-auto-revert-mode has a race condition with tramp.
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-check-vc-info t)
  (auto-revert-remote-files t)
  (auto-revert-avoid-polling t))
  ;; :config
  ;; (setq-default
  ;;  ;; TODO: Maybe want to set `global-auto-revert-non-file-buffers' to `t' but maybe not. Could lead to a lot of revert spamming, look into that later.
  ;;  ;; auto-revert-interval 1
  ;;  ;; auto-revert updates version control (vc) info already, however only if there are vc changes to the backing file directly (e.g. editing the file's contents externally); if a vc update occurs without editing the backing files contents directly auto-revert may miss this information. For example if you fetch and pull new commits which change other files that a buffer doesn't care about. Here vc info has changed (head commit) and unless auto-revert-check-vc-info is non-nil said vc info changes will be ignored.
  ;;  auto-revert-check-vc-info t
   ;; ))

;;;;; Unique buffer names

;; If two buffers have the same name, differentiate them by setting their respective names
;; equal to whatever is left after the common-prefix between them is removed.

(use-package uniquify
  :ensure nil
  :defer t
  :custom
  (uniquify-strip-common-suffix t)
  (uniquify-buffer-name-style 'forward))

;;;;; Spelling

(use-package ispell
  :ensure nil
  :defer t
  :custom
  (ispell-dictionary "en_GB"))

;;;;; Search

(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq isearch-lazy-count t)          ; show match counts (current and total) in search prompt
  ;; TODO: `lazy-count-prefix-format'.
  )

;;;;; Autosaves and backups

;; Autosaved files are surrounded by pounds (#) by default, whereas backup files are suffixed with tilde (~).

(use-package emacs
  :ensure nil
  ;; :after xdg
  :after (:all exec-path-from-shell xdg)
  :custom
  (auto-save-interval 300)         ; keystroke-count between autosaves
  (auto-save-timeout 60)           ; seconds of idle time between autosaves
  (backup-by-copying t)            ; don't clobber symlinks
  (make-backup-files t)            ; backup file the first time it is saved
  (version-control t)              ; use version numbers on backups
  (delete-old-versions t)          ; silently (without confirmation prompt) delete old versions
  (kept-new-versions 10)           ; count of newest versions to keep
  (kept-old-versions 0) ; count of oldest versions to keep (keeping old versions clobbers new versions), see bottom of: https://www.emacswiki.org/emacs/ForceBackups
  (backup-directory-alist
   `(("." . ,(concat (xdg-cache-home) "/emacs/backups"))))
  ;; TODO: Auto saves still polluting, read docs for auto-save-file-name-transforms and test this config out to get this stuff to stop polluting. Perhaps use the hash thing too for long dirs and what not, in which ase a nice mapping file additionally?
  (auto-save-file-name-transforms `((".*" ,(concat (xdg-cache-home) "/emacs/autosaves") t))))

;;;;; Minibuffer

(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq minibuffer-visible-completions t ; nil is default, I used to use t
		completion-styles '(orderless basic)
		;; Ensures above completion-styles are always respected by other packages.
		completion-category-defaults nil
		;; TODO: Since Vertico update is `basic' here of any value? It's not needed for the workaround anymore so if it isn't then nuke it.
		completion-category-overrides '((file (styles . (basic partial-completion orderless))))))

;;;;; Which-key

;; Display keybindings for the currently (incomplete) command.

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))

;;;;; Operating system

;; TODO: Worth it to check in Emacs init that keys are laid out as I expect them to?

(use-package emacs
  :ensure nil
  :config
  ;; macOS...
  (when tsujp/is-mac
    ;; Keyboard modifier layout
    (setq mac-command-modifier 'meta
		  mac-option-modifier 'super
		  mac-right-command-modifier 'hyper)

    ;; macOS and GUI...
    (when tsujp/is-gui
      ;; Re-enable menu bar since that's outside of the application window (Emacs frame).
      (menu-bar-mode 1)
      ;; Also set Emacs to be fullscreen by default. ns-use-native-fullscreen nil already set
      ;; in early-init.el
      ;; TODO: Double check that is what this does
      (set-frame-parameter nil 'fullscreen 'fullboth))))

;;;; Package management

;; Elpaca package manager configuration as well as package repositories.

;;;;; Elpaca

;;;;;; Bootstrap

;; The following code was copied from Elpaca's installation instructions README.md on
;; 2025/02/07 commit 141b2f59406e94c61478dca7779efd800d59258f

(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;;;;;; Configure

;; Enable use-package support
(elpaca elpaca-use-package              ; install package `elpaca-use-package`
  (elpaca-use-package-mode))            ; enable support for use-package's `:ensure`

;; Wait for Elpaca to bootstrap (if appropriate)
(elpaca-wait)

;;;; Theme

;;;;; Modus

;; Using the non-bundled modus-theme so we can get new updates outside of Emacs release cycle (if we were to use the bundled version).
(use-package modus-themes
  :ensure
  :custom
  (modus-themes-bold-constructs t)      ; enable use of bold
  (modus-themes-italic-constructs t)    ; enable use of italic
  (modus-themes-prompts '(bold))        ; style for minibuffer and repl prompts
  (modus-themes-common-palette-overrides
   '((border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
	 (fringe unspecified)
     (bg-tab-bar bg-main)
     (bg-tab-current bg-main)
     (bg-tab-other bg-dim)
     ;; TODO: Make the matched paren bold? Integrate my old paren faces thing.
	 (underline-paren-match fg-main)))
  :custom-face
  ;; Do not extend `region' background past end of line.
  (region ((t :extend nil)))
  :config
  (load-theme 'modus-vivendi :no-confirm)
  (tsujp/modus-fill-column-face-style)
  (tsujp/org-test-block-face))

;; TODO: Rename this function appropriately and move other face customisations from lingering old stuff into this one.
;; TODO: Rework into hook within use-package for modus above.
;; XXX: Doesn't seem to work when used within custom-face of use-package.. I for whatever reason.
(defun tsujp/modus-fill-column-face-style ()
  (modus-themes-with-colors
    (custom-set-faces
     `(fill-column-indicator ((,c (:height 1.0 :foreground ,bg-dim :background unspecified))))
     ;; XXX: If font in-use looks terrible try oblique as (by custom) italic is specifically designed to be skewed whereas oblique is simply the normal font automatically (by tooling) angled to some extent.
     ;; `(tab-bar-tab ((,c (:foreground ,yellow-intense :weight black :slant italic)))))))
     `(tab-bar-tab ((,c (:foreground ,fg-main :weight bold :slant italic)))))))

;;;;; Custom faces

(defun tsujp/org-test-block-face ()
  (modus-themes-with-colors
    (defface org-test-block-face
	  `((t :background ,bg-prose-block-contents :extend t))
	  "Face for test block in org mode.")))

;;;;; Modeline scrollbar

;; TODO: This package useful or needed in modern emacs?: https://github.com/mrkkrp/cyphejor
;; TODO: Colours.
(use-package mlscroll
  :ensure
  :hook (elpaca-after-init . mlscroll-mode))

;;;; Meow

;; Editing interface (e.g. modal editing) configuration.

;; TODO: Cursor styles based on mode, here or configured elsewhere?
(defun tsujp/meow-cursor ()
  (setq meow-cursor-type-insert '(bar . 2))
  (setq meow-cursor-type-normal 'box)
  (custom-set-faces
   '(meow-insert-cursor ((t (:background "#FFFFFF"))))
   '(meow-normal-cursor ((t (:background "#FFFF00"))))))

(defun meow-smart-reverse ()
  "Reverse selection or begin negative argument."
  (interactive)
  (if (use-region-p)
	  (meow-reverse)
    (negative-argument nil)))

(defun meow-word ()
  "Expand word/symbol under cursor."
  (interactive)
  (if (and (use-region-p)
		   (equal (car (region-bounds))
				  (bounds-of-thing-at-point 'word)))
	  (meow-mark-symbol 1)
    (progn
	  (when (and (mark)
				 (equal (car (region-bounds))
						(bounds-of-thing-at-point 'symbol)))
		(meow-pop-selection))
	  (meow-mark-word 1))))

;; TOOD: Put this where appropriate (and probably into fontaine)
										;(set-face-attribute 'meow-position-highlight-number-1 nil
										;                    :foreground "#FFFF00"
										;                    :background nil
										;                    :family "Iosevka SS03"
										;                    :weight 'heavy)

;; Source: https://github.com/meow-edit/meow/issues/590
;; Meow digit keys in normal mode act as universal argument without needing C-u prefix.
;; e.g. for doing 10j to move down 10 lines instead of C-u 10 j
;; TODO: Has a bug where if you do 10j and then type 5 to go another 5 lines it does 105; i.e. it's not clearing prior universal argument settings after invoking a movement. Could fix but probably better to use scroll commands or avy or goto-line instead rather than this now that I think about it.
;; (defun tsujp/meow-smart-digit (digit)
;;   (interactive)
;;   (if (not (and meow--expand-nav-function
;;                 (region-active-p)
;;                 (meow--selection-type)))
;;       (progn
;;         (universal-argument)
;;         (meow-digit-argument))
;;     (meow-expand digit)))

;; (defmacro tsujp/meow-hoc (fn-name meow-cmds-list)
;;   "Creates an interactive function which interactively-calls each Meow command in the list."
;;   `(defun ,fn-name ()
;;      (interactive)
;;      (mapc #'call-interactively ,meow-cmds-list)))

;; (tsujp/meow-hoc tsujp/cancel-meow-secondary-selection '(meow--cancel-second-selection))

;; Based on: https://github.com/meow-edit/meow/issues/506
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

    ;; '("'" . meow-reverse)
    '("'" . meow-smart-reverse)

	'("H-q" . meow-grab)

    ;; '("J" . meow-end-of-thing)
    ;; '("K" . meow-beginning-of-thing)
    ;; '("H" . meow-bounds-of-thing)
    ;; '("L" . meow-inner-of-thing)
    ;; '("K" . meow-prev-expand)
    ;; '("L" . meow-right-expand)
    ;; '("J" . meow-next-expand)
    ;; '("H" . meow-left-expand)

    ;; Movement
    '("k" . meow-prev)
    '("l" . meow-right)
    '("j" . meow-next)
    '("h" . meow-left)

    '("w" . avy-goto-char-timer)

    '("/" . meow-visit)

    ;; Expansion
    '("x" . meow-word)
    '("n" . meow-cancel-selection)
    '("N" . meow-pop-selection)
    '("s" . meow-visual-line)
    ;; '("s" . meow-line-expand)

										;'("w" . meow-mark-word)

    '("i" . meow-back-word)
    '("I" . meow-back-symbol)
    '("o" . meow-next-word)
    '("O" . meow-next-symbol)

    ;; TODO: Command `meow-till-expand` is quite nice; work it in somewhere?
    ;; TODO: c for copy and x for paste?
    ;; TODO: meow-delete by char instead of until the end of line by default.

    ;; Editing
    '("d" . meow-kill)
    '("c" . meow-change)

    '("r" . meow-append)
    '("R" . meow-insert)
    '("e" . meow-open-below)
    '("E" . meow-open-above)

    ;; '("e" . meow-insert)
    ;; '("E" . meow-open-above)
    ;; '("r" . meow-append)
    ;; '("R" . meow-open-below)

    '("u" . undo-only)
    '("U" . undo-redo)

    '("v" . meow-right-expand)

    ;; '("v" . meow-end-of-thing)
    ;; '("V" . meow-beginning-of-thing)

    ;; Prefix ;
    ;; '(";c" . meow-comment)
    ;; '(";w" . save-buffer)
    '(";c" . (lambda () (interactive) (meow--cancel-second-selection)))

    ;; Prefix g
    '("gd" . xref-find-definitions)
    '("gj" . meow-end-of-thing)
    '("gk" . meow-beginning-of-thing)
    '("go" . meow-bounds-of-thing)
    '("gi" . meow-inner-of-thing)

    ;; Ignore escape
    '("<escape>" . ignore)))

(defun meow-setup ()
  (progn
    (tsujp/meow-ergo-keys)))

;; TODO: To delete secondary selection overlay: (delete-overlay secondary-mouse-overlay)
;; (add-hook 'meow-insert-exit-hook 'corfu-quit)
(use-package meow
  :ensure
  ;; :custom
  ;; TODO: How to disable these, or perhaps only show them while a key combo is being pressed (i.e. not toggle, but while holding).
  ;; (meow-expand-hint-counts '((word . 3) (line . 3) (block . 3) (find . 3) (till . 3)))
  :config
  (meow-setup)
  (setq meow-use-enhanced-selection-effect t)
  ;; TODO: PR/Change to Meow so it's possible to enter "selection mode" like in Helix where we can extend by incremental amounts (e.g. a word/line/symbol) without breaking the old selecting as it currently would; as per Meow's design. I imagine probably not but there are current commands which are _kind of_ like that `meow-expand-line` which always expands a line but it's still limited to the line selection type IIRC. So perhaps this boils down to adding a new selection type of 'universal' alongside the existing ones: line, char, word, etc.
  (setq meow-expand-selection-type 'expand)
  (meow-leader-define-key
   '("e" . "C-x b"))
  (add-hook 'meow-insert-exit-hook #'corfu-quit)
  (meow-global-mode 1)
  ;; (add-to-list 'meow-expand-exclude-mode-list 'emacs-lisp-mode)
  ;; Cursor customisation must be done afterwards.
  (tsujp/meow-cursor))

;; TODO: Limit computed repeat of the _functionality_ as well since expand-hint-counts only affects the visual indicator. I.e. I had set it to visually show up to 3, but typing 5 still works.
;; Disable meow expand _hints_ (functionality still works) by overriding the function to nothing since there doesn't look like a nicer way to do that currently. TODO: PR for such functionality?
;; (advice-add #'meow--maybe-highlight-num-positions :override (lambda ()))

;;;; Avy

(use-package avy
  :ensure
  :bind (("H-e" . #'avy-goto-char-timer)
         ("H-`" . #'toggle-frame-fullscreen))
  :custom
  (avy-timeout-seconds 0.3))
;; :custom-face
;; (avy-goto-char-timer-face ((t (:foreground "#FFFF00"))))
;; (avy-lead-face ((t (:foreground "#FFFF00" :background nil :weight black))))
;; (avy-lead-face-0 ((t (:foreground "#f78fe7" :background "#555" :weight black)))))

;; (keymap-global-set "H-e" #'avy-goto-char-timer)
;; (keymap-global-set "H-`" #'toggle-frame-fullscreen)

;;;; Completion

;; The Emacs completion system uses a completion frontend which provides a completion UI for the user, said frontend calls a completion backend which provides completions based on a configurable completion style.

;;;;; Orderless

;; Provides a completion style with configurable components to match literally, by regexp, by word prefixes and more. Most notably to complete things out of order and in shorthand.

(use-package orderless
  :ensure
  :after minibuffer
  :bind
  ;; SPC should never complete, now it activates orderless.
  ;; ? is orderless' default dispatch for regexp (TODO: double check this)
  (:map minibuffer-local-completion-map
  	    ("SPC" . nil)
  	    ("?" . nil))
  :custom
  ;; Default is `'(orderless-literal orderless-regexp)`. List of matching styles: https://github.com/oantolin/orderless?tab=readme-ov-file#component-matching-styles
  ;; `orderless-prefixes` is more useful out of the box as we can use style dispatchers (https://github.com/oantolin/orderless?tab=readme-ov-file#style-dispatchers) to enforce a particular style as needed (rarely).

  ;; Flex completion styles add so much fucking noise I can barely even read the completion candidate list my god. I can see it being useful in more nicer cases but on-by-default (as I had previously set) is... not a good idea I now see.
  ;; (orderless-matching-styles '(orderless-prefixes orderless-flex orderless-regexp)))
  (orderless-matching-styles '(orderless-prefixes orderless-regexp)))

;;;;; Vertico

;; Vertical completion framework which uses Emacs' in-built completion engine; meaning it can be used generically instead of inventing it's own custom API (unlike say Helm or Ivy).
;; Specifically Vertico is for minibuffer completions and serves as a completion frontend.

;; TODO: Any extra vertico config?
;; TODO: Config from karthinks video on vertico mode, interesting stuff.
(use-package vertico
  :ensure
  :hook (elpaca-after-init . vertico-mode)
  :bind
  ;; Generally C-DEL runs `backward-kill-word`. Directories may have names not typically associated with words, e.g. `.config` in which case it only deletes back to `.`; `vertico-directory-delete-word` would delete everything while falling back to `backward-word` (not `backward-kill-word`) for similar functionality.
  ;; Specifically enabled on Vertico's keymap, i.e. when in minibuffer completions.
  ;; ~vertico-directory-delete-word~ functions as a better 'go back current directory in completion'
  ;; The equivalent of this map binding using keymap-set would be:
  ;; (keymap-set vertico-map "C-<backspace>" #'vertico-directory-delete-word)
  ;; Although one would have to wait for vertico to be loaded, here use-package does that for us.
  (:map vertico-map
		("C-<backspace>" . #'vertico-directory-delete-word))
  ;; TODO: Here and for other packages `:hook (after-init . vertico-mode)` isn't executing? The fuck? Why?
  ;; Because of that have to move to `:config` form.
  :custom
  (vertico-count 6)) ; maximum number of candidates to show

;;;;; Corfu

;; Displays completion candidates for current point in a popup either below or above said point using Emacs' in-built completion facilities (so ties in with Vertico, and could be used without Vertico if desired).
;; Specifically Corfu is for buffer completions (e.g. identifiers when programming) and serves as a completion frontend.

;; TODO: Corfu go back to previous help buffer window if it moved it during completion.

;; TODO: Extra corfu config?
(use-package corfu
  :ensure
  :hook (elpaca-after-init . global-corfu-mode)
  :custom
  ;; TODO: Maybe not automatic? See corfu-auto-prefix or something like that.
  ;; (corfu-auto 1)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; show documentation after `corfu-popupinfo-delay`
  :bind
  (:map corfu-map
		;; Stop corfu stealing the RET key when completing.
		("RET" . nil)))

;;;;; Marginalia

;; Display annotations alongside minibuffer completion candidates.

(use-package marginalia
  :ensure
  :hook (elpaca-after-init . marginalia-mode))

;;;;; Magit

(use-package magit
  :defer 0.5
  :ensure
  :custom
  (magit-no-message '("Turning on magit-auto-revert-mode"))
  :config
  ;; prepare the arguments
  (setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles.git")))
  (setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

  ;; function to start magit on dotfiles
  (defun dotfiles-magit-status ()
	(interactive)
	(add-to-list 'magit-git-global-arguments dotfiles-git-dir)
	(add-to-list 'magit-git-global-arguments dotfiles-work-tree)
	(call-interactively 'magit-status))
										;(global-set-key (kbd "F5 d") 'dotfiles-magit-status)

  ;; wrapper to remove additional args before starting magit
  (defun magit-status-with-removed-dotfiles-args ()
	(interactive)
	(setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
	(setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
	(call-interactively 'magit-status))
  ;; redirect global magit hotkey to our wrapper
  (global-set-key (kbd "C-x g") 'magit-status-with-removed-dotfiles-args))

;; TODO: Newest commits at top when doing an interactive rebase like at the CLI.

;; TODO: Create a new project as a git repository and some templates (like e.g. adding a package.json if its a javascript project, creating a README.md and so on).
;; -- create git repo with `magit-init`

;; TODO: Configure remote?

;; TODO: Configure `magit-repository-directories` to include my home repo and how to tell it that the git directory is at another location?

;;;; Tree-sitter

;; Treesitter grammar sources and general configuration.

;;;;; Grammar sources

;; TODO: Later have a way to specify a specific commit hash instead of just HEAD of a named branch or a tag, but also a way to compile the grammars if they do not exist WITHOUT constantly recompiling them when starting Emacs as-is currently the case with the commented out (dolist) where it is. If that (dolist) is in :config instead it doesn't even run unless the specified language isn't by-default distributed with a ts grammar in Emacs (e.g. zig) meaning if you specify a newer grammar for (e.g. typescript) it wont be automatically compiled unless you interactively call treesit-install-language-grammar. The problem there is the `(unless (treesit-ready-p (car source))` which wraps said expression. Basically auto-installing treesitter grammars with Emacs right now kinda sucks when done at a granular level.

(use-package treesit
  :ensure nil
  :after emacs
  :preface
  ;; Define source of language grammars.
  (setq treesit-language-source-alist
		'((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
		  (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master"))
		  (json . ("https://github.com/tree-sitter/tree-sitter-json"))
		  (zig . ("https://github.com/maxxnino/tree-sitter-zig"))))
  :init
  ;; Compile language grammars; we cannot do this in `:config` because treesit may have loaded
  ;; in-built grammars by then and reloading them is currently (as of Emacs 30) not possible
  ;; without restarting Emacs.
  ;; Compile and install all of them.
  ;; See TODO at start of this top-level sexp.
  ;; (dolist (source treesit-language-source-alist)
  ;;   (treesit-install-language-grammar (car source)))
  :config
  ;; todo: This automatically and unified with grammar definition source above.
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode)))

;;;; Terminal

;; Eat seems to be fast enough when paired with certain optimisations like setting `process-adaptive-read-buffering` to nil as mentioned here: https://www.reddit.com/r/emacs/comments/17nl7cw/shout_out_to_the_eat_terminal_emulator_package/#k7tmgz0
;; If eat is ever legitimately too slow, perhaps consider using vterm for eshell visual commands via: https://github.com/iostapyshyn/eshell-vterm/blob/master/eshell-vterm.el
;; However because eat by itself offers great Emacs integration and an eshell mode I'll use eat for now.
;; TODO: Instead of libvterm use ghosttylib with emacs as an xwidget?

;; global-hl-line-mode and hl-line-mode do not interact (the single exception being the latter if invoked interactively). So, to disable hl-line-mode in a buffer when global-hl-line-mode was earlier called the self-same local variable must be set to nil.
(defun disable-local-global-hl-line ()
  (setq-local global-hl-line-mode nil))

;; TODO: `eat-kill-process' redefine and use signal instead of delete-process
(use-package eat
  :ensure
  :hook (elpaca-after-init . eat-eshell-mode)
  :init
  (add-hook 'eat-mode-hook #'disable-local-global-hl-line)
  (add-hook 'eat-exec-hook #'tjp/eat--integrate)
  :config
  ;; Clear commands eshell considers visual by default.
  (setq eshell-visual-commands '())
  (setq eat-minimum-latency 0.002)
  ;; Shell/Emacs integration niceties.
  (setq eat-enable-directory-tracking t)
  (setq eat-enable-shell-prompt-annotation t)
  ;; Our own custom automatic eat setup when creating shells.
  (defconst tjp/eat--terminfo-dir "eat-terminfo") ; name to use for terminfo dir on remote
  (defconst tjp/eat--terminfo-dir-c "e") ; man 5 term => @TERMINFO@/c/name
  (defconst tjp/eat--script-name "eat-bash.sh") ; name to use for integration script on remote
  (defconst tjp/eat--source-script
	(concat (file-name-as-directory eat-term-shell-integration-directory) "bash"))


  (cl-defun tjp/eat--get-digests (script-filename terminfo-dir &optional (shasum-cmd "shasum"))
	"Given SCRIPT-FILENAME and TERMINFO-DIR compute the SHA1 hash of the script, and
directory contents of terminfo and return these as a list of two elements. Optional
SHASUM-CMD can specify local shasum-like command for hash computation.

TERMINFO-DIR should include the single-character prefix as described in term(5)."
	;; XXX: Is setq the right thing to use here? It feels cleaner wrt conditionally applying a change to the given function parameters. Also the appending with tjp/eat--terminfo-dir-c.
	(setq terminfo-dir (concat (file-name-as-directory terminfo-dir) tjp/eat--terminfo-dir-c))
	(when (file-remote-p default-directory)
	  (setq script-filename (tramp-file-local-name script-filename))
	  (setq terminfo-dir (tramp-file-local-name terminfo-dir)))
	(let ((the-cmd
		   (concat
			;; Eat shell integration script.
			(format "{ %s %s 2>/dev/null || printf 'NO_HASH '; } | cut -d' ' -f1 | tr '\n' ' '"
					shasum-cmd
					(shell-quote-argument script-filename))
			";"
			;; Eat compiled terminfo contents.
			(format "__digests=\"$(find %s -type f -print0 2> /dev/null | sort -z | xargs -0 -r %2$s | cut -d' ' -f1)\"; if [ -z \"$__digests\" ]; then printf 'NO_HASH'; else %2$s <<< \"$__digests\" | cut -d' ' -f1 | tr -d '\n'; fi"
					(shell-quote-argument terminfo-dir)
					shasum-cmd))))
	  ;; (message "[tjp/eat] DEBUG getting digests with: %s" the-cmd) ; poor man's debug
	  (split-string (shell-command-to-string the-cmd) " ")))


  (defun tjp/eat--integrate (eat-proc)
	(let ((shell-setup-cmd
		   (when-let* (((file-remote-p default-directory))
					   (remote-temp-dir (file-name-as-directory (tramp-handle-temporary-file-directory)))
					   ;; Remote eat integration locations.
					   (remote-eat-script (concat remote-temp-dir tjp/eat--script-name))
					   (remote-eat-terminfo (concat remote-temp-dir (file-name-as-directory tjp/eat--terminfo-dir)))
					   ;; Get remote digests.
					   (digests (tjp/eat--get-digests remote-eat-script remote-eat-terminfo "sha1sum")))
			 ;; (message "[tjp/eat] DEBUG: remote digests %s" digests) ; poor man's debug
			 ;; (message "eat terminal %s" eat--t-term)

			 ;; Check integration script digests match.
			 (unless (string-equal (nth 0 digests) (nth 0 tjp/eat--master-digests))
			   (message "[tjp]: eat integration script digest mismatch want (%s) got (%s)" (nth 0 tjp/eat--master-digests) (nth 0 digests))
			   ;; Copy integration script to remote.
			   (copy-file tjp/eat--source-script remote-eat-script t))

			 ;; Check terminfo script digests match.
			 (unless (string-equal (nth 1 digests) (nth 1 tjp/eat--master-digests))
			   (message "[tjp]: eat terminfo digest mismatch want (%s) got (%s)" (nth 1 tjp/eat--master-digests) (nth 1 digests))
			   ;; copy-directory doesn't have an overwrite flag so we will delete the remote directory (without following symlinks) before copying to prevent errors in this function.
			   ;; TODO / BUG: Tramp cannot delete the remote directory, im guessing something to do with the shell its setting up to run the command perhaps? Investigate later.
			   (delete-directory remote-eat-terminfo t t)
			   ;; Never create DIRECTORY (see `copy-directory') as a symlink on the remote.
			   (let ((copy-directory-create-symlink nil))
				 (copy-directory
				  (concat (file-name-as-directory eat-term-terminfo-directory) tjp/eat--terminfo-dir-c)
				  (concat (file-name-as-directory remote-eat-terminfo) tjp/eat--terminfo-dir-c))))

			 ;; List of extra shell commands specific to a remote host.
			 (list
			  (format "export TERMINFO=%s" (tramp-file-local-name remote-eat-terminfo))
			  (format "source %s" (tramp-file-local-name remote-eat-script))))))

	  ;; XXX: Calling eat-reset or variants like eat--t-reset doesn't work since it looks like we send these strings so fast eat hasn't "set up" yet (for lack of a better word) but these strings (commands) are actually being enacted. Weird to explain basically: don't try "optimise" this.

	  (message "[tjp/eat] DEBUG: shell command remote base: %s" shell-setup-cmd) ; poor man's debug

	  ;; TODO: Perhaps this final command construction logic could be better, or maybe this is idiomatic elisp idk. Already spent WAYYYYYYYY too much time doing this and it works correctly as-is.

	  ;; No remote setup cmd, add sourcing of local eat integration script.
	  (unless shell-setup-cmd
		(push (format "source %s" tjp/eat--source-script) shell-setup-cmd))

	  (push "unset EAT_SHELL_INTEGRATION_DIR" shell-setup-cmd)
	  (push "clear\n" shell-setup-cmd)

	  (setq shell-setup-cmd (mapconcat #'identity (nreverse shell-setup-cmd) " && "))

	  (message "[tjp/eat] DEBUG: shell command final: %s" shell-setup-cmd) ; poor man's debug

	  ;; XXX: Functions `tramp-send-command-and-read', `tramp-send-command-and-check', or the various internal eat functions that send a string directly to the eat process don't work here; only `eat--send-string'. Also `eat--send-string' hardcoded but should be replaced by getting the input method of the current `eat-terminal' if this is ever NOT the input method. If you log `eat-proc' you'll see `input-fn' (as far as I can tell always `eat--send-string') hence the hardcoding.

	  ;; Interactively (from shell's perspective) execute commands to integrate eat into established shell session.
	  (eat--send-string
	   eat-proc
	   shell-setup-cmd)))

  ;; Compute master eat digests from local running Emacs' `eat' files.
  (setq tjp/eat--master-digests (tjp/eat--get-digests
								 tjp/eat--source-script
								 eat-term-terminfo-directory))

  ;; (add-hook 'eat-mode-hook #'disable-local-global-hl-line)
  ;; (add-hook 'eat-exec #'tjp/eat--integrate)

  ;; Let `eat' know to run Bash when connected to a remote "podman".
  (add-to-list 'eat-tramp-shells '("podman" . "/bin/bash"))
  (add-to-list 'eat-tramp-shells '("ssh" . "/bin/bash"))
  (add-to-list 'eat-tramp-shells '("jam" . "/bin/bash")))

;;;; Projects

;; Project management and associated tasks (e.g. task running).

;;;;; Task running

;;;;;; Just

;; Just is a task runner, while not strictly needed its nice and limited in scope so that's a win for me honestly.

;; TODO: just mode based on treesitter?
;; Provides syntax highlighting.

(use-package just-mode
  :ensure)

;; Provides an interface to driving justfiles from Emacs.
;; TODO: determine executable without hardcoding?
(use-package justl
  :ensure
  :after tramp)
;; Looks like it asks for this in-case $PATH problems, so if we set a valid $PATH we should be fine.
;; :custom
;; (justl-executable "/opt/local/bin/just"))

;;;;; Project workspaces

;; Working on a project (think of a simple vc-dir) may involve simple work on one or two files, it may also involve looking at multiple vc-dirs, normal directories, with a specific window layout, various non-file-backed buffers (e.g. an eshell session), some notes and more.
;; Constantly re-establishing window layouts, buffers, and restoring (if even possible) non-file-backed buffers every time is annoying and in the latter case not possible (without custom code).
;; activities.el lets us do this, easily switching between different... activities which contain projects, buffers, non-file-backed buffers, window layouts and the like; all saved and restorable (even if Emacs closes).

;; (use-package activities
;;   :ensure
;;   :config
;;   (activities-mode)
;;   ;; Manage activities using `tab-bar` as tabs, instead of as frames.
;;   (activities-tabs-mode))

;; TODO: Creating a new tab does not drag the current buffer into it, there was an option for that (I think in Bufferlo, but it's generic to tab-bar).
;; TODO: Consult.

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  ;; XXX: use-package :custom-face expands to applying over #'face-spec-set which states it also defines the face name if not already done so we can do this instead of :config and calling defface.
  :custom-face
  ;; TODO: #efef00 is yellow-intense from modus themes
  ;; TODO: #d0bc00 is yellow from modus themes
  ;; TODO: #ff66ff is magenta intense from modus themes
  ;; TODO: #d2b580 is yellow-faint from modus themes
  (tab-bar-hint ((t :foreground "#efef00" :inherit tab-bar-tab)))
  (tab-bar-hint-inactive ((t :foreground "#d2b580" :slant italic :inherit tab-bar-tab-inactive)))
  :config
  (setq tjp/tab-bar-name-format-faces '((tab-bar-tab tab-bar-tab-inactive)
                                        (tab-bar-hint tab-bar-hint-inactive)))

  (defun tjp/tab-bar-name-format-all (name tab i)
    "Given a string NAME, tab TAB, and integer index I return a string to
display as the tab name. The string can be propertised."
    (let* ((tab-active (if (eq (car tab) 'current-tab) 0 1))
           (hint (if (= tab-active 0) " @" (format " %d" i))))

      ;; Add extra spaces around name and parentheses before adding properties so our additions are propertised.
      (if (= tab-active 0)
          (progn
            (setq name (concat "(" name ") "))
            (add-face-text-property 0 1 (nth 0 (nth 1 tjp/tab-bar-name-format-faces)) t name)
            (add-face-text-property (- (length name) 2) (length name) (nth 0 (nth 1 tjp/tab-bar-name-format-faces)) t name))
        (setq name (concat " " name "  ")))

      ;; Apply tab-bar-tab faces.
      (add-face-text-property 0 (length name) (nth tab-active (nth 0 tjp/tab-bar-name-format-faces)) t name)

      ;; Tab hint (index) and it's face.
      (when-let* ((tab-bar-tab-hints)
                  (hint-length (length hint)))
        (setq name (concat hint name))
        (add-face-text-property 0 hint-length (nth tab-active (nth 1 tjp/tab-bar-name-format-faces)) t name))

      name))

  ;; XXX: Crudely done to match config width of line number display column.
  (defun tjp/tab-bar-pad ()
    "Padding to apply to extreme edges of tab bar"
    "       ")

  :custom
  (tab-bar-close-button-show nil)
  ;; (tab-bar-auto-width-min '((25) 2))
  ;; (tab-bar-auto-width-max '((100) 20))
  (tab-bar-auto-width nil)
  (tab-bar-tab-hints t)
  (tab-bar-separator "")
  ;; XXX: If you want the close button back need to add the format function for it to this list.
  (tab-bar-tab-name-format-functions '(tab-bar-tab-name-format-truncated
                                       tjp/tab-bar-name-format-all))
  ;; TODO: What is the purpose of tab-bar-history-mode vs. something like winner-mode?
  (tab-bar-format '(tjp/tab-bar-pad
                    tab-bar-format-tabs
                    tab-bar-format-align-right
                    tab-bar-format-global))
  ;; By default the currently active buffer is also shown in a newly created tab, effectively bringing it into the new tabs local bufferlist. We share the default scratch buffer instead.
  ;; (tab-bar-new-tab-choice (lambda () (switch-to-buffer (generate-new-buffer-name "*local scratch*")))))
  (tab-bar-new-tab-choice "*scratch*"))

(use-package tabspaces
  :ensure
  :hook (elpaca-after-init . tabspaces-mode)
  :custom
  (tabspaces-keymap-prefix "H-p")
  (tabspaces-default-tab "Default"))

;; TODO: When minibuffer is completing directory/file-paths have backtick ` substituted to tilde ~ so I don't have to press shift-` to type ~ all the time.

;;;; Consult

(use-package consult
  :ensure
  :bind
  ("C-x b" . consult-buffer) ; orig: switch-to-buffer
  :config
  ;; Use Supplementary Private Use Area-B codepoints as seperators with consult otherwise the hotfuzz dynamic module errors. Consult's default seperator is a codepoint outside of the valid unicode range which dynamic modules cannot access.
  (setq
   consult--tofu-char #x100000
   consult--tofu-range #x00fffe))

;; (dolist (src consult-buffer-sources)
;;   (unless (eq src 'consult--source-buffer)
;;     (set src (plist-put (symbol-value src) :hidden t))))
;;;; Right-shift keymap

;; Interesting, there a good way to use these?
;; (keymap-set vertico-map "<f16> f" #'vertico-quick-insert)
;; (keymap-set vertico-map "<f16> c" #'vertico-quick-exit)

(defvar-keymap muh-map
  :doc "muh map"
  "w" #'save-buffer
  ;; TODO: Predicate for this to scope it to current project (if there is one) and (probably) do it silently without prompting us for each buffer.
  "W" #'save-some-buffers)

;;;; Custom (Hyper) keybinds
(keymap-global-set "H-w" #'save-buffer)
(keymap-global-set "H-c" #'comment-dwim)
(keymap-global-set "H-j" #'scroll-up-line)
(keymap-global-set "H-k" #'scroll-down-line)
(keymap-global-set "H-t" #'tramp-cleanup-connection)

;; Shortcuts to profiler start/stop/report
(defvar-keymap tjp/profiler-keymap
  :doc "TODO: profiler keymap (default cpu)"
  "q" #'profiler-start
  "w" #'profiler-stop
  "e" #'profiler-report)
(keymap-global-set "H-=" tjp/profiler-keymap)

;; move-to-window-line-top-bottom
;; recenter-top-bottom
;; scroll-up-command
;; scroll-down-command



;; TODO: Cool pattern here, perhaps useful for other things but obsolete now that using Karabiner to get better modifier behaviour.
;; Using as a general helper as modifier key real estate is limited.
;; (keymap-set global-map "<f16>" muh-map)
;; (keymap-set local-function-key-map "<f16>" 'event-apply-hyper-modifier)

;; (use-package org-inlinetask
;;   :ensure
;;   :defer t)

(use-package org-remark
  :ensure
  :defer t)

(use-package org-transclusion
  :ensure
  :defer t)

;; TODO: Disable custom.el shit?

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages nil)
;;  '(safe-local-variable-directories '("/Users/tsujp/prog/tree_sitter_noir/")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((((class color) (min-colors 256)) (:foreground "#efef00" :background unspecified))))
 '(diff-hl-delete ((((class color) (min-colors 256)) (:foreground "#ff5f5f" :background unspecified))))
 '(diff-hl-insert ((((class color) (min-colors 256)) (:foreground "#44bc44" :background unspecified))))
 '(fill-column-indicator ((((class color) (min-colors 256)) (:height 1.0 :foreground "#303030" :background unspecified))))
 '(meow-insert-cursor ((t (:background "#FFFFFF"))))
 '(meow-normal-cursor ((t (:background "#FFFF00")))))

;; TODO: Move above here; maybe.

;; TODO: Move this elsewhere.

(defun tsujp/diff-hl-modus-faces ()
  (modus-themes-with-colors
	(custom-set-faces
	 `(diff-hl-insert ((,c (:foreground ,green :background unspecified))))
	 ;; `(diff-hl-change ((,c (:foreground ,yellow-intense :background unspecified))))
	 `(diff-hl-delete ((,c (:foreground ,red-intense :background unspecified)))))))

;; TODO: diff-hl-mode is DESTROYING emacs scrolling performance in buffers... why?
;; TODO: So for now it's disabled.
(use-package diff-hl
  :disabled t
  :ensure
  :after modus-themes
  :config
  (define-fringe-bitmap 'tsujp/diff-hl-bitmap [224] nil 1 '(center repeated))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) 'tsujp/diff-hl-bitmap))
  (tsujp/diff-hl-modus-faces)
  :hook
  (prog-mode . diff-hl-mode)
  (prog-mode . diff-hl-flydiff-mode))

;; (let* ((width 2)
;;        (bitmap (vector (1- (expt 2 width)))))
;;   (define-fringe-bitmap 'my:diff-hl-bitmap bitmap 1 width '(top t)))
;; (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my:diff-hl-bitmap))

;; (setq diff-hl-fringe-bmp-function (lambda (type pos) 'my:diff-hl-bitmap))

;; END TODO.

;; TODO: Move elsehwere agian.

(use-package inspector
  :ensure
  :defer 1)

(use-package tree-inspector
  :ensure
  :defer 1)

;; END TODO.

(use-package keycast
  :ensure
  :defer 1)

;; TODO: Devdocs package
;; TODO: Kind icon: https://github.com/jdtsmith/kind-icon
;; TODO: Indent bars: https://github.com/jdtsmith/indent-bars

;; TODO: Place elsewhere.
;; TODO: Also hide corfu popupinfo after too.
;; (defun tsujp/re-maxi ()
;;   (interactive)
;;   (dotimes (_ 2)
;;     (funcall-interactively #'toggle-frame-fullscreen))
;;   (corfu-quit))

;; TODO IRC Config
;; (setq erc-modules '(sasl services-regain autojoin button completion fill imenu irccontrols list
;; 						 match menu move-to-prompt netsplit networks readonly ring stamp track))
;; (defun run-erc ()
;;   (interactive)
;;   (erc-tls :server "chat.sr.ht"
;; 		   :port 6697
;; 		   :nick "tsujp"
;; 		   :user "tsujp/irc.libera.chat"
;; 		   :password ""))
;; ---------------

(use-package go-ts-mode
  :ensure nil
  :custom
  ;; Should match tab-width.
  (go-ts-mode-indent-offset 4))

;; (use-package typescript-ts-mode
;;   :ensure nil
;;   )


;; TODO: Configure programming languages.
;; END programming language configuration.

;; TODO: Yes but not always visible, would like a way to toggle display this.
;; (setq flymake-show-diagnostics-at-end-of-line t)

;; TEMPORARY install transient from github head until 0.7.5 is in main emacs
(use-package transient
  :ensure
  :defer 1)
;; END TEMPORARY


;; TODO: Move elsewhere as appropriate.
(use-package flymake
  :ensure nil
  :custom
  (flymake-indicator-type 'margins)
  (flymake-autoresize-margins nil)
  (flymake-margin-indicators-string '((error "\u2045" compilation-error) (warning "\u2E2E" compilation-warning) (note "\u2E31" compilation-info))))
;; END TODO.

(use-package org
  :ensure nil
  :defer 1
  :custom
  ;; Show the options in the minibuffer and not a seperate window.
  (org-use-fast-todo-selection 'expert)
  (org-log-done t))
;; (org-log-into-drawer “LOGBOOK”))

;; (aset glyphless-char-display #xea87 "X")
;; (update-glyphless-char-display 'glyphless-char-display-control '((format-control . empty-box) (no-font . empty-box)))

;; This won't work with justl because it doesnt use comint directly. Or maybe it does but im doing something wrong idk.
;; (defun foo (str)
;;   (message "called")
;;   "bar")
;; (add-hook 'comint-preoutput-filter-functions #'foo)
;; ----

;; Project .dir-locals.el and local file variables override for self-owned projects since being prompted 5000 times is extremely annoying.
;; TODO: Move elsewhere but still within general init stuff.

;; TODO: This is currently very brittle and hyper speceific for the tree_sitter_noir project at the exact project location it occupies.
(defun tsujp/project-tree-sitter-test-org-export ()
  "Configures a project which uses ox-tst to load that file and
have after-save-hook events to export the rendered tree-sitter
test file."
  (load-file (expand-file-name "ox-tst.el"))
  (load-file (expand-file-name "ox-ngd.el"))
  ;; TODO: Only want this for the noir submodule directory and downwards however.
  ;; TODO: Are project-local variables a thing?
  ;; TODO: Only add this hook in test.org file. See and re-read docs for dir-locals-set-directory-class and dir-locals-set-class-variables both.
  (add-hook 'after-save-hook #'tst-export-current))

(use-package files
  :ensure nil
  ;; :defer 1
  :custom
  (enable-remote-dir-locals t)
  :config
  ;; This asks if you'd like to mark local variables for a directory as always safe (answer with plus sign: +) and it stores that answer above in custom-set-variables. Snippet copied here for understanding.
  ;;  '(safe-local-variable-directories '("/Users/tsujp/prog/tree_sitter_noir/")))
  ;; TODO: It would be better if this kind of thing was unified into project.el or something instead of being a hap-hazard hack. Perhaps contribute this to emacs when you have time?
  ;; Based on this: https://www.reddit.com/r/emacs/comments/yhs5zp/a_new_approach_for_me_for_project_wide_variables/
  ;; 2024/11/21: I am setting safe-local-variable-directories manually here since I don't want to use the customize interface.
  (setq safe-local-variable-directories '("/Users/tsujp/prog/tree_sitter_noir/"))
  (dir-locals-set-class-variables 'org-tree-sitter-test '((org-mode . ((eval . (tsujp/project-tree-sitter-test-org-export))))))
  (dir-locals-set-directory-class "~/prog/tree_sitter_noir" 'org-tree-sitter-test))

;; TODO: Annoying `starting "look" process` in messages buffer is coming from ispell.
;; In liue of better solutions perhaps this to shut it up:
;; (advice-add 'ispell-lookup-words :around
;;             (lambda (orig &rest args)
;;               (shut-up (apply orig args))))

;; (require 'org-inlinetask)

(setq org-indent-indentation-per-level 1)

(use-package popper
  :ensure
  :defer 1
  :bind (("H-r" . popper-toggle))
  :init
  (setq popper-reference-buffers
		'("\\*Messages\\*"
		  "\\*Warnings\\*"
		  "^\\*eat\\*$" eat-mode ; eat shell as a popup
		  help-mode
		  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))


;; Until the bug with polluting nonsense symbols in completion is fixed used te older function definition
;; (defun help-definition-prefixes ()
;;   "Return the up-to-date radix-tree form of `definition-prefixes'."
;;   (when (> (hash-table-count definition-prefixes) 0)
;;     (maphash (lambda (prefix files)
;;                (let ((old (radix-tree-lookup help-definition-prefixes prefix)))
;;                  (setq help-definition-prefixes
;;                        (radix-tree-insert help-definition-prefixes
;;                                           prefix (append old files)))))
;;              definition-prefixes)
;;     (clrhash definition-prefixes))
;;   help-definition-prefixes)
;; (help-definition-prefixes)


;; TODO: Put elsewhere.
;; (require 'org-id)
(use-package org-id
  :ensure nil
  :defer 1
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Original idea: https://writequit.org/articles/emacs-org-mode-generate-ids.html
  (defun prot-org--id-get ()
    "Get the CUSTOM_ID of the current entry.
If the entry already has a CUSTOM_ID, return it as-is, else
create a new one."
    (let* ((pos (point))
           (id (org-entry-get pos "CUSTOM_ID")))
      (if (and id (stringp id) (string-match-p "\\S-" id))
          id
	    (setq id (org-id-new "h"))
	    (org-entry-put pos "CUSTOM_ID" id)
	    id)))

  (declare-function org-map-entries "org")

  (defun prot-org-id-headlines ()
    "Add missing CUSTOM_ID to all headlines in current file."
    (interactive)
    (org-map-entries
     (lambda () (prot-org--id-get))))

  (defun prot-org-id-headline ()
    "Add missing CUSTOM_ID to headline at point."
    (interactive)
    (prot-org--id-get)))

;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; TODO: Per 4533 in consult.el set recentf-filename-handlers to nil?

;; TODO: Make it generic? i.e. it returns alist of properties for the return and user supplies a formatting function for that.
(defun tsujp/region-to-transclusion (f &optional project-base)
  ;; (interactive)
  "Given a file F with an active region, return a formatted org-transclusion
property. If optional argument PROJECT-BASE is non-nil file F's path is
relative to the project root that contains it (if any)."
  (with-current-buffer (find-buffer-visiting f)
	(if (use-region-p)
		(let ((proj-root (car (last (project-current nil f)))))
		  ;; TODO: Way to place cursor so foobar can be named more nicely, or integrate into org-capture somehow for that logic.
		  (format "#+transclude: [[file:%s][foobar]] :lines %s-%s :src foo"
				  ;; `project-current' calls down to `project-try-vc' which (looks like) it will only return a single list of 3 items, the last being the project root directory.
				  (if (and project-base proj-root)
					  (file-relative-name f proj-root)
					f)
				  ;; TODO: Automatically determine `:src LANG', perhaps from `org-src-lang-modes'?
				  ;; (org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
				  (line-number-at-pos (region-beginning)) (line-number-at-pos (region-end))))
	  (user-error "No region active in %s" f))))


;; (tsujp/region-to-transclusion "~/prog/tree_sitter_noir/noir/compiler/noirc_frontend/src/parser/parser.rs" t)
;; (tsujp/region-to-transclusion "~/prog/tree_sitter_noir/grammar.js" t)
;; (tsujp/region-to-transclusion "/Applications/MacPorts/Emacs.app/Contents/Resources/lisp/window.el.gz")

(defun tsujp/do-transclusion ()
  (interactive)
  (let ((transclude-prop))
	(with-selected-window (other-window-for-scrolling)
	  (message "Select region to transpose and C-M-c to confirm, C-] to abort")
	  (catch 'exit
		(recursive-edit)
		(setq transclude-prop (tsujp/region-to-transclusion (buffer-file-name) t))))
	(insert transclude-prop)))
;; (message "got: %s" (tsujp/region-to-transclusion (buffer-file-name) t))))))

;; No initial indentation in org source blocks.
(use-package org-src
  :ensure nil
  :defer 1
  :config
  (setq org-edit-src-content-indentation 0)
  (add-to-list 'org-src-lang-modes '("js" . js-ts))
  (add-to-list 'org-src-lang-modes '("rust" . rust-ts)))

;; TODO: Organise blah blah.
(use-package embark
  :ensure
  :defer 1
  :bind
  (("C-." . embark-act)))

(use-package embark-consult
  :ensure
  :defer 1
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
;; END TODO.

;; TODO: Organise alongside everything else.
;; Hilarious hack to give me dummy bindings for commands I want to invoke in kmacro without having the macro literally do M-x f o o RET (comparatively very slow). Ultimately would be good if Emacs could execute commands in macros without having to pollute entire global binding space with rubbish.
;; Much funny hacking with mekeor on #emacs 2024-11-03 ~9-10 AM KST.
;; TODO: Emacs mailing list macros invoking commands as a feature request?
(define-key (current-global-map) [tsujp org-store-link] #'org-store-link)
(define-key (current-global-map) [tsujp org-insert-link] #'org-insert-link)

;; Change final `j' count for moving to the next function, or chop that stuff off entirely and use I O at the front to simple suck the current function point is inside the name of.
(defalias 'noirc_functions_to_checklist_repeatable
  (kmacro "C-e i C-M-b M-w g d C-M-b o C-M-f C-M-f <tsujp> <org-store-link> n C-x o M-<return> r [ SPC ] SPC <tsujp> <org-insert-link> RET C-y ( ) RET <escape> C-x o M-, j j j j"))

(defalias 'noirc_function_to_link
  (kmacro "C-e i C-M-b M-w g d C-M-b o C-M-f C-M-f <tsujp> <org-store-link> n C-x o M-<return> r [ SPC ] SPC <tsujp> <org-insert-link> RET C-y ( ) RET <escape> C-x o M-, j j j j"))

(use-package htmlize
  :ensure
  :defer 1)

;; XXX: Might have to ask on IRC for this one, I don't see anything in project.el or files.el
;; TODO: Have project.el or some .dir-locals.el logic to mark a directory (and all it's children) as read-only? Or just do this via file system attributes (i.e. outside of Emacs?). Would prefer inside since it's not really applicable to elsewhere, just want to avoid constantly being prompted in emacs when accidental edits are made. Specifically I want to mark the entire noir submodule for the treesitter project as read-only.

;; TODO: Tramp in it's own little area.
;; TODO: Place elsewhere

;; TODO: Place elsewhere.
;; TODO: Fork this package and stop it using `reformatter' for formatting.
(use-package zig-mode
  :ensure
  :custom
  ;; Disable since this uses `reformatter' and results in double-format attempts as I use Apheleia instead.
  (zig-format-on-save nil)
  :defer 1)

;; TODO: Custom TRAMP dev box connection method thing ------------------------

;; START TEMP REDEFINITION UNTIL UPSTREAM TRAMP IS PATCHED.
(defun tramp-cleanup-all-connections ()
  "Flush all Tramp internal objects.
This includes password cache, file cache, connection cache, buffers."
  (declare (completion tramp-active-command-completion-p))
  (interactive)

  ;; Flush password cache.
  (password-reset)

  ;; Flush file and connection cache.
  (clrhash tramp-cache-data)

  ;; Initialize the cache version.
  (tramp-set-connection-property
   tramp-cache-version "tramp-version" tramp-version)

  ;; Remove ad-hoc proxies.
  (let ((proxies tramp-default-proxies-alist))
    (while proxies
      (if (ignore-errors
			(get-text-property 0 'tramp-ad-hoc (nth 2 (car proxies))))
		  (setq tramp-default-proxies-alist
				(delete (car proxies) tramp-default-proxies-alist)
				proxies tramp-default-proxies-alist)
		(setq proxies (cdr proxies)))))
  (when (and tramp-default-proxies-alist tramp-save-ad-hoc-proxies)
    (customize-save-variable
     'tramp-default-proxies-alist tramp-default-proxies-alist))

  ;; Cancel timers.
  (cancel-function-timers 'tramp-timeout-session)

  ;; Remove processes and buffers.
  (dolist (name (tramp-list-tramp-buffers))
    (when (processp (get-buffer-process name)) (signal-process name 'SIGTERM)) ; JORDAN: I changed this single line.
    (when (bufferp (get-buffer name)) (kill-buffer name)))

  ;; The end.
  (run-hooks 'tramp-cleanup-all-connections-hook))
;; END TEMP UNTIL TRAMP UPSTREAM FIXES THIS FUNCTION BY NOT USING delete-process.


;; TODO: Also need to patch tramp-cleanup-connection to stop using delete-process


;; Disables vc for all remotes. Could alter it to disable for all except a whitelisted tramp method e.g. "jam" or "podmancp" in the event there's trouble.
;; (setq vc-ignore-dir-regexp
;;       (format "\\(%s\\)\\|\\(%s\\)"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))

;; - ssh agent forwarding
;; - what about remote host config with a forwarded agent, and does git read my config too?
;; - gpg-agent for both

;; TODO: Perhaps do the same on project-prompter? i.e. `project-prompt-project-dir'
;; TODO: Message emacs devel and/or tramp, it looks like this code path skips non-essential which has been considered in other parts of project.el so it's strange this thing doesn't have logic for that resulting in me needing this hacky fix.
(defun project-forget-project (project-root)
  "Remove directory PROJECT-ROOT from the project list.
PROJECT-ROOT is the root directory of a known project listed in
the project list."
  (interactive (list (funcall project-prompter)))
  (let ((non-essential t)) ; JORDAN: My single change, rest is verbatim from project.el sources.
	(project--remove-from-project-list
	 project-root "Project `%s' removed from known projects")))

;; TODO: Transient menu for TRAMP connections and their statuses so they can be viewed more easily and what not?
;; TODO: Tramp shell history per container so it's not all shared (i.e. for these podman dev containers).

;; Setting recentf-filename-handlers to nil still causes TRAMP to establish a connection when C-n or C-p through list of completions regarding consult; MIGHT still be useful if recentf itself is explicitly used later on so keep commented here for now.
;; (setq recentf-filename-handlers nil)

;; This function constructs the list, its when you preview it (by C-n or C-p ing through the list) that the TRAMP connection is established, which does make sense honestly.. probably do want the preview. The trouble is if there's a slow host it could take a while to get that preview and it feels like during that time Emacs is blocked.
;; XXX: Come back to this. Probably need logic in consult--file-state or the preview handler, idk.
;; TODO: Well also it doesn't appear to be able to preview the remote file anyway so perhaps it should just be disabled since it's not being useful.
;; TODO: Could have it preview from the local file cache (if it's available) TRAMP stores that at ~/.cache/emacs/tramp.FOO where FOO is some garbled name of the file. Then, if no local preview is available just perhaps show a buffer with a message "<no preview>" or something idk. Best of both worlds in terms of connection opening. Perhaps then also an embark option to forcefully open a connection in order to get a preview?
;; consult--source-recent-file

;; XXX: Useful snippets for approaching benchmarking remote commands (could be applied to TRAMP): https://emacs.stackexchange.com/questions/451/efficiently-call-remote-processes?rq=1

;; (defun non-essential-prompter (&optional prompt)
;;   (let ((non-essential t))
;; 	(project-prompt-project-dir prompt)))
;; (use-package project
;;   :ensure nil
;;   :config
;;   (setopt project-prompter (cl-letf ((non-essential t)) #'project-prompt-project-dir)))
;; (setopt project-prompter #'non-essential-prompter))
;; (setopt project-prompter (let ((non-essential t)) (message "hi"))))
;; ---------------------------------------------------------------------------

(defun tjp/gimme-the-eln-cache-hash ()
  (interactive)
  (message "-> ELN CACHE DIR: %s" (md5 (concat "6" (concat emacs-version system-configuration system-configuration-options) (mapconcat #'comp--subr-signature comp-subr-list "")))))

;; TODO: If emacs freezes when opening a file that Org is involved with it's likely (at the time of writing this comment Wed 18 Dec 2024) an org-cache issue. To more easily clear the cache from within Emacs for that specific file without having to open it (thus crashing Emacs) execute the following (found from reading org-element-cache-reset source):
;;
;; (org-persist-unregister 'org-element--headline-cache "/Users/tsujp/prog/~wzht/infra/NOTES.org")
;; (org-persist-unregister 'org-element--cache "/Users/tsujp/prog/~wzht/infra/NOTES.org")
;;
;; Unsure if this removes all caching in the future or not, can check org-element--cache-active-p when current buffer is the file in question.
;; TODO: Should report this as a bug and help solve it since it's been going on for a while (month or two).

;; TODO: Put this somewhere better.
;; TRAMP DEV CONTAINER CUSTOMISATIONS -------------------------

;; TODO: Pass the workdir by overriding.
;; TODO: Add marginalia annotator to show the container image, associated project, and other information.

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))
;;  (setq compilation-environment '("TERM=dumb")) ;; Seems to be ignored, but if xterm-256color seems to be respected so patched at podmancp level.


(use-package tramp
  :ensure nil
  :config
  (setq tramp-verbose 0)

  ;; If debugging stuff.
  ;; (setopt tramp-debug-to-file t)
  ;; (setq tramp-verbose 6)
  ;; (setq tramp-completion-use-cache t)
  ;; (setq tramp-completion-use-auth-sources t)
  ;; (trace-function 'compilation-filter)
  ;; (trace-function 'compilation-sentinel)
  ;; (trace-function 'compilation-handle-exit)
  ;; (trace-function 'compilation-filter)
  ;; (trace-function 'compilation-parse-errors)
  ;; (trace-function 'compilation--ensure-parse)
  ;; (trace-function 'compilation--parse-region)

  ;; XXX: Temporary just to test speed
  ;; (setq remote-file-name-inhibit-auto-save t)

  ;; Lower than this size (in bytes?) Tramp will use external methods for connection (if any).
  ;; (setq tramp-copy-size-limit 100)

  ;; Jam tramp method.
  (setq tjp/jam--tramp-method
		`("jam"
		  (tramp-login-program ,tramp-podman-method)
		  (tramp-login-args (("exec")
							 ("-it")
							 ("-e" "TERM=dumb") ; JORDAN: Patch until response regarding this email chain: https://lists.gnu.org/archive/html/tramp-devel/2025-01/msg00020.html
							 ("-u" "jammy") ; default user `jammy'
							 ("--workdir" "/home/jammy/project")
							 ;; ("jam-%h") ; add `jam-' prefix to container name %h ;; XXX: hostname mismatches 'cos its templated... K.I.S.S.
							 ("%h")
							 ("%l")))
		  ;; (tramp-direct-async (,tramp-default-remote-shell "-c"))
          ;; (tramp-remote-shell ,tramp-default-remote-shell)
		  ;; (tramp-direct-async ("/bin/sh" "-c"))
		  ;; (tramp-direct-async ("/bin/bash" "-c"))
		  (tramp-direct-async ("/bin/sh" "--noediting" "--norc" "--noprofile" "-c"))
		  ;; (tramp-direct-async ("/bin/bash" "-c"))
		  ;; (tramp-direct-async ("/bin/sh" "-noediting" "-norc" "-noprofile" "-c"))
		  ;; (tramp-direct-async ("/bin/sh" "-c" "exec" "-c"))
          ;; (tramp-remote-shell "/bin/bash")
		  (tramp-remote-shell "/bin/bash")
		  ;; (tramp-remote-shell "/bin/bash")
          (tramp-remote-shell-login ("-l"))
          (tramp-remote-shell-args ("-i" "-c"))
		  ;; (tramp-remote-shell-args ("-l" "-i" "-c"))
		  (tramp-copy-program ,tramp-podman-method)
		  (tramp-copy-args (("cp")))
		  (tramp-copy-file-name (("%h" ":") ("%f")))
          (tramp-copy-recursive t)))


  ;; Jam tramp completion.
  (defun tjp/jam--tramp-completion-function (method)
	(tramp-skeleton-completion-function method
	  (when-let* ((raw-list
				   (shell-command-to-string
					(concat program " ps -a --filter 'label=sh.jammy.box' --format '{{.ID}}\t{{.Names}}'")))
				  (lines (split-string raw-list "\n" 'omit))
				  (names
				   (tramp-compat-seq-keep
					(lambda (line)
					  (when (string-match
							 (rx bol (group (1+ nonl))
								 ;; Could remove prefix if Tramp connection login args add it back e.g. jam-%h. Maybe Emacs completion backend has annotated values which could do this also but the jam prefix is intended to be a very "hardcoded" value and not fluid. Jam containers are dev environments.
								 ;; "\t" (? "jam-") (? (group (1+ nonl))) eol)
								 "\t" (? (group (1+ nonl))) eol)
							 line)
						(or (match-string 2 line) (match-string 1 line))))
					lines)))
		(mapcar (lambda (name) (list nil name)) names))))

  (tramp-set-completion-function "jam" '((tjp/jam--tramp-completion-function "jam")))

  ;; Add jam to tramp.
  (add-to-list 'tramp-methods tjp/jam--tramp-method)

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Define connection-local-variables defaults for jam.
  (defconst tjp/jam--connection-local-default-variables
	;; /bin/sh is a symlink to bash on fedora which is what jam uses
	;; '((shell-file-name . "/bin/bash")
	;;   (explicit-shell-file-name . "/bin/bash")
	'(
	  (tramp-direct-async-process . t)
	  ;; Does this stop extended SELinux attribute shite?
	  (tramp-use-file-attributes . nil)))

  ;; Profiles are names which associate variables to the connection they are applied to.
  (connection-local-set-profile-variables
   'tjp/jam--connection-local-defualt-profile
   tjp/jam--connection-local-default-variables)

  ;; How the jam connection type is identified (by connection-local).
  (connection-local-set-profiles
   `(:application tramp :protocol "jam")
   'tjp/jam--connection-local-defualt-profile))


;; (setopt shell-file-name "/bin/bash")
;; (setopt explicit-shell-file-name "/bin/bash")
;; END TRAMP DEV CONTAINER CUSTOMISATIONS

;; TODO: Set trash-directory to macOS trash bin

;; project-root
;; project-prefixed-buffer-name

;; TODO: And major mode too
(defun tjp/describe-mode-list (&optional buffer)
  "Like `describe-mode' but a simple list of names"
  (interactive)
  (unless buffer
	(setq buffer (current-buffer)))
  (let ((buffer-major (buffer-local-value 'major-mode buffer))
		(buffer-local-minors (buffer-local-value 'local-minor-modes buffer)))
  (with-output-to-temp-buffer "*tjp/describe-mode-list*"
	;; (princ "Major mode:\n    %s\n" buffer-major)
	(princ "Local minor modes:\n")
	(dolist (lmm buffer-local-minors) (princ (format "    %s\n" lmm)))
	(princ "Global minor modes:\n")
	(dolist (gmm global-minor-modes) (princ (format "    %s\n" gmm))))))

(defun tjp/debug--minimal ()
  "Disable some stuff that usually spams tramp; restart Emacs later if you want to undo this its meant to be quick and dirty."
  (diff-hl-mode -1)
  (diff-hl-flydiff-mode -1)
  (setq auto-revert-debug t))

;; about 0.33 seconds to save
;; auto-revert-buffer
;; vc-after-save
;; verify-visited-file-modtime
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function 'smtpmail-send-it)
 ;; '(smtpmail-debug-info t)
 ;; '(smtpmail-debug-verb t)
 '(smtpmail-smtp-server "smtp.migadu.com")
 '(smtpmail-smtp-service 465)
 '(smtpmail-stream-type 'ssl))

(setq user-mail-address "jc@wz.ht")



;; TODO: The repeat-mode stuff from Prot's config?

;; TODO: Move this big one to the bottom of init.el
;; TODO: Shorten my acronym to `tjp` since tsujp is a little too long (only within the context of M-x typing commands lol).
;; TODO: Probably better to just inspect the output of emacs-report-bug or whatever that command is as that generates a whole lot of diagnostic data.
(defun tsujp/emacs-builinfo ()
  (interactive)
  "Show compilation information for this Emacs"
  (message "-> CONFIGURATION OPTIONS:\n%s" system-configuration-options)
  (message "-> CONFIGURATION FEATURES:\n%s" system-configuration-features)
  (message "-> VERSION:\n%s" emacs-version)
  (message "-> TREESITTER?: %s" (treesit-available-p)))
;; TODO: Call the parts of emacs-build-description myself since it doesn't play nice when called as below.
;; (message "-> BUILD DESCRIPTION: %s\n" (emacs-build-description)))

(setq vc-make-backup-files t) ;; TODO: Put this elsewhere.

;; make sure gpg-agent.conf contains line `allow-loopback-pinentry'.
;; make sure gpg.conf contains line `pinentry-mode loopback` (not sure if this one is required, test later TODO).
;; TODO: Emacs won't read the XDG location I've specified i.e. ~/.config/gnupg and instead defaults to ~/.gnupg (or gpg is creating this trash folder, also ignoring my XDG config). In either case Emacs reads said trash folder which has no keys and fails to sign. Deleting said folder and adding a manual symlink to the correct one: `ln -s ~/.config/gnupg .gnupg` fixes the issue. That sucks, who is at fault here?
;; TODO: Put this elsewhere as appropriate.
(setq-default epg-pinentry-mode 'loopback)

;; TODO: Temp remote formatting experiment.
(defun mah-format ()
  (interactive)
  (let ((proc (make-process
               :name "format-zig"
               :buffer (current-buffer)
               :command '("zig" "fmt" "--stdin")
               :connection-type 'pipe
               :sentinel #'ignore
               :file-handler t)))
    (unless proc (error "failed to create process"))
    (process-send-string proc (buffer-substring-no-properties (point-min) (point-max)))
    (process-send-eof proc)
    (while (accept-process-output proc))
    ;; (process-send-eof proc)
    (message "done formatting")))
;; END temp remote formatting experiment.

;; If instrumenting Emacs startup behaviour.
;;(kill-emacs)
