;;; init.el -*- lexical-binding: t -*-

;;; Commentary

;; This is the main initialisation point for Emacs; default packages are available now so any (using default packages) shared configuration is done here (e.g. a macro). We then load individual package modules which include their configurations.

;;; Code

;;;; Meta information

;; TODO: Probably better to just inspect the output of emacs-report-bug or whatever that command is as that generates a whole lot of diagnostic data.
(defun tsujp/emacs-builinfo ()
  (interactive)
  "Show compilation information for this Emacs"
  (message "-> CONFIGURATION OPTIONS:\n%s" system-configuration-options)
  (message "-> CONFIGURATION FEATURES:\n%s" system-configuration-features)
  (message "-> VERSION:\n%s" emacs-version)
  (message "-> TREESITTER?: %s" (treesit-available-p)))

;; Compute these once instead of over-and-over at each callsite.
(defconst tsujp/modules-dir "modules")
(defconst tsujp/is-gui (display-graphic-p))
(defconst tsujp/is-mac (eq system-type 'darwin))

;;;; Subprocess performance tweaks

;; Mainly due to using LSPs which may or may not have verbose output (unfortunately); increase the maximum chunk-size of data we can read from these processes at a time.
(setq read-process-output-max (* 8 1024 1024))

;; Don't wait for more output to arrive, process it ASAP.
(setq process-adaptive-read-buffering nil)

;;;; Load path

;; Directory created (if necessary) and added to load-path.
(add-to-list 'load-path (directory-file-name (expand-file-name (locate-user-emacs-file tsujp/modules-dir))))

;;;; Silence native compilation

;; Useful when developing a package, annoying (as by default it's verbose) when consuming packages which output warnings when natively compiled.

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent))
										;(native-compile-prune-cache))

;;;; General (built-in)

;; General Emacs configuration concerning in-built packages only.

;;;;; Emacs

;; (setq-default fringes-outside-margins t)
(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq-default
   fill-column 95
   display-fill-column-indicator-character ?\u250A ; fill column indicator (2506 or 250A).
   fringes-outside-margins nil
   left-margin-width 1
   delete-by-moving-to-trash t           ; delete by moving to trash
   select-enable-clipboard t             ; unify emacs and system clipboard
   sentence-end-double-space nil         ; single space after fullstop
   indent-tabs-mode -1                   ; sane whitespace, please
   tab-width 4				 ; Smoller than 8
   initial-scratch-message ";; Scratch"  ; initial scratch message
   initial-major-mode 'fundamental-mode  ; a "nothing" mode for scratch buf
   inhibit-splash-screen t               ; hide welcome screen
   inhibit-startup-echo-area-message t   ; no echo area message
   buffer-file-coding-system 'utf-8 ; utf8 encoding
   locale-coding-system 'utf-8      ; utf8 encoding
   require-final-newline t               ; add newline on buffer save
   ;; display-line-numbers-type 'visual     ; set line numbers to relative
   display-line-numbers-type t 			; absolute line numbers
   display-line-numbers-grow-only t
   display-line-numbers-width 3          ; default width
   display-raw-bytes-as-hex t
   tab-always-indent 'complete           ; indentation & completion with TAB
   completion-cycle-threshold 3          ; TAB wraps if completion list small
   indicate-buffer-boundaries 'left      ; show buffer top/bottom in margin
   ;; indicate-buffer-boundaries t       ; show buffer end in margin
   use-short-answers t                   ; yes/no -> y/n
   save-interprogram-paste-before-kill t ; do not overwrite existing clipboard text on kill
   kill-do-not-save-duplicates t
   load-prefer-newer t 					; Load newer bytecode over old
   delete-pair-blink-delay 0
   ring-bell-function #'ignore            ; no beeping
   ;; echo-keystrokes 0.01                  ; immediate feedback in echo area on unfinished commands
   echo-keystrokes 0.1
   bidi-display-reordering 'left-to-right ; disable bi-directional text rendering by default
   ;; blink-matching-paren nil
   ;; hide commands in M-x not applicable to current mode
   read-extended-command-predicate #'command-completion-default-include-p
   blink-cursor-mode nil ; do not blink cursor
   show-paren-delay 0 ; show matching parenthesis quickly
   ;; Help config TODO: Place elsewhere?
   help-window-keep-selected t
   create-lockfiles nil ; no file locking please
   use-dialog-box nil ; prompt in minibuffer instead of dialog box (for the small commands that do so)
   treesit-font-lock-level 4 ; maximum fontification (hopefully not too skittle-like)
   )
  (set-default-coding-systems 'utf-8) ; utf8
  (prefer-coding-system 'utf-8)	   ; utf8
  ;; (global-display-line-numbers-mode)       ; enable display of line numbers at margin
  (column-number-mode)                     ; enable display of column number in minibuffer
  (global-hl-line-mode)                    ; enable highlight of current line
  (global-font-lock-mode 1)                ; force-enable font-face
  ;; (global-visual-line-mode)                ; visual-line-mode everywhere (TODO: This and below only
  ;; in prog and text?)
  (global-visual-wrap-prefix-mode)	   ; wrap with context-aware prefix everywhere
  (savehist-mode)                          ; save minibuffer history
  (fringe-mode '(5 . 6))
  ;; (winner-mode)                         ; window layout tracking (incase we need to undo)
  (recentf-mode)                        ; remember recently opened files, for easy re-visiting
  (save-place-mode)                     ; when re-visiting, point starts from where it was last time (instead of at the start of the file)
  (show-paren-mode)
  )

;; TODO: Place somewhere more appropriate, idk.
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'visual-line-mode)
(add-hook 'text-mode-hook #'visual-line-mode)

;;;;; Parenthesis / Delimitiers


;;;;; Whitespace

;; TODO: This is breaking fontification at the end of a buffer somehow. I've reported the bug by sending an email to the bug list on Wed 18 Sep 2024.
;; Remove empty lines at start/end of file, trailing whitespace on lines, and ensure a newline at end of file.
;; This happens BEFORE the file is saved; so use with external formatters won't bork things.
(use-package emacs
  :ensure nil
  :hook
  (prog-mode . whitespace-mode)
  (text-mode . whitespace-mode)
  ;; :Config
  ;; (global-whitespace-mode)
  ;; If using proportional font make it dimmer.
  ;; :custom-face
  ;; (whitespace-newline ((t (:foreground "#4C4C4C" :highlight nil))))
  :custom
  ;; newline newline-mark
  (whitespace-style '(face empty trailing missing-newline-at-eof))
  (whitespace-action '(cleanup auto-cleanup))
  :config
  (setq whitespace-display-mappings '((newline-mark ?\n [?⏎ ?\n] [?$ ?\n]))))

;; TODO: Have this only in one modeline, or is there a way to have global header bar above the tab bar?
;; TODO: If on a laptop (currently only do this for macos variant).
(display-battery-mode t)

;;;;; Font

;; TODO: Can this _sanely_ go into a use-package? Or maybe I don't bother as I end up using Fontaine.
;; TODO: If on macOS and gui AND on a display below retina ppi use a font weight of medium (at time of writing) if the defaults value AppleFontSmoothing is 0, otherwise use (whatever it needs to be) if it's 1. Then the same logic for if using a display above retina ppi (i.e. the laptops display). Because font weights shift around and it's all fucky.
;; defaults write org.gnu.emacs applefontsmoothing -int 0
;; todo: iosevka comfy?

;;(font-at (point))#<font-object "-*-Zed Mono-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1">
;;(font-at (point))#<font-object "-*-Iosevka Fixed SS03-regular-normal-normal-*-14-*-*-*-m-0-iso10646-1">

;; (font-spec
;;  :name "foo"
;;  :family "Zed Mono"
;;  :height 140
;;  :weight 'medium)

;; (create-fontset-from-fontset-spec "-*-Iosevka Fixed SS03-regular-normal-normal-*-14-*-*-*-m-0-fontset-if3")

;;#x2026
;;8230
;; (create-fontset-from-fontset-spec
;;  "-*-Zed Mono-medium-normal-normal-*-14-*-*-*-m-0-fontset-zm")

;; (set-fontset-font "fontset-zm" ?… (create-fontset-from-fontset-spec "-*-Iosevka Fixed SS03-regular-normal-normal-*-14-*-*-*-m-0-fontset-if3"))
;; (set-fontset-font t 'unicode "fontset-zm")
;; (set-fontset-font "fontset-default" 8230 "Iosevka Fixed SS03" nil 'prepend)

;;(set-face-attribute 'default nil :font "fontset-zm")

;; (set-face-attribute 'default nil :family "Iosevka Term SS01" :height 130)
;; (set-face-attribute 'default nil :family "Iosevka Term SS02" :height 130) ; prefer underline and * placeent on this
;; (set-face-attribute 'default nil :family "Iosevka Term SS05" :height 130) ; 1

(when tsujp/is-gui
  (set-face-attribute 'default nil
					  ;; :family "Iosevka Term SS01"
					  :family "tsujp"
					  :weight 'medium
					  ;; :height 140 ; On that 1080p samsung display.
					  :height 150 ; or 160 on mac display.
					  ;; :family "Zed Mono"
					  ;; :family "Zed Mono"
					  ;; :height 140
					  ;; :weight 'medium
					  ;; :font "fontset-zm"
					  ))
;; (set-frame-font "fontset-zm" t t)
;; (set-fontset-font "fontset-zm" 'unicode "Zed Mono")
;; (set-fontset-font "fontset-zm" ?… "Iosevka Fixed SS03")
;; (set-fontset-font "fontset-default" '(#x2026 . #x2027) "Iosevka Fixed SS03" nil 'prepend)
;; (set-fontset-font "fontset-zm" ?… (font-spec :script 'symbol) nil 'append)

;; applefontsmoothing -int 0 looks fine on 2560x1440 with these Zed settings.
;; :family "Zed Mono"
;; :height 140
;; :weight 'medium))

;;;;; Mouse

(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  (setq-default
   scroll-conservatively 101    ; scroll just enough to bring point back into view
   scroll-margin 2              ; padding at top/bottom of window which counts as scroll region
   scroll-step 1                ; keyboard scroll one line at a time
   mouse-wheel-follow-mouse t   ; mouse wheel scrolls window mouse is hovering over
   mouse-wheel-progressive-speed nil    ; don't accelerate scrolling
   ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
   ;; fast-but-imprecise-scrolling t ; redraw immediately when scrolling (v)
   ;; jit-lock-defer-time 0
   )
  (when tsujp/is-gui
    (context-menu-mode))                ; right-click to show context menu
  )

;;;;; Auto revert

;; When the file backing a buffer changes (perhaps edited by an external program e.g. git pull) auto-revert-mode can automatically refresh the buffers contents to match. This refresh is called a revert.

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :config
  (setq-default
   ;; TODO: Maybe want to set `global-auto-revert-non-file-buffers' to `t' but maybe not. Could lead to a lot of revert spamming, look into that later.
   ;; auto-revert-interval 1
   ;; auto-revert updates version control (vc) info already, however only if there are vc changes to the backing file directly (e.g. editing the file's contents externally); if a vc update occurs without editing the backing files contents directly auto-revert may miss this information. For example if you fetch and pull new commits which change other files that a buffer doesn't care about. Here vc info has changed (head commit) and unless auto-revert-check-vc-info is non-nil said vc info changes will be ignored.
   auto-revert-check-vc-info t
   ))

;;;;; Delete selection

;; Make Emacs normal and delete the selected region (if any) upon insertion of new text.

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; TODO: Random stuff to organise.

;; Should be a rule. I think I wanted a light stipple so it's not too distracting.
;; (setq display-fill-column-indicator-character ?\u2506) ; unused atm
;; (add-hook 'org-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;; (add-hook 'markdown-mode-hook #'display-fill-column-indicator-mode)

;; Thin window edges.
;; (set-window-fringes nil 1 nil)
;; (set-window-margins nil 0 0)

;; TODO: The repeat-mode stuff from Prot's config?

;; TODO: The current modes under the emacs config at the start of this file into their own use-package which is deferred?

;;;;; Unique buffer names

;; If two buffers have the same name, differentiate them by setting their respective names equal to whatever is left after the common-prefix between them is removed.

(use-package uniquify
  :ensure nil
  :defer t
  :custom
  (uniquify-strip-common-suffix t)
  (uniquify-buffer-name-style 'forward))

;;;; Spelling

(use-package ispell
  :ensure nil
  :defer t
  :custom
  (ispell-dictionary "en_GB"))


;;;; Search

(use-package isearch
  :ensure nil
  :defer t
  :config
  (setq isearch-lazy-count t)           ; show match counts (current and total) in search prompt
  ;; TODO: `lazy-count-prefix-format'.
  )

;;;; Operating system

;; Host operating-system-specific settings.

;;;;; Keyboard modifier layout

;; TODO: Update docs for this when configured karabiner-elements for tap/hold stuff.

;; I remap modifier keys on macOS and Linux to make them more ergonomic. The default ANSI keyboard layout has the following keys at the bottom-left of the keyboard, ordered left to right:
;;   |  Control  |  Super  |  Alt  |  Space  | ...

;; Note that Super is the neutral name for the Command key on Apple products, and the Windows key elsewhere.

;; I remap (using the host operating systems facilities) these to:
;;   |  Nothing  |  Alt  |  Control  |  Spacee  | ...

;; And then replace Caps Lock with Super for that platform. If on Linux I typically use Hyper instead as Linux doesn't discriminate between Control as a system-key like macOS does with Command. Unfortunately Linux decided to copy that choice from Windows. Think C-c being both copy _and_ SIGINT in a terminal; so dumb.

;; This section checks that modifier keys are adjusted accordingly so there are no surprises within Emacs.
										; TODO: Actually make that check comprehensive based on scancodes or keycodes or whatever; right now its assumed the host os has them set up correctly and they are translated into emacs properly.

;; TODO: Modifier usage above now out of date, as of 12 Jul 2024 moving back to caps lock as ctrl, and command/option remain as they are.

;; New settings after 12 Jul 2024 changes.
(when tsujp/is-mac
  (setq mac-command-modifier 'meta
		mac-option-modifier 'super
		mac-right-command-modifier 'hyper))

;; Original settings with older modifiers.
;; (when tsujp/is-mac
;;   (setq mac-command-modifier 'control
;;	mac-option-modifier 'super
;;	mac-control-modifier 'meta))



;;;;; macOS only

										; If running graphically on macOS we want to re-enable the menu bar since that is displayed outside of the application window (Emacs frame).

(when (and tsujp/is-gui tsujp/is-mac)
  ;; (setq ns-use-native-fullscreen nil)   ; set in early-init.el, commented out here as weak documentation.
  ;; (set-frame-parameter nil 'undecorated t)
  ;; (set-frame-parameter nil 'drag-internal-border t)
  ;; (set-frame-parameter nil 'internal-border-width 20)
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (menu-bar-mode 1))

;;;;; Packaging

;; Elpaca package manager configuration as well as package repositories.

;;;;; Elpaca

;;;;;; Bootstrap

;; The following code was copied from Elpaca's installation instructions README.md on 2024/07/01 commit b8ed514119df6aa0e065dfdf8c4fa75f0b8802ca.

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
							  :ref nil :depth 1
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
		(if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
				 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
												 ,@(when-let ((depth (plist-get order :depth)))
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

;;;;;; Enable use-package support

;; use-package is amazing for configuring packages, this makes sure Elpaca has integration.

(elpaca elpaca-use-package ; Install package `elpaca-use-package`.
  (elpaca-use-package-mode)) ; Enable support for use-package's `:ensure`.

;;;;;; Wait for Elpaca to bootstrap (if appropriate)

(elpaca-wait)

										;(tsujp/req 'general)
										;(tsujp/req 'os)
										;(tsujp/req 'packaging)

;;;; Env vars

;; Set environment variables in Emacs by sourcing them from a proper shell session.

;; TODO: Put this elsewhere, or better yet remove it and hardcode with something else (or make it faster idk) when krunvm stuff is possible.
;;; TODO: Put this in it's own file that needs to run as close to startup (after a package manager has been installed) as possible.
;;; Make Emacs use $PATH from users' shell.
(use-package exec-path-from-shell
  :ensure
  :init
  ;;(setq exec-path-from-shell-shell-name "bash")
  ;; (setq exec-path-from-shell-debug t) ; When debugging uncomment.
  (setq exec-path-from-shell-arguments '("-l"))
  ;;(setq exec-path-from-shell-arguments nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK" "LANG" "LC_CTYPE" "GNUPGHOME" "XDG_DATA_HOME" "XDG_CONFIG_HOME" "XDG_VIDEOS_DIR" "XDG_PICTURES_DIR" "XDG_DOWNLOAD_DIR" "XDG_MUSIC_DIR" "XDG_CACHE_HOME" "XDG_DESKTOP_DIR" "XDG_DOCUMENTS_DIR"))
	(add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;;;; Autosaves and backups

;; Autosaved files are surrounded by pounds (#) by default, whereas backup files are suffixed with tilde (~).

;; TODO: Place elsewhere?
;; backup versioned files (we don't commit on every save so back it up!)
(use-package emacs
  :ensure nil
  :after xdg
  :custom
  (auto-save-interval 300) ; keystroke-count between autosaves.
  (auto-save-timeout 60) ; seconds of idle time between autosaves.
  (backup-by-copying t) ; don't clobber symlinks.
  (make-backup-files t) ; backup file the first time it is saved.
  (version-control t) ; use version numbers on backups.
  (delete-old-versions t) ; silently (without confirmation prompt) delete old versions.
  (delete-by-moving-to-trash t)
  (kept-new-versions 10) ; count of newest versions to keep.
  (kept-old-versions 0) ; count of oldest versions to keep (keeping old versions clobbers new versions), see bottom of: https://www.emacswiki.org/emacs/ForceBackups
  (backup-directory-alist
   `(("." . ,(concat (xdg-cache-home) "/emacs/backups"))))
  ;; TODO: Auto saves still polluting, read docs for auto-save-file-name-transforms and test this config out to get this stuff to stop polluting. Perhaps use the hash thing too for long dirs and what not, in which ase a nice mapping file additionally?
  (auto-save-file-name-transforms `((".*" ,(concat (xdg-cache-home) "/emacs/autosaves") t))))
(setq vc-make-backup-files t)


;; make sure gpg-agent.conf contains line `allow-loopback-pinentry'.
;; make sure gpg.conf contains line `pinentry-mode loopback` (not sure if this one is required, test later TODO).
;; TODO: Emacs won't read the XDG location I've specified i.e. ~/.config/gnupg and instead defaults to ~/.gnupg (or gpg is creating this trash folder, also ignoring my XDG config). In either case Emacs reads said trash folder which has no keys and fails to sign. Deleting said folder and adding a manual symlink to the correct one: `ln -s ~/.config/gnupg .gnupg` fixes the issue. That sucks, who is at fault here?
;; TODO: Put this elsewhere as appropriate.
(setq-default epg-pinentry-mode 'loopback)

;;;; Theme
;; (tsujp/req 'theme)
;;;;; Modus theme

;; Using the non-bundled modus-theme so we can get new updates outside of Emacs release cycle (if we were to use the bundled version).
(use-package modus-themes
  :ensure
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-common-palette-overrides
   ;; '((border-mode-line-active bg-mode-line-active)
   ;;   (border-mode-line-inactive bg-mode-line-inactive)
   '(
     (border-mode-line-active unspecified)
     (border-mode-line-inactive unspecified)
     ;; (bg-prose-block-contents bg-blue-nuanced)
     ;; (bg-prose-block-delimiter bg-dim)
     ;; (fg-prose-block-delimiter fg-dim)
	 (fringe unspecified)
	 ;; (string green)
	 ;; (bg-paren-match bg-magenta-intense)
	 (underline-paren-match fg-main)))
  :custom-face
  (region ((t :extend nil)))
  :config
  (load-theme 'modus-vivendi :no-confirm)
  (tsujp/modus-fill-column-face-style)
  (tsujp/org-test-block-face))

;; TODO: Rework into hook within use-package for modus above.
(defun tsujp/modus-fill-column-face-style ()
  (modus-themes-with-colors
    (custom-set-faces
     `(fill-column-indicator ((,c (:height 1.0 :foreground ,bg-dim :background unspecified)))))))

(font-lock-add-keywords
 'org-mode '(("\\(^\s*#\\+begin_test\\(.*\n\\)*?\s*#\\+end_test\\)" 0 'org-test-block-face t)))

(defun tsujp/org-test-block-face ()
  (modus-themes-with-colors
    (defface org-test-block-face
	  `((t :background ,bg-prose-block-contents :extend t))
	  "Face for test block in org mode.")))

;; (defun my-modus-themes-custom-faces (&rest _)
;;   (modus-themes-with-colors
;;     (custom-set-faces
;;      ;; Add "padding" to the mode lines
;;      `(mode-line ((,c :box (:line-width 2 :color ,bg-mode-line-active))))
;;      `(mode-line-inactive ((,c :box (:line-width 2 :color ,bg-mode-line-inactive)))))))

;; (add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)
;;;;; Scrollbar in modeline

;; Scrollbars take up too much horizontal width, and especially when using macOS specifically with Emacs HEAD (i.e. Emacs' default NS integration) look terrible. If/when emacs-mac patches get merged we can think about using native scrollbars again.

;; TODO: This package useful or needed in modern emacs?: https://github.com/mrkkrp/cyphejor
;; TODO: Colours.
;; TODO: `after-init` hook.
(use-package mlscroll
  :ensure
  :hook (elpaca-after-init . mlscroll-mode))
;; :config
;; (mlscroll-mode))

;;;; Editing

;; Editing interface (e.g. modal editing) configuration.

;;;;; Meow

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

;; TODO: Move avy to it's own place.
(use-package avy
  :ensure
  :custom
  (avy-timeout-seconds 0.3))
;; :custom-face
;; (avy-goto-char-timer-face ((t (:foreground "#FFFF00"))))
;; (avy-lead-face ((t (:foreground "#FFFF00" :background nil :weight black))))
;; (avy-lead-face-0 ((t (:foreground "#f78fe7" :background "#555" :weight black)))))

(keymap-global-set "H-e" #'avy-goto-char-timer)

;; (defun blah/blah ()
;; (interactive)
;; (message "the frame is: %s" (window-frame))
;; (set-frame-parameter nil 'fullscreen nil))
(keymap-global-set "H-`" #'toggle-frame-fullscreen)

;; frame hide title bar when maximized

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
  :custom
  (meow-expand-hint-counts '((word . 3) (line . 3) (block . 3) (find . 3) (till . 3)))
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

;;;; Completion
										;(tsujp/req 'completion)
;; Completion facilities, frameworks, and interfaces.

;; The Emacs completion system uses a completion frontend which provides a completion UI for the user, said frontend calls a completion backend which provides completions based on a configurable completion style.

;;;;; Minibuffer

;; Default minibuffer settings.

;; TODO: Extra minibuffer config?
(use-package minibuffer
  :ensure nil
  :demand t
  :config
  (setq minibuffer-visible-completions t ; nil is default, I used to use t
		;; completion-styles '(hotfuzz orderless basic)
        completion-styles '(hotfuzz orderless)
		;; completion-styles '(orderless basic)
        ;; completion-styles '(hotfuzz)
		;; Ensures above completion-styles are always respected by other packages.
		completion-category-defaults nil
		completion-category-overrides '((file (styles . (basic partial-completion hotfuzz orderless))))))
;; completion-category-overrides '((file (styles . (basic partial-completion orderless))))))
;; completion-category-overrides '((file (styles . (hotfuzz))))))

;; OLD minibuffer config
;; (use-package minibuffer
;;   :ensure nil
;;   :demand t
;;   :config
;;   (setq minibuffer-visible-completions t
;;         completion-styles '(basic substring initials flex orderless)))

;;;;; Orderless

;; Provides a completion style with configurable components to match literally, by regexp, by word prefixes and more. Most notably to complete things out of order and in shorthand.

;; TODO: Extra orderless config?
(use-package orderless
  :ensure
  ;; :demand t
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
;; :config
;; (vertico-mode))

;;;;; Corfu

;; Displays completion candidates for current point in a popup either below or above said point using Emacs' in-built completion facilities (so ties in with Vertico, and could be used without Vertico if desired).
;; Specifically Corfu is for buffer completions (e.g. identifiers when programming) and serves as a completion frontend.

;; TODO: Corfu go back to previous help buffer window if it moved it during completion.

;; TODO: Extra corfu config?
(use-package corfu
  ;; :defer 1
  :ensure
  :hook (elpaca-after-init . global-corfu-mode)
  :custom
  (corfu-auto 1)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; show documentation after `corfu-popupinfo-delay`
  :bind
  (:map corfu-map
		;; Stop corfu stealing the RET key when completing.
		("RET" . nil)))
;; ESC to close completion (maybe TODO).
;; :config
;; (global-corfu-mode))
;; TODO: Same as in Vertico.
										;:hook (after-init . global-corfu-mode))

;;;;; Marginalia

;; Display annotations alongside minibuffer completion candidates.

;; TODO: Extra marginalia config?
(use-package marginalia
  ;; :defer 1
  :ensure
  :hook (elpaca-after-init . marginalia-mode))
;; :config
;; (marginalia-mode))
;; TODO: Same as in Vertico.
;;:hook (after-init . marginalia-mode))

;;;;; Which-key

;; Display keybindings for the currently (incomplete) command.

(use-package which-key
  :ensure nil
  :hook (after-init . which-key-mode))


;;;;; Case-sensitivity

;; Remove case sensitivity from general completions.

(use-package emacs
  :ensure nil
  :custom
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (case-fold-search t)
  (read-file-name-completion-ignore-case t))


;;;; Version Control

;; Version control systems and their configuration.

;;;;; Built-in
										;(tsujp/req 'vcs)
;; TODO: More configuration as appropriate.
(use-package vc
  :ensure nil
  :config
  (setq vc-handled-backends '(Git)))

;; TODO: Maybe `vc-git-log-edit-summary-max-len` and `vc-git-log-edit-summary-target-len`.

;;;;; Magit
										;(tsujp/req 'magit)

(use-package magit
  ;; :defer 1
  :ensure
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
										;(tsujp/req 'treesitter)
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
  ;; TODO: This automatically and unified with grammar definition source above.
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode)))

;;;; Terminal
										;(tsujp/req 'terminal)
;; Eat seems to be fast enough when paired with certain optimisations like setting `process-adaptive-read-buffering` to nil as mentioned here: https://www.reddit.com/r/emacs/comments/17nl7cw/shout_out_to_the_eat_terminal_emulator_package/#k7tmgz0
;; If eat is ever legitimately too slow, perhaps consider using vterm for eshell visual commands via: https://github.com/iostapyshyn/eshell-vterm/blob/master/eshell-vterm.el
;; However because eat by itself offers great Emacs integration and an eshell mode I'll use eat for now.

;; global-hl-line-mode and hl-line-mode do not interact (the single exception being the latter if invoked interactively). So, to disable hl-line-mode in a buffer when global-hl-line-mode was earlier called the self-same local variable must be set to nil.
(defun disable-local-global-hl-line ()
  (setq-local global-hl-line-mode nil))

;; TODO: after-init hook.
(use-package eat
  :ensure
  :hook (elpaca-after-init . eat-eshell-mode)
  :init
  (add-hook 'eat-mode-hook #'disable-local-global-hl-line)
  :config
  ;; Clear commands eshell considers visual by default.
  (setq eshell-visual-commands '())
  (setq eat-minimum-latency 0.002))
;; (add-hook 'eat-mode-hook #'disable-local-global-hl-line)
;; (eat-eshell-mode))

;;;; Projects
										;(tsujp/req 'projects)
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
  :ensure)
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
  :custom
  ;; By default the currently active buffer is also shown in a newly created tab, effectively bringing it into the new tabs local bufferlist. We share the default scratch buffer instead.
  ;; (tab-bar-new-tab-choice (lambda () (switch-to-buffer (generate-new-buffer-name "*local scratch*")))))
  (tab-bar-new-tab-choice "*scratch*"))

;; TODO: after-init hook when fixed.
(use-package tabspaces
  :ensure
  :hook (elpaca-after-init . tabspaces-mode)
  :custom
  (tabspaces-keymap-prefix "H-p")
  (tabspaces-default-tab "Default"))
;; :config
;; (tabspaces-mode))

;; TODO: When minibuffer is completing directory/file-paths have backtick ` substituted to tilde ~ so I don't have to press shift-` to type ~ all the time.


;; TODO: Single-file config init.el

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


;; TODO: Cool pattern here, perhaps useful for other things but obsolete now that using Karabiner to get better modifier behaviour.
;; Using as a general helper as modifier key real estate is limited.
;; (keymap-set global-map "<f16>" muh-map)
;; (keymap-set local-function-key-map "<f16>" 'event-apply-hyper-modifier)

(use-package org-remark
  :ensure)

(use-package org-transclusion
  :ensure)

;;; ------ Experimental stuff.
;; (defun some-handler ()
;;   (interactive)
;;   (message "unread events: %s" (this-command-keys)))
;; (discard-input)
;; (funcall-interactively 'event-apply-hyper-modifier "s"))
;; (message "got key: %s" (read-key t)))
;; (funcall-interactively 'event-apply-hyper-modifier "s"))
;; (message "event is: %S" last-input-event)) ;
;; (message "last inpu tevent %S" last-input-event))
;; (vector (event-apply-modifier (read-event) 'hyper 24 "H-")))
;; (keymap-set global-map "H-g" muh-map)
;; (keymap-set input-decode-map "<f16>" [16777319])
;; (keymap-set input-decode-map "<f16>" 'some-handler)
;; (keymap-set special-event-map "<f6>" 'some-handler)
;; (event-convert-list '(hyper ?g))
;; (key-description [27])
;; Interesting, now double pressing f16 invokes f16 DEL
;; (keymap-set local-function-key-map "<f16>" 'some-handler)
;; (keymap-global-set "<f16> w" 'save-buffer)
;; ------------- end experimental stuff

;; TODO: Disable custom.el shit?

										;(message (emacs-init-time))
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
	 `(diff-hl-change ((,c (:foreground ,yellow-intense :background unspecified))))
	 `(diff-hl-delete ((,c (:foreground ,red-intense :background unspecified)))))))

;; TODO: diff-hl-mode is DESTROYING emacs scrolling performance in buffers... why?
;; TODO: So for now it's disabled.
(use-package diff-hl
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

;; TODO: Place alongside orderless (completion style relavence)
;; TODO: Use the C module for MOAR SPEED?
;; TODO: Autocompile that C module if not already done.
;; https://github.com/axelf4/hotfuzz
(use-package hotfuzz
  :ensure)
;; :defer 1)

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

;; TODO: Why is fontification at the end of the buffer weird as I type? White and then fontified after? Regex problem?

;; TODO: Pseudo-global mode line
;; (setq tab-bar-format
;; TODO: -----------------------

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

;; LSP-MODE

;; TODO: Can this be done only for go-ts-mode rather than globally?
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-ts-mode
  :ensure nil
  :custom
  ;; Should match tab-width.
  (go-ts-mode-indent-offset 4))

;; (use-package typescript-ts-mode
;;   :ensure nil
;;   )

(use-package lsp-mode
  :ensure
  :defer 1
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((typescript-ts-mode . lsp-deferred)
		 (go-ts-mode . lsp-deferred)
         (rust-ts-mode . lsp-deferred))
  :commands lsp lsp-deferred
  :custom
  ;; (lsp-auto-configure nil) ; nah mate, we'll configure ourselves (lsp-mode has A LOT of bloat).

  (lsp-log-io nil) ; set to `t' to troubleshoot LSP server problems (shows communication logs).
  (lsp-keep-workspace-alive nil) ; kill LSP server if all project buffers are killed.

  ;; Core
  (lsp-enable-xref t)
  (lsp-eldoc-enable-hover t) ; Eldoc integration.
  (lsp-enable-imenu t) ; imenu integration when server `textDocument/documentSymbol'.
  (lsp-enable-symbol-highlighting nil) ; Symbol usage at point in current buffer.
  (lsp-enable-links t) ; TODO: default is `t', is this feature useful/bloatware/expensive?
  (lsp-enable-dap-autoconfigure t)
  (lsp-enable-file-watchers t) ; TODO: Performance of this, perhaps `entr' as an Emacs module?
  (lsp-enable-folding nil) ; Bloat.
  (lsp-enable-indentation nil) ; Bloat.
  (lsp-enable-on-type-formatting nil) ; Bloat.
  (lsp-enable-text-document-color nil) ; Bloat.

  ;; Diagnostics
  (lsp-diagnostics-provider :flymake)

  ;; Completion
  (lsp-completion-provider :none) 	; Disable company integration.
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; e.g. additionally insert an import statement.
  ;; (lsp-enable-completion-at-point t) ; TODO: This is the same as `lsp-completion-enable'?
  (lsp-enable-relative-indentation nil)
  (lsp-enable-snippet nil) ; TODO: where do snippets come from?

  ;; Clients (actually servers lol)
  (lsp-disabled-clients '(deno-ls)) ; XXX: Appears Deno2 is dead in the water agian, artificial Deno-only limitations.
  (lsp-clients-typescript-prefer-use-project-ts-server t)

  ;; Modeline / Headerline
  (lsp-signature-doc-lines 1) ; Don't raise the Echo area height.
  ;; (lsp-ui-doc-use-childframe t) ; TODO: Needed?
  ;; (lsp-eldoc-render-all nil) ; TODO: I need this? Is this the thing making the echo area HUGE?
  (lsp-modeline-workspace-status-enable t)
  (lsp-modeline-diagnostics-enable nil) ; Already showing them via Flymake.
  (lsp-modeline-code-actions-enable nil) ; Too noisy.
  (lsp-headerline-breadcrumb-enable nil) ; Bloatware.

  ;; (lsp-inlay-hint-enable t)

  ;; Fucking Microsoft, why is this bullshit a part of the LSP spec?
  ;; (lsp-enable-on-type-formatting nil)
  ;; (lsp-enable-indentation nil)
  (lsp-trim-final-newlines nil)
  (lsp-semantic-tokens-enable nil))

;; END LSP-MODE


;; TODO: Configure programming languages.

;; https://github.com/radian-software/apheleia#user-guide
(use-package apheleia
  :ensure
  ;; :defer 1
  :hook (elpaca-after-init . apheleia-global-mode)
  :config
  ;; TODO: For other programming languages also.
  ;; Change formatter for JavaScript files to Biome.
  (add-to-list 'apheleia-formatters '(biome "biome" "check" "--apply" "--stdin-file-path" filepath))
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . biome))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . biome)))
;; (apheleia-global-mode))

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
  (add-hook 'after-save-hook #'tst-export-current))

(use-package files
  :ensure nil
  ;; :defer 1
  :config
  ;; This asks if you'd like to mark local variables for a directory as always safe (answer with plus sign: +) and it stores that answer above in custom-set-variables. Snippet copied here for understanding.
  ;;  '(safe-local-variable-directories '("/Users/tsujp/prog/tree_sitter_noir/")))
  ;; TODO: It would be better if this kind of thing was unified into project.el or something instead of being a hap-hazard hack. Perhaps contribute this to emacs when you have time?
  ;; Based on this: https://www.reddit.com/r/emacs/comments/yhs5zp/a_new_approach_for_me_for_project_wide_variables/
  (dir-locals-set-class-variables 'org-tree-sitter-test '((org-mode . ((eval . (tsujp/project-tree-sitter-test-org-export))))))
  (dir-locals-set-directory-class "~/prog/tree_sitter_noir" 'org-tree-sitter-test))

;; TODO: Annoying `starting "look" process` in messages buffer is coming from ispell.
;; In liue of better solutions perhaps this to shut it up:
;; (advice-add 'ispell-lookup-words :around
;;             (lambda (orig &rest args)
;;               (shut-up (apply orig args))))

(require 'org-inlinetask)

(use-package dap-mode
  :ensure)

(setq org-indent-indentation-per-level 1)

;; Trying out to see if scrolling less laggy feeling with these.
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;; TODO: Move as appropriate.
(electric-pair-mode 1)

(use-package popper
  :ensure
  :defer 1
  :bind (("H-r" . popper-toggle))
  :init
  (setq popper-reference-buffers
		'("\\*Messages\\*"
		  "^\\*eat\\*$" eat-mode ; eat shell as a popup
		  help-mode
		  compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

;; (use-package vterm
;;   :ensure
;;   :defer 1)

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
(require 'org-id)

;; Original idea:
;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
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

;;;###autoload
(defun prot-org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (prot-org--id-get))))

;;;###autoload
(defun prot-org-id-headline ()
  "Add missing CUSTOM_ID to headline at point."
  (interactive)
  (prot-org--id-get))

;; TODO: Per 4533 in consult.el set recentf-filename-handlers to nil?

(defun tsujp/region-to-transclusion (f &optional project-base)
  ;; (interactive)
  "Given a file F return a formatted org-transclusion property. If
optional argument PROJECT-BASE is non-nil file F's path is relative
to the project root that contains it (if any)."
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
