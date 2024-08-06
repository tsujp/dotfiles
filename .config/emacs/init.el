;;; init.el -*- lexical-binding: t -*-

;;; Commentary

;; This is the main initialisation point for Emacs; default packages are available now so any (using default packages) shared configuration is done here (e.g. a macro). We then load individual package modules which include their configurations.

;;; Code

;;;; Meta information

;; Compute these once instead of over-and-over at each callsite.
;;(defconst tsujp/custom-config-dir "tsujp")
(defconst tsujp/dyn-mod-dir "tsujp_modules")
(defconst tsujp/is-gui (display-graphic-p))
(defconst tsujp/is-mac (eq system-type 'darwin))

;;;; Subprocess performance tweaks

;; Mainly due to using LSPs which may or may not have verbose output (unfortunately); increase the maximum chunk-size of data we can read from these processes at a time.
;; (setq read-process-output-max (* 1024 1024))
(setq read-process-output-max (* 8 1024 1024))

;; Don't wait for more output to arrive, process it ASAP.
(setq process-adaptive-read-buffering nil)

;;;; Load path

;; Configured external packages are in `modules` and any custom lisp code (that isn't configuration) is under `lisp`.
;; TODO: use dolist macro for this instead?
;; TODO: Perhaps add back a directory (maybe) if/when lots of custom stuff, idk.
;; (mapc
;;  (lambda (string)
;;    (add-to-list 'load-path (locate-user-emacs-file (concat tsujp/custom-config-dir "/" string))))
;;  '("modules" "lisp"))

;;;;; Dynamic modules

;; Directory created (if necessary) and added to load-path.
;; (add-to-list 'load-path (locate-user-emacs-file (concat tsujp/dyn-mod-dir "/")))
(add-to-list 'load-path (directory-file-name (expand-file-name (locate-user-emacs-file  tsujp/dyn-mod-dir))))


;;;; Silence native compilation

;; Useful when developing a package, annoying (as by default it's verbose) when consuming packages which output warnings when natively compiled.

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-compile-prune-cache t))

;;;; General (built-in)

;; General Emacs configuration concerning in-built packages only.

;;;;; Emacs

(setq-default fringes-outside-margins t)

(use-package emacs
  :ensure nil
  :demand t
  :config
  (setq-default
   fill-column 95
   display-fill-column-indicator-character ?\u2506 ; fill column indicator
   delete-by-moving-to-trash t           ; delete by moving to trash
   select-enable-clipboard t             ; unify emacs and system clipboard
   sentence-end-double-space nil         ; single space after fullstop
   indent-tabs-mode -1                   ; sane whitespace, please
   initial-scratch-message ";; Scratch"  ; initial scratch message
   initial-major-mode 'fundamental-mode  ; a "nothing" mode for scratch buf
   inhibit-splash-screen t               ; hide welcome screen
   inhibit-startup-echo-area-message t   ; no echo area message
   buffer-file-coding-system 'utf-8-unix ; utf8 encoding
   locale-coding-system 'utf-8-unix      ; utf8 encoding
   require-final-newline t               ; add newline on buffer save
   display-line-numbers-type 'visual     ; set line numbers to relative
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
   delete-pair-blink-delay 0
   ring-bell-function 'ignore            ; no beeping
   echo-keystrokes 0.01                  ; immediate feedback in echo area on unfinished commands
   bidi-display-reordering 'left-to-right ; disable bi-directional text rendering by default
   ;; blink-matching-paren nil
   ;; hide commands in M-x not applicable to current mode
   read-extended-command-predicate #'command-completion-default-include-p
   blink-cursor-mode nil ; do not blink cursor
   show-paren-delay 0 ; show matching parenthesis quickly
   ;; Help config TODO: Place elsewhere?
   help-window-keep-selected t
   )
  (set-default-coding-systems 'utf-8-unix) ; utf8
  (prefer-coding-system 'utf-8-unix)	   ; utf8
  (global-display-line-numbers-mode)       ; enable display of line numbers at margin
  (column-number-mode)                     ; enable display of column number in minibuffer
  (global-hl-line-mode)                    ; enable highlight of current line
  (global-font-lock-mode 1)                ; force-enable font-face
  (global-visual-line-mode)                ; visual-line-mode everywhere (TODO: This and below only in prog and text?)
  (global-visual-wrap-prefix-mode)	   ; wrap with context-aware prefix everywhere
  (savehist-mode)                          ; save minibuffer history
  (fringe-mode '(2 . 6))
  )

;; TODO: Place somewhere more appropriate, idk.
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;;;;; Parenthesis / Delimitiers


;;;;; Whitespace

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
(when tsujp/is-gui
  (set-face-attribute 'default nil
		      :family "Zed Mono"
                      :height 160
                      :weight 'medium))

;;;;; Mouse

(use-package mouse
  :ensure nil
  :hook (after-init . mouse-wheel-mode)
  :config
  (setq-default
   scroll-conservatively 101    ; scroll just enough to bring point back into view
   scroll-margin 6              ; padding at top/bottom of window which counts as scroll region
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
;; (setq display-fill-column-indicator-character ?\u2502) ; unused atm
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
  :custom
  (uniquify-strip-common-suffix t)
  (uniquify-buffer-name-style 'forward))

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
        mac-option-modifier 'super))

;; Original settings with older modifiers.
;; (when tsujp/is-mac
;;   (setq mac-command-modifier 'control
;; 	mac-option-modifier 'super
;; 	mac-control-modifier 'meta))

;;;;; macOS only

; If running graphically on macOS we want to re-enable the menu bar since that is displayed outside of the application window (Emacs frame).

(when (and tsujp/is-gui tsujp/is-mac)
  ;; (setq ns-use-native-fullscreen nil)   ; set in early-init.el, commented out here as weak documentation.
  (set-frame-parameter nil 'undecorated t)
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
  (setq exec-path-from-shell-debug t)
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
   `(("." . ,(concat (xdg-cache-home) "/emacs/backups")))))
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
  :config
  (load-theme 'modus-vivendi :no-confirm))

;;;;; Scrollbar in modeline

;; Scrollbars take up too much horizontal width, and especially when using macOS specifically with Emacs HEAD (i.e. Emacs' default NS integration) look terrible. If/when emacs-mac patches get merged we can think about using native scrollbars again.

;; TODO: Put this into a modeline configuration module instead?
;; TODO: This package useful or needed in modern emacs?: https://github.com/mrkkrp/cyphejor
;; TODO: Colours.
;; TODO: `after-init` hook.
(use-package mlscroll
  :ensure
  :config
  (mlscroll-mode))

;;;; Editing

;; Editing interface (e.g. modal editing) configuration.
;(tsujp/req 'editing)
;;;;; Meow

;; TODO: Cursor styles based on mode, here or configured elsewhere?
(defun tsujp/meow-cursor ())

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

    '("'" . meow-reverse)

    '("K" . meow-prev-expand)
    '("L" . meow-right-expand)
    '("J" . meow-next-expand)
    '("H" . meow-left-expand)

    ;; Movement
    '("k" . meow-prev)
    '("l" . meow-right)
    '("j" . meow-next)
    '("h" . meow-left)

    ;; Expansion
    ;'("o" . meow-word)
    '("n" . meow-cancel-selection)
    '("s" . meow-line)

    ;; TODO: Command `meow-till-expand` is quite nice; work it in somewhere?
    ;; TODO: c for copy and x for paste?

    ;; Editing
    '("d" . meow-kill)
    '("e" . meow-insert)
    '("E" . meow-open-above)
    '("r" . meow-append)
    '("R" . meow-open-below)

    '("u" . undo-only)
    '("U" . undo-redo)

    '("v" . meow-end-of-thing)
    '("V" . meow-beginning-of-thing)

    ;; Prefix ;
    '(";c" . meow-comment)
    '(";w" . save-buffer)

    ;; Ignore escape
    '("<escape>" . ignore)))

(defun meow-setup ()
  (progn
    (tsujp/meow-cursor)
    (tsujp/meow-ergo-keys)))

(use-package meow
  :ensure
  :config
  (meow-setup)
  (meow-global-mode 1))

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
  (setq minibuffer-visible-completions t
        completion-styles '(basic substring initials flex orderless)))

;;;;; Vertico

;; Vertical completion framework which uses Emacs' in-built completion engine; meaning it can be used generically instead of inventing it's own custom API (unlike say Helm or Ivy).
;; Specifically Vertico is for minibuffer completions and serves as a completion frontend.

;; TODO: Any extra vertico config?
(use-package vertico
  :ensure
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
  :config
  (vertico-mode))

;;;;; Corfu

;; Displays completion candidates for current point in a popup either below or above said point using Emacs' in-built completion facilities (so ties in with Vertico, and could be used without Vertico if desired).
;; Specifically Corfu is for buffer completions (e.g. identifiers when programming) and serves as a completion frontend.

;; TODO: Extra corfu config?
(use-package corfu
  :ensure
  :custom
  (corfu-auto 1)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; show documentation after `corfu-popupinfo-delay`
  :bind
  (:map corfu-map
        ;; Stop corfu stealing the RET key when completing.
        ("RET" . nil))
  ;; ESC to close completion (maybe TODO).
  :config
  (global-corfu-mode))
  ;; TODO: Same as in Vertico.
  ;:hook (after-init . global-corfu-mode))

;;;;; Orderless

;; Provides a completion style with configurable components to match literally, by regexp, by word prefixes and more. Most notably to complete things out of order and in shorthand.

;; TODO: Extra orderless config?
(use-package orderless
  :ensure
  :demand t
  :after minibuffer)

;;;;; Marginalia

;; Display annotations alongside minibuffer completion candidates.

;; TODO: Extra marginalia config?
(use-package marginalia
  :ensure
  :config
  (marginalia-mode))
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
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (zig . ("https://github.com/maxxnino/tree-sitter-zig"))))
  :init
  ;; Compile language grammars; we cannot do this in `:config` because treesit may have loaded
  ;; in-built grammars by then and reloading them is currently (as of Emacs 30) not possible
  ;; without restarting Emacs.
  ;; Compile and install all of them.
  ;; See TODO at start of this top-level sexp.
  ;; (dolist (source treesit-language-source-alist)
  ;;    (treesit-install-language-grammar (car source)))
  :config
  ;; TODO: This automatically and unified with grammar definition source above.
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode)))

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
  :config
  ;; Clear commands eshell considers visual by default.
  (setq eshell-visual-commands '())
  (setq eat-minimum-latency 0.002)
  (add-hook 'eat-mode-hook #'disable-local-global-hl-line)
  (eat-eshell-mode))

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
  :hook (after-init . tab-bar-mode)
  :custom
  ;; By default the currently active buffer is also shown in a newly created tab, effectively bringing it into the new tabs local bufferlist. We share the default scratch buffer instead.
  ;; (tab-bar-new-tab-choice (lambda () (switch-to-buffer (generate-new-buffer-name "*local scratch*")))))
  (tab-bar-new-tab-choice "*scratch*"))

;; TODO: after-init hook when fixed.
(use-package tabspaces
  :ensure
  ;; :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-keymap-prefix "<f16> p")
  (tabspaces-default-tab "Default")
  :config
  (tabspaces-mode))

;; TODO: When minibuffer is completing directory/file-paths have backtick ` substituted to tilde ~ so I don't have to press shift-` to type ~ all the time.


;; TODO: Single-file config init.el

;;;; Consult

(use-package consult
  :ensure
  :bind
  ("C-x b" . consult-buffer) ; orig: switch-to-buffer
  )
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

;; Using as a general helper as modifier key real estate is limited.
(keymap-set global-map "<f16>" muh-map)
(keymap-set local-function-key-map "<f16>" 'event-apply-hyper-modifier)

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
;; (keymap-set special-event-map "<f16>" 'some-handler)
;; (event-convert-list '(hyper ?g))
;; (key-description [27])
;; Interesting, now double pressing f16 invokes f16 DEL
;; (keymap-set local-function-key-map "<f16>" 'some-handler)
;; (keymap-global-set "<f16> w" 'save-buffer)
;; ------------- end experimental stuff

;; TODO: Disable custom.el shit?

;(message (emacs-init-time))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; TODO: Move this elsewhere.

(defun tsujp/diff-hl-modus-faces ()
  (modus-themes-with-colors
    (custom-set-faces
     `(diff-hl-insert ((,c (:foreground ,green :background nil))))
     `(diff-hl-change ((,c (:foreground ,yellow-intense :background nil))))
     `(diff-hl-delete ((,c (:foreground ,red-intense :background nil)))))))

(use-package diff-hl
  :ensure
  :after modus-themes
  :config
  (define-fringe-bitmap 'tsujp/diff-hl-bitmap [224] nil nil '(center repeated))
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
