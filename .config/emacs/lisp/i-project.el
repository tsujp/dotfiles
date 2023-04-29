;;; -*- lexical-binding: t -*-

;;; Project configuration and management.
;;; This somewhat ties into our use of Eglot since without being able to
;;;   accurately detect the root of a project (including no project at all i.e.
;;;   just the current file) the wrong working directory will be given to the
;;;   LSP server which may then error out or attempt to index too many things.
;;;
;;; docs: https://docs.projectile.mx/projectile
;;; docs: https://github.com/bbatsov/projectile

;;; Configuration

;;; Projectile
(use-package projectile
  :init
  ;; (setq projectile-ignored-projects '("~"))
  :config
  (projectile-mode 1)
  ;; Custom Ruby project type to match that which I typically define. This is
  ;;   for non-packageable projects (i.e. not gems).
  (projectile-register-project-type 'ruby-generic '("Gemfile" "src" "vendor")
                                    :project-file "Gemfile"
                                    :src-dir "src/"
                                    :run "bundle exec rake")
  ;; Jekyll Ruby project.
  (projectile-register-project-type 'ruby-generic
                                    '("Gemfile" "src" "build" "jekyll.toml" "vendor")
                                    :project-file "Gemfile"
                                    :src-dir "src/"
                                    :compile "./do build"
                                    :run "bundle exec bake")
  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(provide 'i-project)
