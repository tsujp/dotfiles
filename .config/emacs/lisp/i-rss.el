;;; -*- lexical-binding: t -*-

;;; RSS feed reading

;; TODO: Tiny Tiny RSS: https://codingquark.com/emacs/2020/04/19/elfeed-protocol-ttrss.html
;; TODO: Autotagging.
;; TODO: Elfeed feed defintion using Org Mode since that has auto tagging and looks
;;       to be nicer to maintain? See what Tiny Riny RSS provides first.
;;       Source: https://lucidmanager.org/productivity/read-rss-feeds-with-emacs-and-elfeed/

;;; Blog feed list
(setq blog-rss-feeds
      (list
       ;; People / Companies.
       "https://fly.io/blog/feed.xml" ; Fly.io general blog.
       "https://fly.io/ruby-dispatch/feed.xml" ; Fly.io Ruby.
       "https://drewdevault.com/blog/index.xml" ; Drew DeVault.
       ; TODO: justine.lol (no RSS feed it looks like) ; Justine Tunney.
       "https://xeiaso.net/blog.rss" ; Xe Iaso.
       "https://trofi.github.io/feed/rss.xml" ; Sergei Trofimovich -- Wine, C, Linux.
       "https://werat.dev/index.xml" ; Andy Hippo -- Wine, low level, debugging.
       "https://hxslr.com/blog/feed.xml" ; Reverse engineering, security.
       ; TODO: https://nickjanetakis.com/blog/ (no RSS feed ugh) ; Nick Janetakis.
       "https://aras-p.info/atom.xml" ; Aras Pranckevičius.
       "https://jvns.ca/atom.xml" ; Julia Evans.
       "https://muffinman.io/atom.xml" ; Stanko Tadić.
       "https://ciechanow.ski/atom.xml" ; Bartosz Ciechanowski -- Very cool general explanatory posts.
       "https://www.yieldcode.blog/rss/feed.xml" ; Dmitry Kudryavtsev.
       "https://blog.orhun.dev/rss.xml" ; Orhun Parmaksız -- Zig, Linux, Rust.
       "https://en.liujiacai.net/index.xml" ; Jiacai Liu -- Zig, Emacs.
       "https://matklad.github.io/feed.xml" ; -Alex Kladov -- Zig, Rust, Deno.
       ;; Communities.
       ;;; Programming languages.
       ;;;; Zig.
       "https://zig.news/feed.rss" ; Zig News.
       "https://zigmonthly.org/index.xml" ; Zig Monthly.
       "https://buttondown.email/ZigSHOWTIME/rss" ; Zig SHOWTIME.
       ;;;; Ruby.
       "https://cprss.s3.amazonaws.com/rubyweekly.com.xml" ; Ruby weekly.
       ; TODO: https://newsletter.shortruby.com/ (no RSS feed?) ; Short Ruby Newsletter.
       ))

;;; Video feed list
;; (setq video-rss-feed)
; TODO: Zig Showtime youtube feed.

;;; Configuration
(use-package elfeed
  :config
  (global-set-key (kbd "C-x w") 'elfeed)
  (setq elfeed-feeds blog-rss-feeds))

(provide 'i-rss)
