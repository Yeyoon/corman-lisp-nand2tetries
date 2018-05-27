;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

 (add-to-list 'load-path user-emacs-directory)
 (require 'init-benchmarking) ;; Measure startup time

 ;;----------------------------------------------------------------------------
 ;; Which functionality to enable (use t or nil for true and false)
 ;;----------------------------------------------------------------------------
 (defconst *spell-check-support-enabled* nil)
 (defconst *is-a-mac* (eq system-type 'darwin))
 (defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))

; ;;----------------------------------------------------------------------------
; ;; Bootstrap config
; ;;----------------------------------------------------------------------------
 (require 'init-compat)
 (require 'init-utils)
 (require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
 (require 'init-elpa)      ;; Machinery for installing required packages
 (require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(setf package-check-signature nil)

(require-package 'wgrep)
;(require-package 'project-local-variables)
;;(require-package 'diminish)
(require-package 'scratch)
;;(require-package 'mwe-log-commands)

(require 'init-frame-hooks)
;;(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
;;(require 'init-maxframe)
;;(require 'init-proxies)
;(require 'init-dired)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-recentf)
;(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
;(require 'init-sessions)
;;(require 'init-fonts)
(require 'init-mmm)
;;(require 'init-growl)

; (require 'init-editing-utils)

; ;;(require 'init-darcs)
; (require 'init-git)

; (require 'init-crontab)
; ;;(require 'init-textile)
; (require 'init-markdown)
; (require 'init-csv)
; ;;(require 'init-erlang)
; (require 'init-javascript)
; ;;(require 'init-w3m)
; (require 'init-php)
; ;;(require 'init-multi-web-mode)
; (require 'init-org)
; (require 'init-nxml)
; (require 'init-css)
; (require 'init-haml)
; ;;(require 'init-python-mode)
; (require 'init-haskell)
; (require 'init-ruby-mode)
; (require 'init-rails)
; (require 'init-sql)

 ;(require 'init-paredit)
 ;(require 'init-lisp)
 ;(require 'init-slime)
 ;;(require 'init-clojure)
 ;; add slime
 (setq inferior-lisp-program "C:/sbcl/sbcl.exe")
 (add-to-list 'load-path "C:/Users/Moment/AppData/Roaming/slime-master/")
 (require 'slime)
 (slime-setup)
 (require 'slime-autoloads)
 (slime-setup '(slime-fancy))

 ;; add color-theme
 (add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
 (require 'color-theme)
 (color-theme-initialize)
 (color-theme-empty-void)

 ;(require 'init-common-lisp)

; (when *spell-check-support-enabled*
  ; (require 'init-spelling))

; (require 'init-marmalade)
 (require 'init-misc)

 ;; Extra packages which don't require any configuration

 ;;(require-package 'gnuplot)
 ;;(require-package 'lua-mode)
 (require-package 'htmlize)
 ;;(require-package 'dsvn)
 ;; (when *is-a-mac*
 ;;   (require-package 'osx-location))
 (require-package 'regex-tool)

; ;;----------------------------------------------------------------------------
; ;; Allow access from emacsclient
; ;;----------------------------------------------------------------------------
; (require 'server)
; (unless (server-running-p)
  ; (server-start))


; ;;----------------------------------------------------------------------------
; ;; Variables configured via the interactive 'customize' interface
; ;;----------------------------------------------------------------------------
 (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
 (when (file-exists-p custom-file)
   (load custom-file))


 ;;----------------------------------------------------------------------------
 ;; Allow users to provide an optional "init-local" containing personal settings
 ;;----------------------------------------------------------------------------
 (require 'init-local nil t)


 ;;----------------------------------------------------------------------------
 ;; Locales (setting them earlier in this file doesn't work in X)
 ;;----------------------------------------------------------------------------
 (require 'init-locales)
 (require 'init-keybindings)
 ;;(require 'init-google-pkgs)
 ;;(require 'init-auctex)
 (require 'init-abbrev)

 ;; multimedia
 ;;(require 'init-douban)

 ;(require 'init-look)
 ;(require 'init-yasnippet)

 (require 'init-cedet)

(message "init completed in %.2fms"
         (sanityinc/time-subtract-millis (current-time) before-init-time))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
