;;; init-modules.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))


;; Adjust garbage collection thresholds during startup, and thereafter

(let ((normal-gc-cons-threshold (* 512 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; set some path for emacs.d clean
(setq eshell-directory-name (concat user-emacs-directory  ".cache/eshell/"))
(setq auto-save-list-file-prefix (concat user-emacs-directory  ".cache/auto-save-list/.saves-"))
(setq transient-levels-file (concat user-emacs-directory  ".cache/transient/levels.el"))
(setq transient-values-file (concat user-emacs-directory  ".cache/transient/values.el"))
(setq transient-history-file (concat user-emacs-directory  ".cache/transient/history.el"))
;; Bootstrap config
(setq custom-file (expand-file-name ".cache/custom.el" user-emacs-directory))

(with-temp-message ""
  (require 'init-utils)
  (require 'init-const)
  (require 'init-exec-path) ;; Set up $PATH
  ;; Allow users to provide an optional "init-preload-local.el"
  (require 'init-preload-local nil t)
  ;; Load configs for specific features and modes
  (require 'diminish)
  (require 'scratch)
  (require 'init-frame-hooks)
  (require 'init-themes)
  (require 'init-osx-keys)
  (require 'init-gui-frames)
  (require 'init-isearch)
  (require 'init-grep)
  (require 'init-scratch)
 ;;(require 'init-completion)
  (require 'init-corfu) ;; completion
  (require 'init-recentf)
  (require 'init-minibuffer)
  (require 'init-windows)
  (require 'init-sessions)

  ;; rust
  (require 'init-rust)

  (require 'init-editing-utils)
  (require 'init-whitespace)
  (require 'init-projectile)
  (require 'init-meow)
  (require 'init-compile)
 ;;;(require 'init-python)
  ;;;(require 'init-paredit)
  ;;;(require 'init-lisp)
  ;;;(require 'init-common-lisp)
  (when *spell-check-support-enabled*
    (require 'init-spelling))
  (require 'init-folding)
  (require 'init-workspaces)
  (require 'init-terminal)

  ;; Extra packages which don't require any configuration
  (when(require 'uptimes)
    (setq-default uptimes-keep-count 200)
    (add-hook 'after-init-hook (lambda () (require 'uptimes))))
  (when (fboundp 'global-eldoc-mode)
    (add-hook 'after-init-hook 'global-eldoc-mode))

  (require 'init-direnv)
  
  ;; Allow access from emacsclient
  (add-hook 'after-init-hook
            (lambda ()
              (require 'server)
              (unless (server-running-p)
                (server-start))))

  ;; Variables configured via the interactive 'customize' interface
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Locales (setting them earlier in this file doesn't work in X)
  (require 'init-locales)
  (require 'init-keybinding)
  ;; Allow users to provide an optional "init-local" containing personal settings
  (require 'init-local nil t)
  (flythief/setup-fonts)
  (global-visual-line-mode 1)
  )

(provide 'init-modules)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-modules.el ends here
