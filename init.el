;;; now, I manage emacs confg without package management.

(require 'cl-lib)

(tool-bar-mode -1)                      ;禁用工具栏
(menu-bar-mode -1)                      ;禁用菜单栏
(scroll-bar-mode -1)                    ;禁用滚动条

(setq native-comp-deferred-compilation t)
(setq native-comp-speed 3)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

          ;; 注意：`add-to-list' 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path)))))

(let ((normal-gc-cons-threshold (* 512 1024 1024))
      (init-gc-cons-threshold (* 512 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Bootstrap config
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-utils)
(require 'init-const)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el


;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH


;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

;; Load configs for specific features and modes
(require-package 'use-package)
(setq use-package-always-ensure t)

(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
;;;(require 'init-ibuffer)
(require 'init-scratch)
(require 'init-corfu) ;; completion
(require 'init-recentf)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-windows)
(require 'init-sessions)
;;;(require 'init-mmm)

(require 'init-editing-utils)
(require 'init-whitespace)

(require 'init-vc)
(require 'init-git)
(require 'init-github)
(require 'init-projectile)
(require 'init-evil)
(require 'init-compile)
(require 'init-crontab)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-org)
(require 'init-html)
(require 'init-css)
(require 'init-http)
;;;(require 'init-python)
(require 'init-sql)
(require 'init-toml)
(require 'init-yaml)

(require 'init-paredit)
(require 'init-lisp)
(require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)

(require 'init-folding)
(require 'init-dash)
;; Extra packages which don't require any configuration

(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

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

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
