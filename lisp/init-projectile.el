;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dirvish)
(require 'dirvish-side)
(require 'dirvish-icons)
(require 'dirvish-ls)
(require 'dirvish-fd)
(require 'dirvish-history)
(dirvish-override-dired-mode)
(dirvish-side-follow-mode)

(when(require 'projectile)
  (add-hook 'after-init-hook 'projectile-mode)
  (setq projectile-cache-file (expand-file-name ".cache/projectile.cache"
                                                user-emacs-directory)
        projectile-known-projects-file (expand-file-name ".cache/projectile-known-projects.eld"
                                                         user-emacs-directory))
  ;; Shorter modeline
  (setq-default projectile-mode-line-prefix " Proj")

  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))

  (with-eval-after-load 'projectile
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

  (require 'ibuffer-projectile))


(provide 'init-projectile)
;;; init-projectile.el ends here
