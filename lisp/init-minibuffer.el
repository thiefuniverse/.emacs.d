;;; init-minibuffer.el --- Config for minibuffer completion       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(when(require 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

 (require 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless))

  (defun sanityinc/use-orderless-in-minibuffer ()
    (setq-local completion-styles '(substring orderless)))
  (add-hook 'minibuffer-setup-hook 'sanityinc/use-orderless-in-minibuffer)

  ;; (when(require 'embark)
  ;;   (with-eval-after-load 'vertico
  ;;     (define-key vertico-map (kbd "C-c C-o") 'embark-export)
  ;;     (define-key vertico-map (kbd "C-c C-c") 'embark-act)))
  (require 'consult-xref)
  (when(require 'consult)
    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key 'any)))

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark)

    (when(require 'projectile)
      (setq-default consult-project-root-function 'projectile-project-root))

    (when (and (executable-find "rg")(require 'affe))
      (defun sanityinc/affe-grep-at-point (&optional dir initial)
        (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                        (symbol-name s))))
        (affe-grep dir initial))
      (global-set-key (kbd "M-?") 'sanityinc/affe-grep-at-point)
      (sanityinc/no-consult-preview sanityinc/affe-grep-at-point)
      (with-eval-after-load 'affe (sanityinc/no-consult-preview affe-grep)))

    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)



    ;; (when(require 'embark-consult)
    ;;   (with-eval-after-load 'embark
    ;;     (require 'embark-consult)
    ;;     (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))

   (require 'consult-flycheck)))

(when(require 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
