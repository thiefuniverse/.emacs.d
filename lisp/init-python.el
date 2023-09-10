;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq python-shell-interpreter "python3")
;;;(require-package 'pip-requirements)

;;;(use-package elpy
;;;  :requires (flycheck)
;;;  )
;;;(elpy-enable)
;;;(add-hook 'elpy-mode-hook 'flycheck-mode)
;;;(add-hook 'elpy-mode-hook (lambda ()
;;;                              (add-hook 'before-save-hook
;;;                                        'elpy-format-code nil t)))
(provide 'init-python)
;;; init-python.el ends here
