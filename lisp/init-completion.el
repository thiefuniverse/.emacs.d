(add-to-list 'load-path (expand-file-name "lisp/lsp-bridge" user-emacs-directory))

;;;(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))
;;;(use-package posframe)
;;;(use-package all-the-icons)
(require 'lsp-bridge)
(setq lsp-bridge-python-command "/home/xf/soft/py/bin/python")
(global-lsp-bridge-mode)

;; (require 'lsp-bridge)
;; (require 'lsp-bridge-icon)
;; (require 'lsp-bridge-orderless)
;; For corfu users:

;;;(global-lsp-bridge-mode)
;;;(add-hook 'lsp-bridge-mode-hook (lambda ()
;;;                                  (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))


(provide 'init-completion)
