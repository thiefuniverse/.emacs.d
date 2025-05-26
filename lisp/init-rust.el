(require 'rust-mode)
(setq rust-mode-treesitter-derive t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq rust-format-on-save t)

(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)
            (prettify-symbols-mode)))

(provide 'init-rust)
