(use-package evil
  :ensure t
  :init
  :config
  (evil-mode 1)
  )

(use-package evil-escape
  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2)
  (evil-escape-mode))

;;; for corfu move by C-n and C-p
(define-key evil-insert-state-map "\C-n" nil)
(define-key evil-insert-state-map "\C-p" nil)


(provide 'init-evil)
