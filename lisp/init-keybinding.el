(use-package general
  :defer t
  :config
  (setq which-key-prefix-prefix "+" )
  (general-create-definer thief-leader-def
    :prefix "SPC"
    :keymaps 'normal
    "c" 'org-capture
    "d" 'xref-find-definitions
    "b" 'consult-buffer
    "f" 'consult-recent-file
    ":" 'execute-extended-command
    )
  (thief-leader-def
    ;;:keymaps 'org-mode-map
    :keymaps 'normal
    "o" '(:ignore t :wk "org")
    "oa" 'org-agenda)
  ;; (my-local-leader-def
  ;;   :keymaps 'evil-normal-state-map
  ;;   "c" 'org-capture
  ;;   )
  )
;;(define-key evil-normal-state-map (kbd "SPC") 'nil)
;;;(unbind-key (kbd "SPC"))
;; set local leader
(provide 'init-keybinding)
