(use-package general
  :defer t
  :config
  (setq which-key-prefix-prefix "+" )
  (general-create-definer thief-leader-def
    :prefix "SPC"
    :keymaps 'normal
    "d" 'xref-find-definitions
    "f" 'consult-recent-file
    ":" 'execute-extended-command
    "P" 'edit-config-file
    "x" 'jump-scratch
    "k" 'delete-other-windows
    )
  (thief-leader-def
    :keymaps 'normal
    "o" '(:ignore t :wk "org")
    "oa" 'org-agenda
    "oc" 'org-capture
    )
  (thief-leader-def
    :keymaps 'normal
    "b" '(:ignore t :wk "buffer")
    "bb" 'consult-buffer
    "bk" 'kill-current-buffer
    )
  (thief-leader-def
    :keymaps 'normal
    "p" '(:ignore t :wk "project")
    "px" 'jump-project-scratch
    "pf" 'projectile-find-file
    )
  (thief-leader-def
    :keymaps 'normal
    "h" '(:ignore t :wk "help")
    "hk" 'describe-key
    "hf" 'describe-function
    )
  (thief-leader-def
    :keymaps 'normal
    "c" '(:ignore t :wk "consult")
    "cl" 'consult-line
    )
  (thief-leader-def
    :keymaps 'normal
    "s" '(:ignore t :wk "save")
    "sd" 'save-buffer
    )
  )
(define-key evil-normal-state-map (kbd "e") 'sanityinc/eval-last-sexp-or-region)
;; set local leader
(provide 'init-keybinding)
