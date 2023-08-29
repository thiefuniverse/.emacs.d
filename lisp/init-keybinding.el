(use-package general
  :defer t
  :config
  (setq which-key-prefix-prefix "+" )
  (general-create-definer thief-leader-def
    :prefix "SPC"
    :keymaps 'normal
    "d" 'xref-find-definitions
    ";" 'execute-extended-command
    "P" 'edit-config-file
    "x" 'jump-scratch
    "k" 'delete-other-windows
    )
  (thief-leader-def
    :keymaps 'normal
    "f" '(:ignore t :wk "file")
    "fr" 'consult-recent-file
    "fd" 'delete-frame
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
    "pb" 'consult-project-buffer
    "ps" 'project-shell
    )
  (thief-leader-def
    :keymaps 'normal
    "h" '(:ignore t :wk "help")
    "hk" 'describe-key
    "hf" 'describe-function
    "hv" 'describe-variable
    )
  (thief-leader-def
    :keymaps 'normal
    "c" '(:ignore t :wk "consult")
    "cl" 'consult-line
    "ci" 'consult-imenu
    "cm" 'consult-imenu-multi
    "cy" 'consult-yank-from-kill-ring
    )
  (thief-leader-def
    :keymaps 'normal
    "s" '(:ignore t :wk "save")
    "sd" 'save-buffer
    )
  (thief-leader-def
    :keymaps 'normal
    "j" '(:ignore t :wk "jump")
    "jc" 'avy-goto-char
    "jl" 'avy-goto-line
    "jw" 'avy-goto-word-0
    )
  )

;;; modify evil mode actions
(define-key evil-normal-state-map (kbd "e") 'sanityinc/eval-last-sexp-or-region)
(define-key evil-normal-state-map (kbd "P") 'move-dup-duplicate-down)
(define-key evil-motion-state-map (kbd "t") 'evil-jump-item)
(define-key evil-motion-state-map (kbd "C-;") 'evil-end-of-line)


(define-key minibuffer-local-map (kbd "C-;") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-k") 'backward-delete-char)
;; set local leader
(provide 'init-keybinding)
