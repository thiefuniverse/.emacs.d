(meow-define-keys
    ;; state
    'normal
  ;; bind to a command
  '("a" . meow-append)
  '("d" . xref-find-definitions)
  ;; bind to a keymap
                                        ;
  )

(require 'general)
(setq which-key-prefix-prefix "+" )
(general-create-definer thief-leader-def
                        :prefix "SPC"
                        :keymaps 'normal

                        ";" 'execute-extended-command
                        "P" 'edit-config-file
                        "x" 'jump-scratch
                        "k" 'delete-other-windows
                        "," 'pop-tag-mark
                        "-" 'split-window-below
                        "\\" 'split-window-right
                        "g" 'goto-line
                        "t" 'switch-between-cpp-h
                        "o" 'switch-window
                        "r" 'rg
                        )
(thief-leader-def
 :keymaps 'normal
 "j" '(:ignore t :wk "jump")
 "jl" 'consult-line
 "jw" 'avy-goto-word-1
 "jf" 'switch-window
 "jk" 'consult-buffer
 "jj" 'consult-project-buffer
 )
(thief-leader-def
  :keymaps 'normal
  "f" '(:ignore t :wk "file")
  "fr" 'consult-recent-file
  "fd" 'delete-window
  "fk" 'kill-current-buffer
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
 "o" '(:ignore t :wk "org")
 )
(thief-leader-def
 :keymaps 'normal
 "b" '(:ignore t :wk "buffer")
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
 "c" '(:ignore t :wk "consult")
 "cy" 'consult-yank-from-kill-ring
 )
(thief-leader-def
 :keymaps 'normal
 "s" '(:ignore t :wk "save")
 "sd" 'save-buffer
 )

;;; modify evil mode actions
(define-key evil-normal-state-map (kbd "e") 'eval-last-sexp)
(define-key evil-normal-state-map (kbd "P") 'move-dup-duplicate-down)

(define-key evil-motion-state-map (kbd "t") 'evil-jump-item)
(define-key evil-motion-state-map (kbd "C-;") 'evil-end-of-line)
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)

(define-key evil-insert-state-map (kbd "C-k") 'backward-delete-char)
(define-key evil-insert-state-map (kbd "C-d") 'delete-forward-char)
(define-key evil-insert-state-map (kbd "C-j") 'avy-goto-word-1)

(define-key minibuffer-local-map (kbd "C-;") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-k") 'backward-delete-char)
(define-key minibuffer-local-map (kbd "C-SPC") 'vertico-exit)

;;; set local leader
(provide 'init-keybinding)
