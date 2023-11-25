(defvar jump-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "l" 'consult-line)
    (define-key keymap "w" 'avy-goto-word-1)
    (define-key keymap "f" 'switch-window)
    (define-key keymap "k" 'consult-buffer)
    (define-key keymap "j" 'consult-project-buffer)
    keymap))

(defalias 'jump jump-quick-keymap)
(global-set-key (kbd "C-c j") 'jump)

(global-set-key (kbd "C-c ;") 'execute-extended-command)
(global-set-key (kbd "C-c P") 'edit-config-file)
(global-set-key (kbd "C-c `") 'jump-scratch)
(global-set-key (kbd "C-c k") 'delete-other-windows)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c \\") 'split-window-right)
(global-set-key (kbd "C-c t") 'ff-find-other-file)
(global-set-key (kbd "C-c o") 'switch-window)
(global-set-key (kbd "C-c r") 'rg)
(global-set-key (kbd "C-c e") 'eval-last-sexp)


(global-set-key (kbd "C-c d") 'lsp-bridge-find-def)
(global-set-key (kbd "C-c ,") 'lsp-bridge-find-def-return)



(defvar file-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "r" 'consult-recent-file)
    (define-key keymap "d" 'delete-window)
    (define-key keymap "k" 'kill-current-buffer)
    keymap))
(defalias 'file file-quick-keymap)
(global-set-key (kbd "C-c f") 'file)

(global-unset-key (kbd "C-c C-f"))
(defvar describe-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "k" 'describe-key)
    (define-key keymap "f" 'describe-function)
    (define-key keymap "v" 'describe-variable)
    keymap))
(defalias 'des describe-quick-keymap)
(global-set-key (kbd "C-c h") 'des)

(meow-define-keys
    'insert
  '("C-k" . backward-delete-char)
  '("C-j" . avy-goto-word-1)
  )

(meow-define-keys
    'normal
  '("P" . move-dup-duplicate-down)
  )

(define-key minibuffer-local-map (kbd "C-;") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-k") 'backward-delete-char)
(define-key minibuffer-local-map (kbd "C-SPC") 'vertico-exit)

;;; set local leader
(provide 'init-keybinding)
