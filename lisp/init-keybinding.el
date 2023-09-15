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
(global-set-key (kbd "C-c x") 'jump-scratch)
(global-set-key (kbd "C-c k") 'delete-other-windows)
(global-set-key (kbd "C-c ,") 'pop-tag-mark)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c \\") 'split-window-right)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c t") 'switch-between-cpp-h)
(global-set-key (kbd "C-c o") 'switch-window)
(global-set-key (kbd "C-c r") 'rg)
(global-set-key (kbd "C-c e") 'eval-last-sexp)

(defvar file-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "r" 'consult-recent-file)
    (define-key keymap "d" 'delete-window)
    (define-key keymap "k" 'kill-current-buffer)
    keymap))
(defalias 'file file-quick-keymap)
(global-set-key (kbd "C-c f") 'file)

(defvar describe-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "k" 'describe-key)
    (define-key keymap "f" 'describe-function)
    (define-key keymap "v" 'describe-variable)
    keymap))
(defalias 'describe describe-quick-keymap)
(global-set-key (kbd "C-c h") 'describe)

(meow-define-keys
    'insert
  '("C-k" . backward-delete-char)
  '("C-j" . avy-goto-word-1)
  )

(define-key minibuffer-local-map (kbd "C-;") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-k") 'backward-delete-char)
(define-key minibuffer-local-map (kbd "C-SPC") 'vertico-exit)

;;; set local leader
(provide 'init-keybinding)
