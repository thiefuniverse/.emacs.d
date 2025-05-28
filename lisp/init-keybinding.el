(defvar window-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "u") #'winner-undo)
    (define-key keymap (kbd "r") #'winner-redo)
    (define-key keymap (kbd "c") #'delete-window)
    keymap))
;; define an alias for your keymap
(defalias 'window window-keymap)

(defvar jump-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "l") #'consult-line)
    (define-key keymap (kbd "w") #'avy-goto-word-1)
    (define-key keymap (kbd "k") #'consult-buffer)
    (define-key keymap (kbd "l") #'consult-imenu)
    (define-key keymap (kbd "j") #'consult-project-buffer)
    keymap))
; define an alias for your keymap
(defalias 'jump jump-keymap)

(defvar file-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "r") #'consult-recent-file)
    (define-key keymap (kbd "k") #'kill-current-buffer)
    (define-key keymap (kbd "c") #'consult-yank-from-kill-ring)
    (define-key keymap (kbd "s") #'save-buffer)
    keymap))
; define an alias for your keymap
(defalias 'file file-keymap)

(defvar project-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "x") #'jump-project-scratch)
    (define-key keymap (kbd "f") #'projectile-find-file)
    (define-key keymap (kbd "b") #'consult-project-buffer)
    (define-key keymap (kbd "s") #'project-shell)
    (define-key keymap (kbd "p") #'projectile-switch-project)
    (define-key keymap (kbd "r") #'projectile-ripgrep)
    (define-key keymap (kbd "g") #'rg)
    keymap))
; define an alias for your keymap
(defalias 'project project-keymap)

(defvar workspace-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "n" '+workspace/new)
    (define-key keymap "d" '+workspace/delete)
    (define-key keymap "j" '+workspace/switch-to)
    (define-key keymap "r" '+workspace/rename)
    (define-key keymap "s" '+workspace/cycle)
    (define-key keymap "l" '+workspace/display)
    keymap))
(defalias 'workspace workspace-quick-keymap)

(defun mode-specific-run ()
  (interactive)
  (cond
   ((eq major-mode 'rust-mode)
    (if (fboundp 'rust-run)
        (rust-run)
      (user-error "can't find rust-run function")))
   ((eq major-mode 'emacs-lisp-mode)
    (eval-buffer))
   (t
    (message "no mode config" major-mode))))

(meow-leader-define-key
'("a" . centaur-tabs-ace-jump)
'("d" . xref-find-definitions)
'(";" . execute-extended-command)
'("P" . edit-config-file)
'("x" . jump-scratch)
'("k" . delete-other-windows)
'("," . pop-tag-mark)
'("-" . split-window-below)
'("\\" . split-window-right)
'("g" . goto-line)
'("t" . switch-between-cpp-h)
'("o" . switch-window)
'("e" . eval-last-sexp)
'("j" . jump)
'("f" . file)
'("h" . help)
'("p" . project)
'("s" . workspace)
'("r" . mode-specific-run))

(global-unset-key (kbd "C-c C-f"))
;;; disable emacs-FAQ keymap
(global-unset-key (kbd "C-h C-f"))
(global-set-key (kbd "C-h f") 'describe-function)
;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(meow-define-keys
    'insert
  '("C-k" . backward-delete-char)
  '("C-j" . avy-goto-word-1)
  )
;;; modify evil mode actions
(define-key minibuffer-local-map (kbd "C-;") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-k") 'backward-delete-char)
(define-key minibuffer-local-map (kbd "M-k") 'backward-delete-word)
(define-key minibuffer-local-map (kbd "C-l") 'vertico-exit)


;;; set local leader
(provide 'init-keybinding)
