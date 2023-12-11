(defvar jump-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "l" 'consult-line)
    (define-key keymap "w" 'avy-goto-word-1)
    (define-key keymap "f" 'switch-window)
    (define-key keymap "k" 'consult-buffer)
    keymap))

(defalias 'jump jump-quick-keymap)
(global-set-key (kbd "C-c j") 'jump)

(global-set-key (kbd "C-c ;") 'execute-extended-command)
(global-set-key (kbd "C-c P") 'edit-config-file)
(global-set-key (kbd "C-c `") 'jump-scratch)
(global-set-key (kbd "C-c -") 'split-window-below)
(global-set-key (kbd "C-c \\") 'split-window-right)
(global-set-key (kbd "C-c t") 'projectile-find-other-file)
(global-set-key (kbd "C-c o") 'switch-window)
(global-set-key (kbd "C-c e") 'eval-last-sexp)
(global-set-key (kbd "C-c p") 'projectile-switch-project)
;;;(global-set-key (kbd "C-c a") 'dirvish-fd)
(global-set-key (kbd "C-c s") 'consult-project-buffer)
(global-set-key (kbd "C-c d") 'lsp-bridge-find-def)
(global-set-key (kbd "C-c ,") 'lsp-bridge-find-def-return)

(defcustom quick-close-buffer-list '("*Help*") "define close window name")
(defun delete-other-or-popup-window ()
  "Close windows containing specific buffer names from the list."
  (interactive)
  (let ((has-quick-close-window nil))
    (walk-windows
     (lambda (w)
       (let ((buf (window-buffer w)))
         ;; Check if the buffer's name is in the list of buffers to close
         (when (and (buffer-live-p buf)
                    (member (buffer-name buf) quick-close-buffer-list))
           (progn (kill-buffer buf)
                  (setq has-quick-close-window t))))))
    (if (not has-quick-close-window)
        (delete-other-window))))
(global-set-key (kbd "C-c k") 'delete-other-window)

(defvar search-rg-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "r" 'projectile-ripgrep)
    (define-key keymap "g" 'rg)
    keymap))
(defalias 'search-rg search-rg-quick-keymap)
(global-set-key (kbd "C-c r") 'search-rg)

(defvar workspace-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "n" '+workspace/new)
    (define-key keymap "d" '+workspace/delete)
    (define-key keymap "w" '+workspace/switch-to)
    (define-key keymap "r" '+workspace/rename)
    (define-key keymap "c" '+workspace/cycle)
    (define-key keymap "l" '+workspace/display)
    (define-key keymap "1" '+workspace/switch-to-0)
    (define-key keymap "2" '+workspace/switch-to-1)
    (define-key keymap "3" '+workspace/switch-to-2)
    (define-key keymap "4" '+workspace/switch-to-3)
    (define-key keymap "5" '+workspace/switch-to-4)
    keymap))
(defalias 'workspace workspace-quick-keymap)
(global-set-key (kbd "C-c w") 'workspace)

;;;define action when switch to another project,switch to buffer if has, or open
;;; file
(defun projectile-switch-to-one-buffer-if-has ()
  "Switch to a project buffer."
  (let ((bufs (projectile-project-buffers)))
    (if (> (length bufs) 0)
        (switch-to-buffer (car bufs))
      (projectile-find-file))))
(setq projectile-switch-project-action 'projectile-switch-to-one-buffer-if-has)

(defvar file-quick-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap "r" 'consult-recent-file)
    (define-key keymap "d" 'delete-window)
    (define-key keymap "k" 'kill-current-buffer)
    (define-key keymap "j" 'projectile-find-file)
    (define-key keymap "i" 'ido-dired)
    keymap))
(defalias 'file file-quick-keymap)
(global-set-key (kbd "C-c f") 'file)


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

(meow-define-keys
    'normal
  '("P" . move-dup-duplicate-down)
  )

(define-key minibuffer-local-map (kbd "C-;") 'minibuffer-keyboard-quit)
(define-key minibuffer-local-map (kbd "C-k") 'backward-delete-char)
(define-key minibuffer-local-map (kbd "M-k") 'backward-kill-word)
(define-key minibuffer-local-map (kbd "C-l") 'vertico-exit)


;;; set local leader
(provide 'init-keybinding)
