(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)

(require 'lsp-bridge)
(setq lsp-bridge-python-command (exec-path-from-shell-getenv "EMACS_PYTHON"))

(defun lsp-bridge-find-def ()
  (interactive)
  (cond
   ((acm-is-elisp-mode-p)
    (xref-find-definitions (thing-at-point 'symbol t))
    )
   (t
    (setq-local lsp-bridge-jump-to-def-in-other-window nil)
    (lsp-bridge-call-file-api "find_define" (lsp-bridge--position)))))

(defun lsp-bridge-find-def-return ()
  "Pop off lsp-bridge-mark-ring and jump to the top location."
  (interactive)
  (if (acm-is-elisp-mode-p)
      (pop-tag-mark))
  ;; Pop entries that refer to non-existent buffers.
  (while (and lsp-bridge-mark-ring (not (marker-buffer (car lsp-bridge-mark-ring))))
    (setq-local lsp-bridge-mark-ring (cdr lsp-bridge-mark-ring)))
  (or lsp-bridge-mark-ring
      (error "[LSP-Bridge] No lsp-bridge mark set"))
  (let* ((this-buffer (current-buffer))
         (marker (pop lsp-bridge-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
          (error "[LSP-Bridge] mark position is outside accessible part of buffer %s"
                 (buffer-name buffer))))
    (goto-char position)
    (unless (equal buffer this-buffer)
      (switch-to-buffer buffer))
    (recenter)
    ))
(defun search-file-upward (filename)
  "在当前目录及其父目录中逐层向上搜索指定文件名"
  (let ((dir (expand-file-name default-directory))
        (root (expand-file-name "/")))
    (while (and (not (string= dir root))
                (not (file-exists-p (expand-file-name filename dir))))
      (setq dir (expand-file-name (concat dir "../"))))
    (if (string= dir root)
        nil
      (expand-file-name filename dir))))


;; (defun switch-between-cpp-h ()
;;   "switch cpp from header by lsp"
;;   (interactive)
;;   (let* ((name (buffer-name))
;;          (file-suffix (car (last (split-string name "\\." t) )))
;;          (file-name (first (split-string name (concat "." file-suffix "$") t))))
;;     (if (or (string-equal file-suffix "cpp") (string-equal file-suffix "c"))
;;         (find-file (concat file-name ".hpp")))
;;     (message "warning: only work on cpp/h file"))
;;   )
;; (first (split-string "t.mm.cpp.cpp" ".cpp$" t) )
;; (split-string  (buffer-name) (concat "." "el" "$") t)
(require 'fzf)

;;; (setq lsp-bridge-enable-auto-format-code nil)
(global-lsp-bridge-mode)

(provide 'init-completion)
