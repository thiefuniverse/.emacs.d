;;; init-windows.el --- Working with windows within frames -*- lexical-binding: t -*-
;;; Commentary:

;; This is not about the "Windows" OS, but rather Emacs's "windows"
;; concept: these are the panels within an Emacs frame which contain
;; buffers.

;;; Code:

;; Navigate window layouts with "C-c <left>" and "C-c <right>"

(add-hook 'after-init-hook 'winner-mode)


;; Make "C-x o" prompt for a target window when there are more than 2
(require 'switch-window)
(setq-default switch-window-shortcut-style 'qwerty)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)


;; When splitting window, show (other-buffer) in the new window

(defun split-window-func-with-other-buffer (split-function)
  (lambda (&optional arg)
    "Split this window and switch to the new window unless ARG is provided."
    (interactive "P")
    (funcall split-function)
    (let ((target-window (next-window)))
      (set-window-buffer target-window (other-buffer))
      (unless arg
        (select-window target-window)))))

(global-set-key (kbd "C-x 2") (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key (kbd "C-x 3") (split-window-func-with-other-buffer 'split-window-horizontally))

(defun sanityinc/toggle-delete-other-windows ()
  "Delete other windows in frame if any, or restore previous window config."
  (interactive)
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(global-set-key (kbd "C-x 1") 'sanityinc/toggle-delete-other-windows)


;; Rearrange split windows

(defun split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(global-set-key (kbd "C-x |") 'split-window-horizontally-instead)
(global-set-key (kbd "C-x _") 'split-window-vertically-instead)


;; Borrowed from http://postmomentum.ch/blog/201304/blog-on-emacs

(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)))

(global-set-key (kbd "<f7>") 'sanityinc/split-window)




(defun sanityinc/toggle-current-window-dedication ()
  "Toggle whether the current window is dedicated to its current buffer."
  (interactive)
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not was-dedicated))
    (message "Window %sdedicated to %s"
             (if was-dedicated "no longer " "")
             (buffer-name))))

(global-set-key (kbd "C-c <down>") 'sanityinc/toggle-current-window-dedication)


(unless (memq window-system '(nt w32))
  (require 'windswap)
  (add-hook 'after-init-hook (apply-partially 'windmove-default-keybindings 'control))
  (add-hook 'after-init-hook (apply-partially 'windswap-default-keybindings 'shift 'control)))

;;; control window pop behavior
(require 'shackle)
(setq shackle-rules
      ;; CONDITION(:regexp)            :select     :inhibit-window-quit   :size+:align|:other     :same|:popup
      '((compilation-mode              :select nil                                               )
        ("*undo-tree*"                                                    :size 0.25 :align right)
        ("*eshell*"                    :select t                          :other t               )
        ("*Shell Command Output*"      :select nil                                               )
        ("\\*Async Shell.*\\*" :regexp t :ignore t                                                 )
        (occur-mode                    :select nil                                   :align t    )
        ("*Help*"                     :select t   :inhibit-window-quit nil)
        ("*Completions*"                                                  :size 0.3  :align t    )
        ("*Messages*"                  :select nil :inhibit-window-quit t :other t               )
        ("*Calendar*"                  :select t                          :size 0.3  :align below)
        ("*info*"                      :select t   :inhibit-window-quit t                         :same t)
        ))
(setq shackle-select-reused-windows t)
(shackle-mode)


(require 'centaur-tabs)
(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons nil)
(setq centaur-tabs-set-bar 'over)
(setq centaur-tabs-set-close-button nil)
(setq centaur-tabs-style "bar"
      centaur-tabs-height 32
      centaur-tabs-set-icons nil
      centaur-tabs-show-new-tab-button t
      centaur-tabs-set-modified-marker t
      centaur-tabs-show-navigation-buttons nil
      centaur-tabs-set-bar 'under
      centaur-tabs-show-count nil
      ;; centaur-tabs-label-fixed-length 15
      ;; centaur-tabs-gray-out-icons 'buffer
      ;; centaur-tabs-plain-icons t
      x-underline-at-descent-line t
      centaur-tabs-left-edge-margin nil)
(defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     (string= "dired-mode" (buffer-local-value 'major-mode x))
     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-suffix-p ".el.gz" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
          (not (file-name-extension name)))
     )))

(setq centaur-tabs-ace-jump-keys
      '(?a ?s ?d ?f ?j ?k ?l ?g ?h))
(setq centaur-tabs-ace-jump-dim-buffer nil)
(centaur-tabs-mode)
(centaur-tabs-group-by-projectile-project)


(defun my-projectile-in-submodule-p ()
  "Check if the current project is a submodule of another project."
  (interactive)
  (when (and (projectile-project-p)
             (eq 'git (projectile-project-vcs)))
    (let* ((current-root (projectile-project-root))
           (git-command (concat "git --git-dir=" current-root "/.git "
                                "rev-parse --show-superproject-working-tree")))
      (with-temp-buffer
        (unless (zerop (call-process-shell-command git-command nil t))
          (message "This project is a submodule.")
          t)))))

(provide 'init-windows)
;;; init-windows.el ends here
