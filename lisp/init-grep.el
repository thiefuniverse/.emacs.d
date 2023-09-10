;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; grep word in project
;;;(use-package deadgrep)

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(when (and (executable-find "ag")
          (require 'ag))
  (require 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

(when (and (executable-find "rg")
          (require 'rg))
  (global-set-key (kbd "M-?") 'rg-project))

;;;(use-package fzf
;;;  :bind
;;;  ;; Don't forget to set keybinds!
;;;  :config
;;;  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
;;;        fzf/executable "fzf"
;;;        fzf/git-grep-args "-i --line-number %s"
;;;        ;; command used for `fzf-grep-*` functions
;;;        ;; example usage for ripgrep:
;;;        ;; fzf/grep-command "rg --no-heading -nH"
;;;        fzf/grep-command "rg --no-heading -nH"
;;;        ;; If nil, the fzf buffer will appear at the top of the window
;;;        fzf/position-bottom t
;;;        fzf/window-height 15))

(provide 'init-grep)
;;; init-grep.el ends here
