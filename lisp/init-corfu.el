(require 'corfu)
(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto t)                 ;; Enable auto completion
(setq corfu-preview-current nil)    ;; Disable current candidate preview
(setq corfu-preselect-first nil)    ;; Disable candidate preselection
(setq corfu-history-mode 1)
(setq savehist-mode 1)
;;(setq add-to-list 'savehist-additional-variables 'corfu-history)
  ;; (:map corfu-map
  ;;       ("TAB" . corfu-next)
  ;;       ("C-n" . corfu-next)
  ;;       ("C-p" . corfu-previous)
  ;;       ([tab] . corfu-next)
  ;;       ("S-TAB" . corfu-previous)
  ;;       ([backtab] . corfu-previous)
  ;;       )

(global-corfu-mode)

;; A few more useful configurations...
(require 'emacs)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
 (setq tab-always-indent 'complete)

;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t



(provide 'init-corfu)
