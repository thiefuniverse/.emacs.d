(require 'corfu)
(setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
(setq corfu-auto t)                 ;; Enable auto completion
(setq corfu-preview-current nil)    ;; Disable current candidate preview
(setq corfu-preselect-first nil)    ;; Disable candidate preselection
(setq corfu-history-mode 1)
(setq corfu-popupinfo-mode 1)
(add-hook 'rust-mode-hook 'eglot-ensure)

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

;; Option 1: Specify explicitly to use Orderless for Eglot
(setq completion-category-overrides '((eglot (styles orderless))
                                      (eglot-capf (styles orderless))))

;; Option 2: Undo the Eglot modification of completion-category-defaults
(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

(require 'cape)
;; Enable cache busting, depending on if your server returns
;; sufficiently many candidates in the first place.
;;(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)


;; A few more useful configurations...
(require 'emacs)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
;;(setq tab-always-indent 'complete)

;; Enable auto completion and configure quitting
(setq corfu-auto t
      corfu-quit-no-match 'separator) ;; or t



(provide 'init-corfu)
