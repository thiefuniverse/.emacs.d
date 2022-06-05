;;; init-locales.el --- Configure default locale -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq system-time-locale "C");;; fix bug by https://github.com/purcell/emacs.d/issues/736

(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system
 (if (eq system-type 'windows-nt)
     'utf-16-le  ;; https://rufflewind.com/2014-07-20/pasting-unicode-in-emacs-on-windows
   'utf-8))

;; Fonts: steal from centaur
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun flythief/setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Fira Code" "Source Code Pro"
                           "Menlo" "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :font font
                                        :height (cond (sys/macp 160)
                                                      (sys/win32p 180)
                                                      (t 100))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Emoji
    ;; (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji")
    ;;          when (font-installed-p font)
    ;;          return (set-fontset-font t 'emoji `(,font . "iso10646-1") nil 'prepend))

    ;; Specify font for Chinese characters
    ;; install Sarasa Mono SC Nerd from https://github.com/laishulu/Sarasa-Mono-SC-Nerd
    (cl-loop for font in '("Sarasa Mono SC Nerd" "WenQuanYi Micro Hei" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff) font))))
(add-hook 'server-after-make-frame-hook #'flythief/setup-fonts)

(provide 'init-locales)
;;; init-locales.el ends here
