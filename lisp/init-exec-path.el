;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; for config environment variable in win
(defun win-reg-read (regkey &optional value access-64-bit)
  "Read a value in the Windows registry. If the value does not exist, it returns nil. For an empty REG_BINARY value it returns T."
  (when (string-equal system-type "windows-nt")
    (let* ((regkey (encode-coding-string regkey locale-coding-system))
           (value (and value (encode-coding-string value locale-coding-system)))
           (reg-exe (concat (getenv "WinDir") "\\System32\\reg.exe"))
           (bits-64-param (if access-64-bit
                              "/reg:64"
                            "/reg:32"))
           (reg-value (let ((output (with-output-to-string
                                      (with-current-buffer
                                          standard-output
                                        (if value
                                            (process-file reg-exe nil t nil
                                                          "query"
                                                          regkey
                                                          "/v"
                                                          value
                                                          bits-64-param)
                                          (process-file reg-exe nil t nil
                                                        "query"
                                                        regkey
                                                        "/ve"
                                                        bits-64-param))))))
                        (set-text-properties 0 (length output) nil output)
                        output)))
      (let* ((lines (split-string reg-value "\n" nil nil))
             (result-line (nth 2 lines)))
        (when (> (length lines) 4)
          (let* ((result-line (if value ; skip value name
                                  (cl-subseq result-line (+ 8 (length value)))
                                (replace-regexp-in-string "^[ ]+[\(][^\)]+[\)][ ]+" ; default value
                                                          ""
                                                          result-line)))
                 (tokens (split-string result-line "    " t))
                 (type-string (car tokens)) ; get registry value type
                 (result (cl-subseq result-line (+ 4 (length type-string))))) ; get registry value
            ;; decode the registry value string into a lisp object
            (cond
             ((or (string-equal type-string "REG_DWORD")
                  (string-equal type-string "REG_QWORD"))
              (string-to-number (cl-subseq result 2) 16))
             ((or (string-equal type-string "REG_SZ")
                  (string-equal type-string "REG_EXPAND_SZ"))
              result)
             ((string-equal type-string "REG_MULTI_SZ")
              (split-string result (regexp-quote "\\0") t)) ; will not work if there are '\0' sequences in the string
             ((string-equal type-string "REG_BINARY")
              (let ((res)
                    (pos 0)
                    (size (/ (length result) 2)))
                (if (zerop size)
                    t ; value is present, but empty
                  (progn
                    (dotimes (i size)
                      (push (string-to-number (cl-subseq result pos (+ pos 2)) 16) res)
                      (incf pos 2))
                    (reverse res)))))
             (t nil))))))))

(defconst win-environment-registry
  "HKEY_LOCAL_MACHINE\\SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment\\")

(defun win-environment-read (vars)
  "read current environment variables, updated with the system"
  (seq-map (lambda (var) (win-reg-read win-environment-registry var)) vars))

(require-package 'exec-path-from-shell)
;;; origin version can't handle windows path env, add some special proceses for windows
(defun exec-path-from-shell-printf-win (args)
  (win-environment-read args))

(defun exec-path-from-shell-getenvs (names)
  "Get the environment variables with NAMES from the user's shell.

Execute the shell according to `exec-path-from-shell-arguments'.
The result is a list of (NAME . VALUE) pairs."
  (when (file-remote-p default-directory)
    (error "You cannot run exec-path-from-shell from a remote buffer (Tramp, etc.)"))
  (let* ((random-default (md5 (format "%s%s%s" (emacs-pid) (random) (current-time))))
         (dollar-names (mapcar (lambda (n)
                                 (if sys/win32p
                                     (format "%s" n)
                                   (format "${%s-%s}" n random-default))
                                 ) names))
         (values  (if sys/win32p (exec-path-from-shell-printf-win dollar-names)
                    (split-string (exec-path-from-shell-printf
                                   (mapconcat #'identity (make-list (length names) "%s") "\\000")
                                   dollar-names) "\0")
                    )))
    (let (result)
      (while names
        (prog1
            (let ((value (car values)))
              (push (cons (car names)
                          (unless (string-equal random-default value)
                            value))
                    result))
          (setq values (cdr values)
                names (cdr names))))
      result)))

;;;(win-environment-read (list "PATH"))
;;;(exec-path-from-shell-getenvs (list "OS" "PATH"))
;;;(setq exec-path-from-shell-debug t)

;; (call-process (executable-find "powershell") nil t nil
;;               "-Command" "(get-process explorer).StartInfo.EnvironmentVariables[\"Path\"]"
;;               )
;; (call-process                            "C:/tools/emacs-28.0.91/libexec/emacs/28.0.91/x86_64-w64-mingw32/cmdproxy.exe" "-l" "-i" "-c" "c:/Program Files/Git/usr/bin/printf.exe '__RESULT\\000%s\\000%s\\000__RESULT' \"%PATH%\" \"%PATH%\"")

(with-eval-after-load 'exec-path-from-shell
  (dolist (var '("RIME_USER_DATA" "LIBRIME_ROOT" "ORG_STORE_PATH" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (or (memq window-system '(mac ns x pgtk))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))
(provide 'init-exec-path)
;;; init-exec-path.el ends here
