;;; core/autoload/scratch.el -*- lexical-binding: t; -*-
;;;(require 's)

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun doom-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (let ((projectile-project-root
         (unless dir (bound-and-true-p projectile-project-root)))
        projectile-require-project-root)
    (projectile-project-root dir)))

(defun doom-project-name (&optional dir)
  "Return the name of the current project.

Returns '-' if not in a valid project."
  (if-let (project-root (or (doom-project-root dir)
                            (if dir (expand-file-name dir))))
      (funcall projectile-project-name-function project-root)
    "-"))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "doom--transient-%d-h"
                            (put 'add-transient-hook! 'counter
                                 (1+ (or (get 'add-transient-hook! 'counter)
                                         0)))))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))

(defvar scratch-default-file "__default"
  "The default file name for a project-less scratch buffer.

Will be saved in `scratch-dir'.")

(defvar scratch-dir (concat user-emacs-directory "scratch")
  "Where to save persistent scratch buffers.")

(defvar scratch-initial-major-mode nil
  "What major mode to start fresh scratch buffers in.

Scratch buffers preserve their last major mode, however, so this only affects
the first, fresh scratch buffer you create. This accepts:

  t           Inherits the major mode of the last buffer you had selected.
  nil         Uses `fundamental-mode'
  MAJOR-MODE  Any major mode symbol")

(defvar scratch-buffers nil
  "A list of active scratch buffers.")

(defvar scratch-current-project nil
  "The name of the project associated with the current scratch buffer.")
(put 'scratch-current-project 'permanent-local t)

(defvar scratch-buffer-hook ()
  "The hooks to run after a scratch buffer is created.")


(defun load-persistent-scratch-buffer (project-name)
  (setq-local scratch-current-project
              (or project-name
                  scratch-default-file))
  (let ((smart-scratch-file
         (expand-file-name (concat scratch-current-project ".el")
                           scratch-dir)))
    (make-directory scratch-dir t)
    (when (file-readable-p smart-scratch-file)
      (message "Reading %s" smart-scratch-file)
      (cl-destructuring-bind (content point mode)
          (with-temp-buffer
            (save-excursion (insert-file-contents smart-scratch-file))
            (read (current-buffer)))
        (erase-buffer)
        (funcall mode)
        (insert content)
        (goto-char point)
        t))))

;;;###autoload
(defun scratch-buffer (&optional dont-restore-p mode directory project-name)
  "Return a scratchpad buffer in major MODE."
  (let* ((buffer-name (if project-name
                          (format "*scratch (%s)*" project-name)
                        "*scratch*"))
         (buffer (get-buffer buffer-name)))
    (with-current-buffer
        (or buffer (get-buffer-create buffer-name))
      (setq default-directory directory)
      (setq-local so-long--inhibited t)
      (if dont-restore-p
          (erase-buffer)
        (unless buffer
          (load-persistent-scratch-buffer project-name)
          (when (and (eq major-mode 'fundamental-mode)
                     (functionp mode))
            (funcall mode))))
      (cl-pushnew (current-buffer) scratch-buffers)
      (add-transient-hook! 'switch-buffer-hook (persist-scratch-buffers-h))
      (add-transient-hook! 'switch-window-hook (persist-scratch-buffers-h))
      (add-hook 'kill-buffer-hook #'persist-scratch-buffer-h nil 'local)
      (run-hooks 'scratch-buffer-created-hook)
      (current-buffer))))


;;
;;; Persistent scratch buffer

;;;###autoload
(defun persist-scratch-buffer-h ()
  "Save the current buffer to `scratch-dir'."
  (let ((content (buffer-substring-no-properties (point-min) (point-max)))
        (point (point))
        (mode major-mode))
    (with-temp-file
        (expand-file-name (concat (or scratch-current-project
                                      scratch-default-file)
                                  ".el")
                          scratch-dir)
      (prin1 (list content
                   point
                   mode)
             (current-buffer)))))

;;;###autoload
(defun persist-scratch-buffers-h ()
  "Save all scratch buffers to `scratch-dir'."
  (setq scratch-buffers
        (cl-delete-if-not #'buffer-live-p scratch-buffers))
  (dolist (buffer scratch-buffers)
    (with-current-buffer buffer
      (persist-scratch-buffer-h))))

;;;###autoload
(defun persist-scratch-buffers-after-switch-h ()
  "Kill scratch buffers when they are no longer visible, saving them to disk."
  (unless (cl-some #'get-buffer-window scratch-buffers)
    (mapc #'kill-buffer scratch-buffers)
    (remove-hook 'switch-buffer-hook #'persist-scratch-buffers-after-switch-h)))

;;;###autoload
;;(add-hook 'kill-emacs-hook #'persist-scratch-buffers-h)

;;; Commands
(defvar projectile-enable-caching)
;;;###autoload
(defun open-scratch-buffer (&optional arg project-p same-window-p)
  "Pop up a persistent scratch buffer.

If passed the prefix ARG, do not restore the last scratch buffer.
If PROJECT-P is non-nil, open a persistent scratch buffer associated with the
  current project."
  (interactive "P")
  (let (projectile-enable-caching)
    (funcall
     (if same-window-p
         #'switch-to-buffer
       #'pop-to-buffer)
     (scratch-buffer
      arg
      (cond ((eq scratch-initial-major-mode t)
             (unless (or buffer-read-only
                         (derived-mode-p 'special-mode)
                         (string-match-p "^ ?\\*" (buffer-name)))
               major-mode))
            ((null scratch-initial-major-mode)
             nil)
            ((symbolp scratch-initial-major-mode)
             scratch-initial-major-mode))
      default-directory
      (when project-p
        (doom-project-name))))))

;;;###autoload
(defun switch-to-scratch-buffer (&optional arg project-p)
  "Like `open-scratch-buffer', but switches to it in the current window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (open-scratch-buffer arg project-p 'same-window))

;;;###autoload
(defun open-project-scratch-buffer (&optional arg same-window-p)
  "Opens the (persistent) project scratch buffer in a popup.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (open-scratch-buffer arg 'project same-window-p))

;;;###autoload
(defun switch-to-project-scratch-buffer (&optional arg)
  "Like `open-project-scratch-buffer', but switches to it in the current
window.

If passed the prefix ARG, do not restore the last scratch buffer."
  (interactive "P")
  (open-project-scratch-buffer arg 'same-window))

;;;###autoload
(defun revert-scratch-buffer ()
  "Revert scratch buffer to last persistent state."
  (interactive)
  (unless (string-match-p "^\\*scratch" (buffer-name))
    (user-error "Not in a scratch buffer"))
  (when (load-persistent-scratch-buffer scratch-current-project)
    (message "Reloaded scratch buffer")))

;;;###autoload
(defun delete-persistent-scratch-file (&optional arg)
  "Deletes a scratch buffer file in `scratch-dir'.

If prefix ARG, delete all persistent scratches."
  (interactive)
  (if arg
      (progn
        (delete-directory scratch-dir t)
        (message "Cleared %S" (abbreviate-file-name scratch-dir)))
    (make-directory scratch-dir t)
    (let ((file (read-file-name "Delete scratch file > " scratch-dir "scratch")))
      (if (not (file-exists-p file))
          (message "%S does not exist" (abbreviate-file-name file))
        (delete-file file)
        (message "Successfully deleted %S" (abbreviate-file-name file))))))

(defun jump-scratch ()
  (interactive)
  (if (s-starts-with? "*scratch" (buffer-name))
      (delete-window)
    (open-scratch-buffer)
    )
  )
(defun jump-project-scratch ()
  (interactive)
  (if (s-starts-with? "*scratch" (buffer-name))
      (delete-window)
    (open-project-scratch-buffer)
    )
  )
(provide 'init-scratch)
