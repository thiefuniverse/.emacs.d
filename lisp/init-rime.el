(use-package posframe)
(use-package rime
  :defer t
  :custom
  (default-input-method "rime")
  ;;; copy librime-emacs.dll to emacs install directory
  (rime-librime-root (substitute-in-file-name "$LIBRIME_ROOT"))
  (rime-share-data-dir (substitute-in-file-name "$RIME_USER_DATA"))
  (rime-user-data-dir (substitute-in-file-name "$RIME_USER_DATA"))
  (rime-show-candidate 'posframe))

(provide 'init-rime)
