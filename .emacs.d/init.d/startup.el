;; Get rid of startup screen
(setq
  inhibit-startup-message t
  inhibit-startup-echo-area-message t)

(setq backup-directory-alist
  (list (cons "." (expand-config-file-name "backup-files"))))

(setq custom-file (expand-config-file-name "custom.el"))
