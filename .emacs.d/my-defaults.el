;; Disable the arrow keys. This isn't as much of a problem
;; as when I was learning vim. I'm already trained to keep
;; the fingers on the main cluster.
(global-set-key (kbd "<up>") 'disabled-key)
(global-set-key (kbd "<down>") 'disabled-key)
(global-set-key (kbd "<left>") 'disabled-key)
(global-set-key (kbd "<right>") 'disabled-key)

;; change "yes-or-no" to "y-or-n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always show line numbers in left gutter
(global-linum-mode 1)

;; Turn off line wrap.
(setq-default
    truncate-lines t)

;; Disable the GUI Controls
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Get rid of startup screen
(setq
    inhibit-startup-message t
    inhibit-startup-echo-area-message t)


(provide 'my-defaults)
