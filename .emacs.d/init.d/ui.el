;; change "yes-or-no" to "y-or-n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable the GUI Controls
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Always show line numbers in left gutter
(global-linum-mode 1)

;; Turn off line wrap.
(setq-default
    truncate-lines t)

(require 'color-theme)
(color-theme-initialize)

(load "color-theme-vibrant-ink")
(color-theme-vibrant-ink)
(set-cursor-color "#ffffff")

;; Consolas is awesome.
(set-frame-font "Consolas-11.0")
