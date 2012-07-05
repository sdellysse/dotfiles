(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(require 'cl)

(when (not (boundp 'emacs-config-directory))
  (defconst emacs-config-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (concat emacs-config-directory "/packages/key-chord.el"))
(add-to-list 'load-path (concat emacs-config-directory "/packages/evil"))

(require 'evil)
(evil-mode 1)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
