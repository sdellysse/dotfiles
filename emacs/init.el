(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(prefer-coding-system 'utf-8-unix)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;;(menu-bar-mode -1)
;;(tool-bar-mode -1)

(setq custom-file "~/.config/dotfiles/emacs/custom.el")
(load custom-file 'noerror)

(set-default 'truncate-lines t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(load "~/.config/dotfiles/emacs/packages.el")
