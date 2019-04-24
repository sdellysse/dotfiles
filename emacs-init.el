;;;; UTF-8 ALL THE THINGS
(set-language-environment "UTF-8")
(set-locale-environment "en_US.UTF-8")
(prefer-coding-system 'utf-8-unix)

;;;; Clean screen startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; No GUI
(menu-bar-mode -1)
(tool-bar-mode -1)

;;;; Tell emacs to use this file for customizations, leave this one alone
(setq custom-file "~/.config/dotfiles/emacs-custom.el")
(load custom-file)

;;;; Put backup files in /tmp
(setq backup-directory-alist         `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;;; nowrap
(set-default 'truncate-lines t)

;;;; (y or n) instead of (yes or no)
(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Package Repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;; Pull in use-package if we don't already have it, launch our package config
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;;;; PACKAGES:::
;;;; ============
(use-package editorconfig
  :ensure t

  :config (progn
            (editorconfig-mode)))

(use-package evil
  :ensure t

  :init (progn
          (setq evil-want-integration t)
          (setq evil-want-keybinding nil))

  :config (progn
            ;;nest for now, gotta call (evil-mode) after loading evil-leader
            (use-package evil-leader
              :ensure t

              :init (global-evil-leader-mode)
              :config (progn
                        (setq evil-leader/in-all-states t)
                        (evil-leader/set-leader "<SPC>")

                        (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
                        (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
                        (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
                        (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
                        (evil-mode)))))

(use-package evil-collection
  :ensure t
  :after (evil magit)

  :config (evil-collection-init 'magit))

(use-package evil-escape
  :ensure t
  :after evil

  :config (progn
            (setq-default evil-escape-key-sequence "jk")
            (setq-default evil-escape-unordered-key-sequence t)
            (evil-escape-mode)))

;;(use-package helm
;;  :ensure t
;;
;;  :config (progn
;;	    (setq helm-autoresize-mode t)
;;	    (setq helm-buffer-max-length 40)
;;
;;	    (global-set-key (kbd "M-x") #'helm-M-x)
;;
;;	    (define-key helm-map            (kbd "S-SPC") 'helm-toggle-visible-mark)
;;	    (define-key helm-find-files-map (kbd "C-k")   'helm-find-files-up-one-level)
;;
;;	    (helm-mode)))


(use-package magit
  :ensure t)

(use-package neotree
  :ensure t
  :after (evil evil-leader)

  :config (progn
            (setq neo-theme 'nerd)

            (evil-set-initial-state 'neotree-mode 'normal)
            (evil-leader/set-key
              "d" 'neotree-toggle)

	    (add-hook 'neotree-mode-hook (lambda ()
					   (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
					   (define-key evil-normal-state-local-map (kbd "I")   'neotree-hidden-file-toggle)
					   (define-key evil-normal-state-local-map (kbd "i")   'neotree-enter-horizontal-split)
					   (define-key evil-normal-state-local-map (kbd "ma")  'neotree-create-node)
					   (define-key evil-normal-state-local-map (kbd "mc")  'neotree-copy-node)
					   (define-key evil-normal-state-local-map (kbd "md")  'neotree-delete-node)
					   (define-key evil-normal-state-local-map (kbd "mm")  'neotree-rename-node)
					   (define-key evil-normal-state-local-map (kbd "o")   'neotree-enter)
					   (define-key evil-normal-state-local-map (kbd "s")   'neotree-enter-vertical-split)
					   (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-quick-look)
					   (define-key evil-normal-state-local-map (kbd "u")   'neotree-select-up-node)))))
