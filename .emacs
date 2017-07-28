;; disable startup screen and make initial buffer empty
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ())
(setq initial-scratch-message nil)

;; disable chrome
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; turn off line wrapping
(set-default 'truncate-lines t)

;; line numbers
(global-linum-mode 1)

;; scrolling
(setq scroll-margin 9)
(setq scroll-step 1)

;; prevent this file getting changed by CUSTOM stuff
(setq custom-file "~/.emacs.d/custom.el")
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file)

;; Setup package manager
(require 'package)

(add-to-list 'package-archives '("org"               . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa"             . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable"      . "http://stable.melpa.org/packages/"))
;(add-to-list 'package-archives '("sunrise-commander" . "http://joseito.republika.pl/sunrise-commander/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; packages setup
(use-package editorconfig
  :ensure t
  :config (progn
    (editorconfig-mode 1)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :config (progn
    (load-theme 'sanityinc-tomorrow-bright t)))

(use-package evil-leader
  :ensure t
  :config (progn
    (evil-leader/set-leader "<SPC>")
    (global-evil-leader-mode)))

(use-package evil
  :ensure t
  :config (progn
    (evil-mode 1)
    (define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)))

(use-package neotree
  :ensure t
  :config (progn
    (evil-leader/set-key "d" 'neotree-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "I") 'neotree-hidden-file-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "m") 'neotree-rename-node)
    (evil-define-key 'normal neotree-mode-map (kbd "c") 'neotree-create-node)
    (evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-delete-node)
    (evil-define-key 'normal neotree-mode-map (kbd "i") 'neotree-enter-horizontal-split)
    (evil-define-key 'normal neotree-mode-map (kbd "C") 'neotree-change-root)
    (evil-define-key 'normal neotree-mode-map (kbd "u") 'neotree-select-up-node)
    (evil-define-key 'normal neotree-mode-map (kbd "s") 'neotree-enter-vertical-split)))


(use-package key-chord
  :ensure t
  :config (progn
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
    (key-chord-mode 1)))

(use-package smooth-scroll
  :ensure t
  :config (progn
    (smooth-scroll-mode 1)
    (setq smooth-scroll/vscroll-step-size 5)))

;; language-specific packages
(use-package elixir-mode
  :ensure t)

;;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit. In Delete Selection mode, if the mark is active, just deactivate it; then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; For sublimetext-style minimaps. Causes flicker, disabled
;; (use-package sublimity
;;   :ensure t
;;   :config (progn
;;     (require 'sublimity-map)
;;     (sublimity-map-set-delay 0)
;;     (sublimity-mode 1)))
