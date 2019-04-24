(use-package editorconfig
  :ensure t

  :config
  (editorconfig-mode))

(use-package evil
  :ensure t

  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)

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

;;(use-package evil-collection
;;  :ensure t
;;  :after evil
;;
;;  :config
;;  (evil-collection-init))

(use-package evil-escape
  :ensure t
  :after evil

  :config
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-unordered-key-sequence t)
  (evil-escape-mode))

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
                                           (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
                                           (define-key evil-normal-state-local-map (kbd "o") 'neotree-enter)
                                           (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
                                           (define-key evil-normal-state-local-map (kbd "i") 'neotree-enter-horizontal-split)))))
