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
                        (evil-mode)))))

(use-package evil-collection
  :ensure t
  :after evil

  :config
  (evil-collection-init))

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
            (evil-leader/set-key
              "d" 'neotree-toggle)

            (setq neo-theme 'nerd)
            (add-hook 'neotree-mode-hook (lambda ()
                                           (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)))))
