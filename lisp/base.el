;; Base  -*- lexical-binding: t; -*-

;; Base

(use-package nano-modeline
  :ensure t
  :hook (add-hook 'org-mode-hook  #'nano-modeline-org-mode))

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package amx :ensure t :bind ("M-x" . amx))

(use-package vundo
  :ensure t
  :config (setq vundo-glyph-alist vundo-unicode-symbols)
  :custom (set-face-attribute 'vundo-default nil :family "Symbola")
  )

(use-package dashboard
  :ensure t
  :custom
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)
                     (registers . 5)))
  
  :config
  (setq dashboard-navigation-cycle t)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-startup-banner "~/.config/emacs/marivector.png")
  :init
  (dashboard-setup-startup-hook))

(use-package projectile :ensure t)

(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode))

(provide 'base)
