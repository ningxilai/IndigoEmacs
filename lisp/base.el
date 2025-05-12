;; Base  -*- lexical-binding: t; -*-

;; Base

(setq tab-always-indent 'complete
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables nil
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      resize-mini-windows 'grow-only
      icomplete-matches-format t)
(bind-key "TAB" #'icomplete-force-complete icomplete-minibuffer-map)
(bind-key "RET" #'icomplete-force-complete-and-exit icomplete-minibuffer-map)

(set-face-attribute 'default nil
                    :height 140 :weight 'light :family "IBM Plex Mono")
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)

(set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal ))
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default (selected-frame) :height 120)

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

(use-package nord-theme :ensure t :init (load-theme 'nord t))

(use-package nano-modeline
  :ensure t
  :hook (org-mode . nano-modeline-org-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package async
  :ensure t
  :init (dired-async-mode 1)
  :config (async-bytecomp-package-mode 1))

(use-package no-littering
  :ensure t
  :config
  (recentf-mode 1))

(use-package amx :ensure t :bind ("M-x" . amx))

(use-package vundo
  :ensure t
  :config (setq vundo-glyph-alist vundo-unicode-symbols)
  :custom (set-face-attribute 'vundo-default nil :family "Symbola")
  :bind ("C-x C-u" . vundo)
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

;; copy by https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org

(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-mouse-drag-files t)                   ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t) ; added in Emacs 29
  (setq mouse-1-click-follows-link t)
  )

(provide 'base)
