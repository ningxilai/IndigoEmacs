;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(package-initialize) ;; You might already have this line

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/Trash/")

;;  (defalias 'yes-or-no-p 'y-or-n-p)

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)

(use-package doom-themes
 :ensure t
 :init (load-theme 'doom-one t)
 :config (setq-default mode-line-format t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package hl-line
  :ensure nil
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2)))))

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode)
  :diminish (page-break-lines-mode visual-line-mode)
  :config (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family)))

(use-package dashboard
  :vc (:url "https://github.com/emacs-dashboard/emacs-dashboard")
  :custom
  (dashboard-center-content t)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((projects . 5)
                     (recents . 5)
                     (agenda . 5)
                     (registers . 5)))
  
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-navigation-cycle t)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-startup-banner "~/.config/emacs/marivector.xpm")
  :init (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package projectile
  :ensure t)

;; Programming

(use-package saveplace
  :ensure nil
  :init (save-place-mode 1))

(use-package colorful-mode
  :diminish
  :ensure t ; Optional
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t)
  (add-to-list 'global-colorful-modes 'helpful-mode))

(use-package highlight-parentheses
  :ensure t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2))

(use-package company
  :ensure t
  :defer t
  :hook (after-init . global-company-mode)
  :config
  (setq company-backends '((:separate company-capf company-dabbrev-code))
	company-global-modes '(not shell-mode)
	company-minimum-prefix-length 1
	company-dabbrev-code-ignore-case t
	company-dabbrev-code-modes t
	company-dabbrev-code-everywhere t
	company-dabbrev-code-completion-styles '(substring flex))
  (add-to-list 'company-transformers 'company-sort-prefer-same-case-prefix))

(use-package orderless
  :ensure t
  :defer 0.1
  :config
  (setq completion-styles '(substring orderless flex)))

(use-package consult
  :ensure t
  :defer t
  :bind ("C-s" . consult-line))

(use-package vertico
  :ensure t
  :defer t
  :custom
  (vertico-cycle t)
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package embark
  :ensure t
  :defer t
  :bind
  (:map minibuffer-mode-map
	("C-c C-e" . embark-export)
	("C-c C-a" . embark-act)))

(use-package eglot
  :defer t
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eldoc-echo-area-use-multiline-p nil) ;; eldoc-documentation-function should only return a single line
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit nil :weight bold :foreground "yellow3"))))
  :hook
  ((python-ts-mode) . eglot-ensure))

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-ediff-dwim-show-on-hunks t))

(use-package diff-hl
  :vc (:url "https://github.com/dgutov/diff-hl")
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh))

;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :hook (writeroom-mode . markdown-mode))

(use-package focus
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :hook (focus-mode-hook . writeroom-mode))

;; Org

(use-package org
  :diminish org-indent-mode
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook
            '(lambda ()
               (visual-line-mode 1))))

(use-package htmlize
  :ensure t)

;; Eshell

(setq eshell-prompt-function
      (lambda nil
        (concat
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face `(:foreground "#99CCFF"))
           (replace-regexp-in-string
            (getenv "HOME")
            (propertize "~" 'face `(:foreground "#99CCFF"))
            (propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
         (if (= (user-uid) 0)
             (propertize " Î± " 'face `(:foreground "#FF6666"))
         (propertize " Î» " 'face `(:foreground "#A6E22E"))))))

(setq eshell-highlight-prompt nil)

(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

;; Eat

(use-package eat
  :ensure t
  :hook
  ;; For `eat-eshell-mode'.
  (eshell-load-hook . eat-eshell-mode-hook)
  ;; For `eat-eshell-visual-command-mode'.
  (eshell-load-hook . eat-eshell-visual-command-mode-hook))

;; fonts

(set-face-attribute 'default nil :font "Fira Code")

(set-fontset-font t 'unicode (font-spec :family "Noto Sans Mono":weight 'normal :slant 'normal ))
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default (selected-frame) :height 120)

;;; ligature

(use-package ligature
  :ensure t
  :config
  ;; Enable the www ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable ligatures in programming modes                                                           
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  
  :init (global-ligature-mode 't))

;; custom

(setq custom-file "~/.config/emacs/custom.el")

(load-file custom-file)

;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((emojify :url "https://github.com/iqbalansari/emacs-emojify"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
