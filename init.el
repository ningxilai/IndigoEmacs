;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize) ;; You might already have this line

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/Trash/")

;;  (defalias 'yes-or-no-p 'y-or-n-p)

(setq flyspell-mode +1)

(defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(setq initial-buffer-choice t)
(setq initial-scratch-echo-area-message "iris")

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package undo-tree
  :ensure t
  :init (undo-tree-mode +1))

(use-package doom-themes
 :ensure t
 :init (load-theme 'doom-one t)
 :config (setq-default mode-line-format t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode +1))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2)))))

(use-package page-break-lines
  :ensure t
  :hook (after-init . global-page-break-lines-mode)
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
  (setq dashboard-navigation-cycle t)
  (setq dashboard-display-icons-p t)     ; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ; use `nerd-icons' package
  (setq dashboard-startup-banner "~/.config/emacs/marivector.xpm")
  :init
  (dashboard-setup-startup-hook))

(use-package projectile :ensure t)

;; Highlight indentions
(use-package indent-bars
  :ensure t
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.225))
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				                       if_statement with_statement while_statement)))
  :hook ((prog-mode yaml-mode) . indent-bars-mode)
  :config (require 'indent-bars-ts))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :ensure t
  :defines helpful-mode-map
  :bind (:map help-mode-map
              ("w" . rainbow-mode))
  :hook ((mhtml-mode html-mode html-ts-mode php-mode latex-mode help-mode helpful-mode) . rainbow-mode)
  :init (with-eval-after-load 'helpful
          (bind-key "w" #'rainbow-mode helpful-mode-map))
  :config
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
          (overlay-put ov 'ovrainbow t)
          (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                    "white" "black"))
                                  (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)
    
    (defun my-rainbow-clear-overlays ()
        "Clear all rainbow overlays."
        (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays)))

;; Programming

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  :ensure t)

(use-package colorful-mode
  :diminish
  :ensure t ; Optional
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :hook (after-init . global-colorful-mode)
  :config
  (global-colorful-mode t)
  (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode)))

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
  :config
  (setq company-backends '((:separate company-capf company-dabbrev-code))
	company-global-modes '(not shell-mode)
                                        ; company-minimum-prefix-length 1
        )
  )

(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; Use Company backends as Capfs.
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'company-files #'company-keywords #'company-dabbrev))))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (setq corfu-auto t
      corfu-quit-no-match 'separator))

;; (setq text-mode-ispell-word-completion nil) or (customize-set-variable 'text-mode-ispell-word-completion nil) => https://github.com/minad/corfu/discussions/457

(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

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
  (eldoc-echo-area-use-multiline-p t) ;; eldoc-documentation-function should only return a single line
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit nil :weight bold :foreground "yellow3"))))
  :hook
  ((typst-ts-mode) . eglot-ensure)
  ((markdown-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
  )

(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-ediff-dwim-show-on-hunks t))

(use-package diff-hl
  :vc (:url "https://github.com/dgutov/diff-hl")
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh))

;; Markdown && Typst

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("README\\.md\\'" . gfm-mode))
  :hook (writeroom-mode-hook . markdown-mode-hook)
  :config (setq markdown-enable-wiki-links t
                markdown-italic-underscore t
                markdown-asymmetric-header t
                markdown-make-gfm-checkboxes-buttons t
                markdown-gfm-uppercase-checkbox t
                markdown-fontify-code-blocks-natively t))

(use-package markdown-toc
  :diminish
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc))
  :hook (markdown-mode . markdown-toc-mode)
  :init (setq markdown-toc-indentation-space 2
              markdown-toc-header-toc-title "\n## Table of Contents"
              markdown-toc-user-toc-structure-manipulation-fn 'cdr))

(use-package typst-ts-mode
  :vc (:url "https://codeberg.org/meow_king/typst-ts-mode")
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(use-package websocket :ensure t)

(use-package typst-preview
  :vc (typst-preview :url "https://github.com/havarddj/typst-preview.el"
                     :rev :last-release)
  :config
  (setq typst-preview-executable "tinymist preview")
  (setq typst-preview-browser "default")
  )

(setq-default eglot-workspace-configuration
              '(:tinymist (:exportPdf "onSave")))
;; PDF

(use-package pdf-tools
  :ensure t
  :config (pdf-tools-install))

;; Org

(use-package org
  :vc (:url "https://git.sr.ht/~bzg/org")
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook
            '(lambda ()
               (visual-line-mode 1))))

(use-package org-contrib :ensure t)
(use-package org-modern
  :ensure t
  :hook (org-mode-hook . org-modern-mode)(org-agenda-finalize-hook . org-modern-agenda)
  ;; test
  :config (add-hook 'org-modern-mode-hook
                    (lambda ()
	              (setq buffer-face-mode-face '(:family "Iosevka"))
	              (buffer-face-mode)))
  )
(use-package htmlize :ensure t)

;; Eshell

(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
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
             (propertize " α " 'face `(:foreground "#FF6666"))
         (propertize " λ " 'face `(:foreground "#A6E22E"))))))

(setq eshell-highlight-prompt nil)

;; Vterm

(use-package vterm
  :bind (:map vterm-mode-map
              ([f9] . (lambda ()
                        (interactive)
                        (and (fboundp 'shell-pop-toggle)
                             (shell-pop-toggle)))))
  :init (setq vterm-always-compile-module t))

(use-package multi-vterm
  :bind ("C-<f9>" . multi-vterm)
  :custom (multi-vterm-buffer-name "vterm")
  :config
  (with-no-warnings
    ;; Use `pop-to-buffer' instead of `switch-to-buffer'
    (defun my-multi-vterm ()
      "Create new vterm buffer."
      (interactive)
      (let ((vterm-buffer (multi-vterm-get-buffer)))
        (setq multi-vterm-buffer-list
              (nconc multi-vterm-buffer-list (list vterm-buffer)))
        (set-buffer vterm-buffer)
        (multi-vterm-internal)
        (pop-to-buffer vterm-buffer)))
    (advice-add #'multi-vterm :override #'my-multi-vterm)))

;; fonts

(set-face-attribute 'default nil :font "IBM Plex Mono")

(set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal ))
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default (selected-frame) :height 120)

;; custom

(setq custom-file "~/.config/emacs/custom.el")

(load-file custom-file)

;; init.el ends here
