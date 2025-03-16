;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize) ;; You might already have this line

;; ----------------------------------------
;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/Trash/")

;;  (defalias 'yes-or-no-p 'y-or-n-p)
;; ----------------------------------------

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

(use-package prism
  :vc (:url "https://github.com/alphapapa/prism.el")
  :config  (prism-set-colors :num 16
             :desaturations (cl-loop for i from 0 below 16
                                     collect (* i 2.5))
             :lightens (cl-loop for i from 0 below 16
                                collect (* i 2.5))
             :colors (list "dodgerblue" "medium sea green" "sandy brown")
             
             :comments-fn
             (lambda (color)
               (prism-blend color
                            (face-attribute 'font-lock-comment-face :foreground) 0.25))
             
             :strings-fn
             (lambda (color)
               (prism-blend color "white" 0.5))))

(use-package highlight-parentheses
  :ensure t
  :hook ((minibuffer-setup . highlight-parentheses-minibuffer-setup)
         (prog-mode . highlight-parentheses-mode))
  :config
  (setq highlight-parentheses-colors '("firebrick1" "firebrick3" "orange1" "orange3")
        highlight-parentheses-attributes '((:underline t) (:underline t) (:underline t))
        highlight-parentheses-delay 0.2)
  )

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2)))))

(use-package page-break-lines
  :ensure t
  :init (page-break-lines-mode)
  :diminish (page-break-lines-mode visual-line-mode)
  :config (set-fontset-font "fontset-default"
                  (cons page-break-lines-char page-break-lines-char)
                  (face-attribute 'default :family)))

(use-package dogears
  :ensure t
  :hook (after-init . dogears-mode)
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-idle 1
        dogears-limit 200
        dogears-position-delta 20)
  (setq dogears-functions '(find-file recenter-top-bottom
                                      other-window
                                      switch-to-buffer
                                      aw-select
                                      toggle-window-split
                                      windmove-do-window-select
                                      pager-page-down
                                      pager-page-up
                                      tab-bar-select-tab
                                      goto-last-change)))

;; Programming

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
  :defer t)

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

(use-package embark
  :ensure t
  :defer t
  :bind
  (:map minibuffer-mode-map
	("C-c C-e" . embark-export)
	("C-c C-a" . embark-act)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

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

;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :hook (markdown-mode . writeroom-mode)
  )

(use-package focus
  :ensure t)

(use-package writeroom-mode
  :ensure t
  :hook (focus-mode . global-writeroom-mode))

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

(set-face-attribute 'default nil :font "Cascadia Code")

(set-fontset-font t 'unicode (font-spec :family "Noto Sans Mono":weight 'normal :slant 'normal ))
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default (selected-frame) :height 120)

;; custom

(setq custom-file "./custom.el")
(load-file custom-file)

;;; init.el ends here
