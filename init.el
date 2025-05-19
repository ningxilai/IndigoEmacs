;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq  auto-save-list-file-prefix t
       make-backup-files nil
       vc-follow-symlinks t
       use-short-answers t
       package-quickstart t)

;; (global-completion-preview-mode)

;; require

(dolist (dir '("lisp" "elpaca/repos/elpaca"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'nano)
(require 'elpaca-init)
(require 'elpaca-autoloads)
(require 'tools-vertico)

;; ends

(use-package no-littering
  :init
  (savehist-mode 1)
  (save-place-mode 1)
  (save-place-local-mode 1)
  :ensure t
  :config
  (setq no-littering-autoloads t)
  (use-package recentf
    :config
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory))
    )
  )

;; Main

(use-package async
  :ensure t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package hl-line
  ;; :init (global-hl-line-mode t)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2))))
  :hook ((prog-mode) . hl-line-mode)
  )

(use-package highlight-indent-guides
  :ensure t
  :config (setq highlight-indent-guides-method 'bitmap)
  :hook (prog-mode . highlight-indent-guides-mode))

(use-package vundo
  :ensure t
  :bind ("C-x C-u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols)
  :custom (set-face-attribute 'vundo-default nil :family "Unifont"))

(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode))

(use-package dogears
  :ensure t
  :init (dogears-mode)
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
                                      other-window switch-to-buffer
                                      aw-select toggle-window-split
                                      windmove-do-window-select
                                      pager-page-down pager-page-up
                                      tab-bar-select-tab
                                      pop-to-mark-command
                                      pop-global-mark
                                      goto-last-change
                                      xref-go-back
                                      xref-find-definitions
                                      xref-find-references)))

(use-package paren
  :config
  (setq show-paren-delay 0.1
        show-paren-when-point-in-periphery t))

(use-package smartparens
  :ensure t
  :init (show-smartparens-global-mode t)
  :hook
  ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  )

(use-package composite
  :init
  (global-auto-composition-mode nil)
  :hook
  (prog-mode . auto-composition-mode)
  :config
  (dolist (char/ligature-re
           `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                                 "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
                                 "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
                                 "</" "<*")
                           (+ "<"))))
             (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  . ,(rx (+ "&")))
             (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
                                 "|]" "|}" "|=")
                             (+ "|"))))
             (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  . ,(rx (or "+>" (+ "+"))))
           (?\[ . ,(rx (or "[<" "[|")))
           (?\{ . ,(rx "{|"))
           (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                           (+ "#"))))
           (?\; . ,(rx (+ ";")))
           (?_  . ,(rx (or "_|_" "__")))
           (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  . ,(rx "$>"))
           (?^  . ,(rx "^="))
           (?\] . ,(rx "]#"))))
    (let ((char (car char/ligature-re))
          (ligature-re (cdr char/ligature-re)))
      (set-char-table-range composition-function-table char
                            `([,ligature-re 0 font-shape-gstring]))))  
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (smartparens-mode . rainbow-delimiters-mode))

(use-package region-occurrences-highlighter
  :ensure t
  :hook ((prog-mode org-mode text-mode) . region-occurrences-highlighter-mode))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package dired
  :config
  (setq dired-movement-style 'cycle)
  (setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; ends

;; ProjectManager

(use-package magit :ensure t)
(use-package projectile :ensure t)
(use-package transient :ensure t)

;; ends

;; Menu

(use-package enlight
  :ensure t
  :hook (enlight . (lambda () (hl-line-mode nil)))
  :init (setopt initial-buffer-choice #'enlight)
  :custom
  (enlight-content
   (concat
    (propertize "MENU" 'face 'highlight)
    "\n\n"
    (enlight-menu
     '(("\nOrg Mode"
	("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("\nFolder"
	("Desktop folder" (dired "~/Desktop") "p")
	("Downloads folder" (dired "~/Downloads") "d"))
       ("\nInit"
	("init.el" (dired "~/.config/emacs/") "i"))
       ("\nOther"
	("Projects" project-switch-project "p"))))))
  )

;; ends

;; UI

(use-package nord-theme :ensure t :init (load-theme 'nord t))
(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))
(use-package nerd-icons :ensure t)
(use-package nerd-icons-dired :ensure t :init (nerd-icons-dired-mode 1))
(use-package form-feed :ensure t :config (add-hook 'elpaca-after-init-hook #'global-form-feed-mode))

;; ends

;; ViewTools

(use-package pdf-tools :ensure t :config (pdf-tools-install))

;; ends

;; Org

(use-package htmlize
  :ensure t)
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist t)
                              (prettify-symbols-mode 1)))))

;; ends

;; Term

(use-package aweshell
  :ensure (aweshell :type git :host github :repo "manateelazycat/aweshell"))

(use-package vterm
  :ensure  (vterm :type git :host github :repo "akermu/emacs-libvterm"
            :files "*" :post-build t)
  :config
  (setq vterm-shell "zsh")
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "vterm @ %s" title) t))
  :hook
  (vterm-mode . (lambda()
                  (set (make-local-variable 'buffer-face-mode-face) '(:family "FiraCode Nerd Font Mono"))
                  (buffer-face-mode t)))
  (vterm-set-title-functions . vterm--rename-buffer-as-title))
(use-package multi-vterm :ensure t)
(use-package vterm-toggle
  :ensure t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;; ends

;; LSP-BRIDGE

(use-package markdown-mode
  :ensure t
  :mode
  (("README\\.md\\'" . gfm-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-additional-languages "Mermaid"
        )
  )
(use-package markdown-toc :ensure t)

(use-package yasnippet :ensure t :init (yas-global-mode 1))
(use-package lsp-bridge
  :ensure (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config (setq lsp-bridge-markdown-lsp-server 'marksman)
  )

;; ends

(setq initial-scratch-echo-area-message "iris")

;; init.el ends here
