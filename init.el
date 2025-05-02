;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

;; load custom setting

(dolist (dir '("lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path))

(require 'nano)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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

(use-package nano-modeline
  :ensure t
  :hook (add-hook 'org-mode-hook  #'nano-modeline-org-mode))

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(use-package amx :ensure t :bind ("M-x" . amx))

(use-package undo-tree
  :ensure t
  :init (undo-tree-mode +1))

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

;; Programming

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize)
  :ensure t)

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

(use-package symbol-overlay
  :ensure t
  :init (setq symbol-overlay-mode t))

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
  ((LaTeX-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
  )

(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode))

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
  :config
  (pdf-tools-install))

;; Org

(use-package org
  :ensure t
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

;; LaTeX

(use-package latex-change-env
  :after latex
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env)))

(use-package latex
  :ensure auctex
  :commands (TeX-latex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-show-compilation t)
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'xetex)
  (with-eval-after-load 'tex-mode
    ;; "latexmk -shell-escape -bibtex -xelatex -g -f %f"
    (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf"))
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))) ;; https://emacs-china.org/t/auctex-setup-synctex-with-pdf-tools-not-working/11257/2
  
  (setq TeX-output-view-style (quote (("^pdf$" "." "okular %o %(outpage)"))))
  (setq TeX-command-default "XeLaTeX")
  
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                TeX-source-correlate-start-server t
                TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
;; PDF View
  (setq pdf-sync-backward-display-action t
        pdf-sync-forward-display-action t
        TeX-source-correlate-mode t
        TeX-source-correlate-method '((dvi . source-specials)
                                      (pdf . synctex))
        TeX-source-correlate-start-server t
        reftex-plug-into-AUCTeX t)
  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map 
                (kbd "C-c C-g") #'pdf-sync-forward-search))
  (add-hook 'LaTeX-mode
          (defun init-latex-mode ()
            "Stuff to do when opening `LaTeX-mode' files."
            (add-save-hook 'after-save-hook
                           (lambda ()
                             (TeX-command-run-all nil))
                             nil t)))
  :hook
  (pdf-view-mode-hook . (lambda() (line-number-mode -1)))
  (TeX-after-TeX-LaTeX-command-finished-hook . TeX-revert-document-buffer)
  ;; Font Setting
  (LaTeX-mode-hook . (lambda () (setq buffer-face-mode-face '(:family "Hack")) (buffer-face-mode)))
  )

(use-package preview-auto
  :after latex
  :hook (LaTeX-mode . preview-auto-setup)
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)

  ;; Uncomment the following only if you have followed the above
  ;; instructions concerning, e.g., hyperref:

  ;; (preview-LaTeX-command-replacements
  ;;  '(preview-LaTeX-disable-pdfoutput))
  )

(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (LaTeX-mode-hook . turn-on-reftex))

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

(use-package eat
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode)))

;; init.el ends here
