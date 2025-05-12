;; Markup  -*- lexical-binding: t; -*-

;; Markdown

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
                markdown-fontify-code-blocks-natively t
                markdown-open-command "firefox") ;; deps by "https://github.com/simov/markdown-viewer"
  )

(use-package markdown-toc
  :ensure t
  :bind (:map markdown-mode-command-map
         ("r" . markdown-toc-generate-or-refresh-toc))
  :hook (markdown-mode . markdown-toc-mode)
  :init (setq markdown-toc-indentation-space 2
              markdown-toc-header-toc-title "\n## Table of Contents"
              markdown-toc-user-toc-structure-manipulation-fn 'cdr))

;; Typst

(use-package typst-ts-mode
  :defer t
  :vc (typst-ts-mode :url "https://codeberg.org/meow_king/typst-ts-mode.git"
                     :rev :newest
                     :lisp-dir "./"
                     :shell-command "cargo build")
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(use-package websocket :ensure t)

(use-package typst-preview
  :defer t
  :vc (typst-preview :url "https://github.com/havarddj/typst-preview.el"
                     :rev :newest)
  :init
  (require 'typst-preview)
  :config
  (setq typst-preview-executable "tinymist preview")
  (setq typst-preview-browser "default")
  )

(setq-default eglot-workspace-configuration '(:tinymist (:exportPdf "onSave")))

;; Org

(use-package org
  :hook
  (org-mode . org-indent-mode)
  (org-mode . (lambda ()(visual-line-mode 1)))
  (org-mode . org-modern-mode))

(use-package org-contrib :ensure t)

(use-package org-modern
  :ensure t
  :hook
  (org-modern-agenda . org-agenda-finalize)
  (org-modern-mode . (lambda () (setq buffer-face-mode-face '(:family "Iosevka")) (buffer-face-mode))))

(use-package htmlize :ensure t)

(provide 'markup)
