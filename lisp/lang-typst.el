;;; -*- lexical-binding: t; -*-
(use-package typst-ts-mode
  ;; https://github.com/Ziqi-Yang/tree-sitter-typst
  :ensure t
  :mode ("\\.typ'" . typst-ts-mode)
  :bind (:map typst-ts-mode-map
              ("C-c C-c" . typst-ts-tmenu))
  :config
  
  (add-to-list 'lsp-language-id-configuration '(".*\\.typ$" . "typst"))
  
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "tinymist")
                    :activation-fn (lsp-activate-on "typst")
                    :server-id 'tinymist))
  
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))

(use-package websocket :ensure t)

(use-package typst-preview
  :ensure (:host github :repo "havarddj/typst-preview.el")
  :bind (:map typst-ts-mode-map
              ("C-c C-t p" . typst-preview-start))
  :config
  (setq-local typst-preview-executable "tinymist preview"
              typst-preview-browser "default")
  )

; (setq-default eglot-workspace-configuration '(:tinymist (:exportPdf "onSave")))

(provide 'lang-typst)
;;; lang-typst ends here
