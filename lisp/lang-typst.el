;; -*- lexical-binding: t; -*-

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

; (setq-default eglot-workspace-configuration '(:tinymist (:exportPdf "onSave")))

(provide 'lang-typst)
