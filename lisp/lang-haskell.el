;; Lang-Chinese --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package haskell-mode
  :ensure t
  :commands haskell-debug-mode
  :custom
  (haskell-process-suggest-remove-import-lines t)  ; warnings for redundant imports etc
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-show-overlays t)
  :hook
  (haskell-mode . (haskell-collapse-mode interactive-haskell-mode))
  (haskell-mode . (lambda()(setq-local yas-indent-line 'fixed)))
  (haskell-mode-local-vars . lsp-haskell)
  (haskell-literate-mode-local-vars . lsp-haskell))

(use-package haskell-ts-mode
  :ensure (haskell-ts-mode :repos "https://codeberg.org/pranshu/haskell-ts-mode")
  :custom
  (haskell-ts-font-lock-level 4)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  (haskell-ts-use-indent t)
  :config
  (add-to-list 'treesit-language-source-alist
   '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
   (treesit-install-language-grammar 'haskell)))

(use-package haskell-snippets
  :ensure t
  :custom
  (haskell-snippets-dir "elpaca/builds/haskell-snippets/snippets/haskell-mode"))

(use-package flycheck-haskell
  :ensure (:host github :repo "flycheck/flycheck-haskell" :default "*")
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)
  :commands flycheck-haskell-configure
  :hook (flycheck-mode .  flycheck-haskell-configure))

(use-package lsp-haskell
  :ensure (:host github
                 :repo "emacs-lsp/lsp-haskell"
                 :autoloads nil)
  :custom (lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
