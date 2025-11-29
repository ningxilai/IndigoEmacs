;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package rust-ts-mode
  :ensure nil
  :config
  (setq-local tab-width 4
              rust-format-on-save t)
  (setq ;; enable macro expansion
   lsp-rust-analyzer-proc-macro-enable t
   lsp-rust-analyzer-experimental-proc-attr-macros t)
  (add-hook 'rust-ts-mode-hook 'cargo-minor-mode)
  (add-hook 'rust-ts-mode-hook 'tree-sitter-hl-mode)
  :hook
  (rust-ts-mode . (lambda () (setq-local tab-width 4)))
  :custom
  (rust-ts-mode-indent-offset 4))

(use-package uiua-mode :mode "\\.ua\\'" :ensure t)

(use-package cargo :ensure t)
(use-package ron-mode :ensure t)
(use-package flycheck-rust :ensure t :hook (rust-ts-mode .  #'flycheck-rust-setup))

(provide 'lang-rust)

;; ends here
