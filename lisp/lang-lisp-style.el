;; -*- lexical-binding: t; -*-

(use-package ruby-ts-mode
  :ensure nil
  :custom
  (ruby-indent-level 2)
  :config
  (add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rjs\\'" . ruby-mode)))



;; https://alexott.net/en/writings/emacs-devenv/EmacsScheme.html

(use-package slime
  :disabled
  :ensure t
  ;; :config (setq inferior-lisp-program "sbcl")
  :hook (slime-load . (lambda () (require 'slime-scheme))))

(use-package lsp-scheme
  :ensure t
  :hook (scheme-mode . lsp-scheme)
  :custom (lsp-scheme-implementation "guile"))

(provide 'lang-lisp-style)

;; ends here
