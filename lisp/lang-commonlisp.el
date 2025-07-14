;; -*- lexical-binding: t; -*-

;; CommonLisp

(use-package sly
  :ensure t
  :config
  (use-package sly-asdf :ensure t)
  (use-package sly-quicklisp :ensure t)
  (use-package sly-repl-ansi-color :ensure t)
  (use-package sly-macrostep :ensure t)
  (setq-local inferior-lisp-program "~/.roswell/impls/x86-64/linux/sbcl-bin/2.5.4/bin/sbcl"
              sly-net-coding-system 'utf-8-unix
              sly-protocol-version 'ignore)
  (let ((features '(sly-fancy)))
    (sly-setup features))
  :bind (:map sly-repl-mode-map
              ("TAB" . indent-for-tab-command)
              ) 
  )

;; https://alexott.net/en/writings/emacs-devenv/EmacsScheme.html

(use-package slime
  :disabled
  :ensure t
  :config (setq inferior-lisp-program "sbcl")
  :hook (slime-load . (lambda () (require 'slime-scheme))))

(provide 'lang-commonlisp)
;; ends
