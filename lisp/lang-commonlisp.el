;; -*- lexical-binding: t; -*-

;; CommonLisp

(use-package sly
  :ensure t
  :config
  (setq-local inferior-lisp-program "~/.roswell/impls/x86-64/linux/sbcl-bin/2.5.4/bin/sbcl"
              sly-net-coding-system 'utf-8-unix)
  )
(use-package sly-asdf :ensure t)
(use-package sly-quicklisp :ensure t)
(use-package sly-repl-ansi-color :ensure t)

(provide 'lang-commonlisp)
;; ends
