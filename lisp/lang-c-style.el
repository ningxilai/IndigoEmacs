;; -*- lexical-binding: t; -*-

(use-package c-ts-mode
  :ensure nil
  :init
  (setopt c-set-style 'linux)
  :config
  (defvaralias 'c-ts-mode-indent-offset 'tab-width)
  (setq-default c-default-style '((java-mode . "java")
                                  (awk-mode . "awk")
                                  (other . "linux")))
  (setq-default c-ts-mode-indent-style 'linux
                c-ts-mode-enable-doxygen nil))

(use-package rust-ts-mode
  :ensure nil
  :config
  (setq-local tab-width 4)
  :hook
  (rust-ts-mode . (lambda () (setq-local tab-width 4)))
  :custom
  (rust-ts-mode-indent-offset 4))

(use-package sh-mode
  :ensure nil
  :config
  (defvaralias 'sh-basic-offset 'tab-width)
  :hook
  (sh-mode . (lambda () (setq-local indent-tabs-mode t tab-width 4))))

(provide 'lang-c-style)

;; ends here
