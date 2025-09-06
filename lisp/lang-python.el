;; -*- lexical-binding: t; -*-

(use-package python-ts-mode
  :ensure nil
  :mode
  "SConscript\\'" "SConstruct\\'"
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "--simple-prompt")
  (python-shell-completion-native-enable t)
  ;; (python-shell-prompt-detect-failure-warning nil)
  (python-indent-block-paren-deeper t)
  ;;(python-shell-dedicated 'project)  ;; This is better to set in a dir-locals file
  (python-forward-sexp-function nil)
  :bind (:map python-mode-map
              ("C-c C-z" . python-shell)))

(use-package flymake-ruff
  :ensure t
  :hook
  (python-mode . (lambda () (when (executable-find "ruff")
                      (flymake-ruff-load)))))

(use-package ruff-format :ensure t)

(use-package reformatter
  :ensure t
  :config
  (reformatter-define black :program "black" :args '("-")))

(use-package pip-requirements
  :ensure t)

(use-package toml-ts-mode
  :ensure nil
  :mode "poetry\\.lock\\'")


(provide 'lang-python)

;; ends here
