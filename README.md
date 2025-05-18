Indigo Emacs
--------------------
Deps:

``` bash
#Install the python dependencies for LSP Bridge. (if you're using uv or pip).

uv pip install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
```

``` emacs-lisp
;; lsp-bridge

(use-package markdown-mode
  :ensure t)
(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1))
(use-package lsp-bridge
  :vc t
  :load-path "lsp-bridge"
  :init (global-lsp-bridge-mode))

;;ends

;; or elpaca

(use-package lsp-bridge
  :ensure (lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
            :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
            :build (:not compile))
  :init
  (global-lsp-bridge-mode))
```

--------------------
Acknowledge:

- IsaacGu .emacs.d
- seagle0128 Centaur
- snackon Witchmacs / pprobst yukimacs
- ltylty .emacs.d
- rougier nano-emacs
- EmacsChina forum Emacs-general
- [Cold's World](https://coldnight.github.io/dump-brain-with-emacs/)
- etc..

--------------------
eglot:

``` emacs-lisp
(use-package eglot
  :ensure t
  :defer t
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eldoc-echo-area-use-multiline-p t) ;; eldoc-documentation-function should only return a single line
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit nil :weight bold :foreground "yellow3"))))
  :hook
  ((typst-ts-mode) . eglot-ensure)
  ((markdown-mode) . eglot-ensure)
  ((LaTeX-mode) . eglot-ensure)
  ((python-ts-mode) . eglot-ensure)
  ((c-ts-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(markdown-mode . ("marksman")))
  (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
  (add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
  )
```

--------------------
[EAT](https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu-devel/doc/eat.html)

``` bash
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
```
--------------------
PackageManager:

``` emacs-lisp
;; -*- lexical-binding: t; -*-

;; Package Manager

(setq package-user-dir
      (expand-file-name
       (format "elpa/%s.%s"
           emacs-major-version emacs-minor-version)
       user-emacs-directory))

(require 'use-package-ensure)
(require 'package)

(setq package-quickstart t
      use-package-always-ensure nil
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-vc-prefer-newest t
      native-comp-deferred-compilation t
      native-comp-jit-compilation t
      package-native-compile t
      version-control t
      package-enable-at-startup t
      delete-old-versions t
      package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-activate-all)

(provide 'package-manager)
```
