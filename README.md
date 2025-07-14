# Indigo Emacs

<!--toc:start-->
- [Indigo Emacs](#indigo-emacs)
  - [Acknowledge:](#acknowledge)
  - [Fonts](#fonts)
  - [LSP](#lsp)
    - [eglot](#eglot)
    - [LSP-Bridge](#lsp-bridge)
  - [Terminal](#terminal)
    - [Kitty](#kitty)
    - [VTerm](#vterm)
    - [EAT](#eat)
  - [Reader](#reader)
    - [build](#build)
    - [using](#using)
<!--toc:end-->

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Indigo Emacs](#indigo-emacs)
  - [Fonts](#fonts)
  - [LSP](#lsp)
    - [LSP-Bridge](#lsp-bridge)
  - [Terminal](#terminal)
    - [VTerm](#vterm)
    - [EAT](#eat)
  - [Reader](#reader)
    - [using](#using)

<!-- markdown-toc end -->

## Acknowledge:

- [IsaacGu](https://gujiaxi.github.io) [.emacs.d](https://github.com/gujiaxi/.emacs.d)
- [seagle0128](https://github.com/seagle0128) [Centaur](https://seagle0128.github.io/.emacs.d/)
- [snackon](https://snackon.github.io/) [Witchmacs](https://github.com/snackon/Witchmacs) / [pprobst](https://github.com/pprobst) [yukimacs](https://github.com/pprobst/yukimacs)
- [ltylty](https://emacs-china.org/u/ltylty/summary) [.emacs.d](https://github.com/ltylty/.emacs.d)
- [rougier](https://github.com/rougier/nano-emacs) [nano-emacs](https://gist.github.com/rougier/8d5a712aa43e3cc69e7b0e325c84eab4)
- [EmacsChina](https://emacs-china.org/c/emacs/5)
- [Cold's World](https://coldnight.github.io/dump-brain-with-emacs/)
- [Vertico](https://kristofferbalintona.me/posts/202202211546/)
- [By Mickey Petersen](https://www.masteringemacs.org/article/unicode-ligatures-color-emoji)
- etc..

## Fonts

There are two packages for handling ligatures in Emacs, namely the built-in `composite.el` and the external `ligature.el`. When you need to use a ligature font, this is how it is. If you do not enable either of these, `Ligalex Mono`/`Lilex` will behave the same as `IBM Plex Mono`. In this configuration, ligatures are enabled by default, so make sure you have a font that supports ligatures installed locally.

``` emacs-lisp
(set-face-attribute 'default (selected-frame)
                    :height 120 :weight 'light :family "Ligalex Mono") ;; IBM Plex Mono / Lilex Nerd Font
```

``` emacs-lisp
(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  :hook ((prog-mode vterm-mode) . ligature-mode)
  :init (global-ligature-mode nil))
```

## LSP
### eglot

``` bash
cargo install emacs-lsp-booster
```

``` emacs-lisp
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (setq-default eglot-booster-io-only t)
  :hook
  (eglot-ensure . (lambda()(eglot-booster-mode))))

 (use-package eglot
   :ensure (:host github :repo "joaotavora/eglot")
   :defer t
   :custom
   (eglot-autoshutdown t)  ;; shutdown language server after closing last file
   (eldoc-echo-area-use-multiline-p t) ;; eldoc-documentation-function should only return a single line
   :hook ((prog-mode . (lambda () (unless (derived-mode-p
                                      'emacs-lisp-mode 'lisp-mode
                                      'makefile-mode 'snippet-mode)
                               (eglot-ensure))))
          ((markdown-mode typst-ts-mode LaTeX-mode python-ts-mode c-ts-mode) . eglot-ensure))
   :config
   (add-to-list 'eglot-server-programs '(markdown-mode . ("vscode-markdown-language-server")))
   (add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
   (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab")))
   :init
   (setq read-process-output-max (* 1024 1024)) ; 1MB
   (setq eglot-autoshutdown t
         eglot-events-buffer-size 0
         eglot-send-changes-idle-time 0.5))
         
(use-package flycheck-eglot
  :after eglot
  :config
  (global-flycheck-eglot-mode))

(use-package consult-eglot
  :ensure t
  :after consult eglot
  :bind (:map eglot-mode-map
      ("C-M-." . consult-eglot-symbols)))

(use-package citre
  :ensure (:host github :repo "universal-ctags/citre")
  :config
  (require 'citre)
  (require 'citre-config)
  (setq-local
   ;; Set this if you want to always use one location to create a tags file.
   citre-default-create-tags-file-location 'global-cache
   ;; Set this if you'd like to use ctags options generated by Citre
   ;; directly, rather than further editing them.
   citre-edit-ctags-options-manually nil
   ;; If you only want the auto enabling citre-mode behavior to work for
   ;; certain modes (like `prog-mode'), set it like this.
   citre-auto-enable-citre-mode-modes '(prog-mode))
  :custom
  (setq-default
   ;; Set this if you use project management plugin like projectile.  It's
   ;; used for things like displaying paths relatively, see its docstring.
   citre-project-root-function #'projectile-project-root)
  :bind (("C-x c j" . citre-jump)
         ("C-x c J" . citre-jump-back)
         ("C-x c p" . citre-ace-peek)
         ("C-x c u". citre-update-this-tags-file)))
```

### LSP-Bridge

``` bash
uv lock --locked
```

``` emacs-lisp
;; lsp-bridge

(use-package markdown-mode
  :ensure t)
(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
  :init (yas-global-mode t))
(use-package lsp-bridge
  :vc nil
  :autoload global-lsp-bridge-mode
  :load-path "site-lisp/lsp-bridge/"
  :init
  (global-lsp-bridge-mode)
  :config
  (setq lsp-bridge-python-command "site-lisp/lsp-bridge/.venv/bin/python3")
  (setq lsp-bridge-markdown-lsp-server 'marksman)
  (setq acm-enable-yas t)
  ;; (setq acm-enable-citre t)
  (setq acm-candidate-match-function 'orderless-flex)
  :custom
  (acm-enable-capf t))
;;ends
```

## Terminal

### Kitty

[keyboard-protocol.rst]("https://github.com/kovidgoyal/kitty/blob/master/docs/keyboard-protocol.rst")

``` emacs-lisp
(when (display-graphic-p)
  (use-package kkp
    :vc (kkp :url "https://github.com/benotn/kkp.git"
             :rev :newest)
    :config (global-kkp-mode t))
)
```

### VTerm 

_If you always want to use the vendored version as opposed to the one on you system, set `USE_SYSTEM_LIBVTERM` to `no`._

### EAT

[EAT](https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/nongnu-devel/doc/eat.html)

``` bash
[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/zsh"
```

## Reader

by [reader](https://codeberg.org/divyaranjan/emacs-reader).

### build

``` bash
cd reader/dep/ && git clone --recursive --depth=1 git://git.ghostscript.com/mupdf.git
cd ../ && make all
```

### using

``` emacs-lisp
;; ViewTools

(use-package reader
  :vc (reader :url "https://codeberg.org/divyaranjan/emacs-reader"
              :rev :newest
  	          :make "all")
  :autoload reader-autoloads)

;; ends
```
