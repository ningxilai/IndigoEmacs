Indigo Emacs
--------------------

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Indigo Emacs](#indigo-emacs)
- [Acknowledge:](#acknowledge)
- [Fonts](#fonts)
- [LSP](#lsp)
  - [eglot](#eglot)
  - [LSP-Bridge](#lsp-bridge)
- [Terminal](#terminal)
  - [VTerm ](#vterm)
  - [EAT](#eat)
- [Reader](#reader)
  - [build](#build)
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

``` emacs-lisp
(use-package composite
  :init
  (global-auto-composition-mode nil)
  :hook
  (prog-mode . auto-composition-mode)
  :config
  (dolist (char/ligature-re
           `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<"
                                 "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>"
                                 "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+"
                                 "</" "<*")
                           (+ "<"))))
             (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  . ,(rx (+ "&")))
             (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>"
                                 "|]" "|}" "|=")
                             (+ "|"))))
             (?.  . ,(rx (or (or ".?" ".=" ".-" "..<") (+ "."))))
             (?+  . ,(rx (or "+>" (+ "+"))))
           (?\[ . ,(rx (or "[<" "[|")))
           (?\{ . ,(rx "{|"))
           (?\? . ,(rx (or (or "?." "?=" "?:") (+ "?"))))
           (?#  . ,(rx (or (or "#_(" "#[" "#{" "#=" "#!" "#:" "#_" "#?" "#(")
                           (+ "#"))))
           (?\; . ,(rx (+ ";")))
           (?_  . ,(rx (or "_|_" "__")))
           (?~  . ,(rx (or "~~>" "~~" "~>" "~-" "~@")))
           (?$  . ,(rx "$>"))
           (?^  . ,(rx "^="))
           (?\] . ,(rx "]#"))))
    (let ((char (car char/ligature-re))
          (ligature-re (cdr char/ligature-re)))
      (set-char-table-range composition-function-table char
                            `([,ligature-re 0 font-shape-gstring]))))
  )
```

## LSP
### eglot

``` bash
cargo install emacs-lsp-booster
```

``` emacs-lisp
(use-package eglot-booster
    :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
    :after eglot
    :config (eglot-booster-mode))

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
  (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab")))
  )

(use-package citre
  :vc (citre :url "https://github.com/universal-ctags/citre"
             :rev :newest)
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
uv venv -p 3.13.3 $HOME/.config/emacs/.venv && source $HOME/.config/emacs/.venv/bin/activate
```

``` bash
uv pip3 install epc orjson sexpdata six setuptools paramiko rapidfuzz watchdog packaging
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
  (setq lsp-bridge-python-command "~/.config/emacs/.venv/bin/python3")
  (setq lsp-bridge-markdown-lsp-server 'marksman)
  (setq acm-enable-yas t)
  ;; (setq acm-enable-citre t)
  (setq acm-candidate-match-function 'orderless-flex)
  :custom
  (acm-enable-capf t))
;;ends
```

## Terminal

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
