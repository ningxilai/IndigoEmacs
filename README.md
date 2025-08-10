# Indigo Emacs

<!--toc:start-->
- [Indigo Emacs](#indigo-emacs)
  - [Acknowledge:](#acknowledge)
  - [Fonts](#fonts)
  - [Flycheck](#flycheck)
  - [Auto-Completion](#auto-completion)
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

## Flycheck

add to `:config`

``` emacs-lisp
  (defcustom flycheck-elisp-noflycheck-marker ";noflycheck"
    "Flycheck line regions marked with this marker string are ignored."
    :type 'string
    :group 'flycheck)

  (defun flycheck-elisp-noflycheck (err)
    "Ignore flycheck if line of ERR contain value of  `flycheck-elisp-noflycheck-marker'."
    (save-excursion
      ;;(debug)
      (goto-char (cdr (flycheck-error-region-for-mode err 'symbols)))  
      (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
        (when (string-match-p flycheck-elisp-noflycheck-marker text) ;noflycheck
          (setq flycheck-current-errors (delete  err flycheck-current-errors )) 
          t
          ))
      )
    )

  (defun elisp-noflycheck-hook ()
    "Add the ;;;###noflycheck thing to elisp."
    (require 'flycheck)
    (add-hook 'flycheck-process-error-functions #'flycheck-elisp-noflycheck nil t)) 
  
  (add-hook 'emacs-lisp-mode-hook #'elisp-noflycheck-hook) ;noflycheck
```

添加` ;noflycheck` 在行尾以规避 `flycheck` 检查。

## Auto-Completion

``` emacs-lisp
(use-package completion
  :ensure nil
  :hook
  (prog-mode . completion-preview-mode)
  :custom
  (completion-preview-ignore-case t)
  (completion-ignore-case t)
  (completion-auto-wrap t)
  (completion-auto-help 'visible)
  (completion-auto-deselect t)
  (completion-auto-select 'second-tab)
  (completions-sort 'historical)
  (completions-format 'one-column)
  (completions-detailed t)
  (completion-show-help nil)
  (completion-styles '(substring partial-completion emacs22))
  (completions-max-height 15)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (minibuffer-completion-auto-choose nil)
  :config
  (global-auto-composition-mode t)
  (add-to-list 'completion-ignored-extensions ".hi")
  :bind (:map minibuffer-local-map
         ("<down>" . next-complete-history-element)
         ("<up>" . previous-complete-history-element)))

```

## Tramp

``` emacs-lisp
(use-package tramp
  :ensure nil
  :custom
  (tramp-auto-save-directory (expand-file-name "tramp-autosave-dir" user-emacs-directory))
  (tramp-default-method "ssh")                  ;; Already default
  (remote-file-name-inhibit-cache 60)            ;; Default 10
  (tramp-completion-reread-directory-timeout 120) ;; Default 10
  (password-cache-expiry 3600)                   ;; Cache for 1 hour
  (tramp-use-scp-direct-remote-copying t)        ;; copy directly between remote hosts
  (remote-file-name-inhibit-locks t)          ;; I know that different Emacs sessions are not modifying the same remote file
  (tramp-verbose (if init-file-debug 10 3))      ;; Default 3 always
  ;; (tramp-use-ssh-controlmaster-options nil) ;; use system control master.
  (tramp-use-connection-share nil)
  (tramp-completion-use-auth-sources nil)        ;; not use auth-sources in tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-process-environment
               (format "DISPLAY=%s" (getenv "DISPLAY=%s"))))
```

## LSP

### LSP-Mode

``` emacs-lisp
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :preface
  (setq lsp-warn-no-matched-clients nil)
  ;; Performace tuning
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setenv "LSP_USE_PLISTS" "true")
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p
                           'emacs-lisp-mode 'lisp-mode
                           'makefile-mode 'snippet-mode
                           'ron-mode)
                    (lsp-deferred))))
   ((markdown-mode yaml-mode yaml-ts-mode haskell-mode shell-script-mode) . lsp-deferred)
   (lsp-mode . (lambda ()
                 ;; Integrate `which-key'
                 (lsp-enable-which-key-integration)
                 (add-hook 'before-save-hook #'lsp-format-buffer t t)
                 (add-hook 'before-save-hook #'lsp-organize-imports t t)

                 (add-hook 'lsp-completion-mode-hook #'lsp-mode-setup-completion))))
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init (setq lsp-use-plists t

              lsp-keymap-prefix "C-c l"
              lsp-keep-workspace-alive nil
              lsp-signature-auto-activate nil
              lsp-modeline-code-actions-enable nil
              lsp-modeline-diagnostics-enable nil
              lsp-modeline-workspace-status-enable nil

              lsp-semantic-tokens-enable t
              lsp-progress-spinner-type 'progress-bar-filled

              lsp-enable-file-watchers nil
              lsp-enable-folding nil
              lsp-enable-symbol-highlighting nil
              lsp-enable-text-document-color nil

              lsp-enable-indentation nil
              lsp-enable-on-type-formatting nil

              ;; For diagnostics
              lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)

              ;; For clients
              lsp-clients-python-library-directories '("~/uv/")

              lsp-completion-provider :none)
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
     :config
     (setopt lsp-headerline-breadcrumb-enable nil
             lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)
             lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                            :face 'lsp-headerline-breadcrumb-separator-face))
     (with-no-warnings
       ;; Emacs LSP booster
       ;; @see https://github.com/blahgeek/emacs-lsp-booster
       (when (executable-find "emacs-lsp-booster")
         (defun lsp-booster--advice-json-parse (old-fn &rest args)
           "Try to parse bytecode instead of json."
           (or
            (when (equal (following-char) ?#)
              (let ((bytecode (read (current-buffer))))
                (when (byte-code-function-p bytecode)
                  (funcall bytecode))))
            (apply old-fn args)))
         (advice-add (if (progn (require 'json)
                                (fboundp 'json-parse-buffer))
                         'json-parse-buffer
                       'json-read)
                     :around
                     #'lsp-booster--advice-json-parse)

         (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
           "Prepend emacs-lsp-booster command to lsp CMD."
           (let ((orig-result (funcall old-fn cmd test?)))
             (if (and (not test?)                             ;; for check lsp-server-present?
                      (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                      lsp-use-plists
                      (not (functionp 'json-rpc-connection))  ;; native json-rpc
                      (executable-find "emacs-lsp-booster"))
                 (progn
                   (message "Using emacs-lsp-booster for %s!" orig-result)
                   (cons "emacs-lsp-booster" orig-result))
               orig-result)))
         (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
         )
       )
     )

   (use-package lsp-ui
     :ensure t
     :custom-face
     (lsp-ui-sideline-code-action ((t (:inherit warning))))
     :bind (("C-c u" . lsp-ui-imenu)
            :map lsp-ui-mode-map
            ("C-c s-<return>" . lsp-ui-sideline-apply-code-actions)
            ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
            ([remap xref-find-references] . lsp-ui-peek-find-references))
     :hook
     (lsp-mode . lsp-ui-mode)
     :init
     (setq lsp-ui-doc-position 'top
           lsp-ui-sideline-show-diagnostics nil
           lsp-ui-sideline-ignore-duplicate t
           lsp-ui-doc-delay 0.1
           lsp-ui-doc-show-with-cursor (not (display-graphic-p))
           lsp-ui-imenu-auto-refresh 'after-save
           lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                 ,(face-foreground 'font-lock-string-face)
                                 ,(face-foreground 'font-lock-constant-face)
                                 ,(face-foreground 'font-lock-variable-name-face)))

     :config
     (with-no-warnings
       ;; Display peek in child frame if possible
       ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
       (defvar lsp-ui-peek--buffer nil)
       (defun lsp-ui-peek--peek-display (fn src1 src2)
         (if (childframe-workable-p)
             (-let* ((win-width (frame-width))
                     (lsp-ui-peek-list-width (/ (frame-width) 2))
                     (string (-some--> (-zip-fill "" src1 src2)
                               (--map (lsp-ui-peek--adjust win-width it) it)
                               (-map-indexed 'lsp-ui-peek--make-line it)
                               (-concat it (lsp-ui-peek--make-footer)))))
               (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
               (posframe-show lsp-ui-peek--buffer
                              :string (mapconcat 'identity string "")
                              :min-width (frame-width)
                              :internal-border-color (face-background 'posframe-border nil t)
                              :internal-border-width 1
                              :poshandler #'posframe-poshandler-frame-center))
           (funcall fn src1 src2)))
       (defun lsp-ui-peek--peek-destroy (fn)
         (if (childframe-workable-p)
             (progn
               (when (bufferp lsp-ui-peek--buffer)
                 (posframe-hide lsp-ui-peek--buffer))
               (setq lsp-ui-peek--last-xref nil))
           (funcall fn)))
       (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
       (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)))

(use-package lsp-scheme
  :ensure t
  :hook (scheme-mode . lsp-scheme)
  :custom (setq lsp-scheme-implementation "guile"))

(use-package lsp-haskell
  :ensure (:host github :repo "emacs-lsp/lsp-haskell" :autoloads nil)
  :init (setq-local lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :init
  (setq dap-breakpoints-file ".cache/dap-breakpoints"
        dap-utils-extension-path ".cache/dap-extension")
  :config
  (define-minor-mode +dap-running-session-mode
    "A mode for adding keybindings to running sessions"
    :init-value nil
    :keymap (make-sparse-keymap)
    (when +dap-running-session-mode
      (let ((session-at-creation (dap--cur-active-session-or-die)))
        (add-hook 'dap-terminated-hook
                  (lambda (session)
                    (when (eq session session-at-creation)
                      (+dap-running-session-mode -1)))))))

  ;; Activate this minor mode when dap is initialized
  (add-hook 'dap-session-created-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when hitting a breakpoint in another file
  (add-hook 'dap-stopped-hook #'+dap-running-session-mode)
  ;; Activate this minor mode when stepping into code in another file
  (add-hook 'dap-stack-frame-changed-hook (lambda (session)
                                            (when (dap--session-running session)
                                              (+dap-running-session-mode 1))))
    :hook
    (dap-mode . dap-ui-mode)
    (dap-ui-mode . dap-ui-controls-mode)
    :custom
    (dap-mode 1)
    (dap-ui-mode 1)
    (dap-tooltip-mode -1)
    (dap-ui-controls-mode 1))
    
(use-package consult-lsp
  :ensure t
  :bind (:map lsp-mode-map
              ("C-M-." . consult-lsp-symbols)))
```

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
   (add-to-list 'eglot-server-programs 
                '(markdown-mode . ("vscode-markdown-language-server")))
   (add-to-list 'eglot-server-programs 
                '(typst-ts-mode . ("tinymist")))
   (add-to-list 'eglot-server-programs 
                '(LaTeX-mode . ("texlab")))
   
   (add-to-list 'eglot-server-programs
                `(((cuda-mode :language-id "cpp")
                   (cuda-ts-mode :language-id "cpp"))
                  . ,(eglot-alternatives
                      '("clangd" "ccls"))))
   
   (add-to-list 'eglot-server-programs
                `((python-mode python-ts-mode) . ,(eglot-alternatives
                                                   '(("delance-langserver" "--stdio")
                                                     ("ruff" "server" "--preview")
                                                     "pylsp" "pyls" ("basedpyright-langserver" "--stdio")
                                                     ("pyright-langserver" "--stdio")
                                                     "jedi-language-server"))))

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

(use-package dape
  :defer t
  :ensure t
  :init
  (setq dape-adapter-dir ".cache/debug-adapters/")
  :config
  (setq dape-buffer-window-arrangement 'right
        dape-inlay-hints t
        dape-cwd-function #'projectile-project-root)

  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t)))
  :custom
  ;; Persist breakpoints after closing DAPE.
  (dape-breakpoint-global-mode +1)
  (dape-buffer-window-arrangment 'right))
  
(use-package gdb
  :ensure nil
  :custom
  (gdb-debug-log-max nil)   ;; no limit log
  (gdb-many-windows nil)
  (gdb-restore-window-configuration-after-quit t)
  (gdb-show-main t))

(use-package compilation-mode
  :ensure nil
  :custom
  (compilation-scroll-output nil)
  (compilation-context-lines t) ;; Don't scroll compilation buffer
  (compilation-always-kill t)
  :hook
  (compilation-mode . (lambda () (setq window-size-fixed 'width))))

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

``` emacs-lisp
(use-package eat
  :ensure t
  :config
  (add-to-list 'eat-semi-char-non-bound-keys
   (append (list (vector meta-prefix-char ?o)) eat-semi-char-non-bound-keys))
  :custom (eat-term-name "xterm-kitty")
  )

(setq eat-tramp-shells '(("ssh" . "/bin/bash")))

(setq explicit-shell-file-name "/bin/bash")
(setq tramp-default-remote-shell "/bin/bash")
```

## Reader

by [reader](https://codeberg.org/divyaranjan/emacs-reader).

``` bash
sudo pacman -S libmupdf
```

``` emacs-lisp
(use-package reader
    :ensure (reader :type git :host codeberg :repo "divyaranjan/emacs-reader"
        :files ("*.el" "render-core.so")
        :pre-build ("make" "all")))
;; ends
```
