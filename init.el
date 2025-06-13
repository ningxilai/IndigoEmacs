;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq flymake-mode nil
      text-mode-ispell-word-completion nil
      auto-save-list-file-prefix t
      make-backup-files nil
      create-lockfiles nil
      vc-follow-symlinks t
      use-short-answers t
      package-quickstart t
      warning-minimum-level :warning
      load-prefer-newer t
      save-interprogram-paste-before-kill t
      find-file-suppress-same-file-warnings t)

(setq c-set-style 'linux)
(setopt x-select-enable-clipboard t
        x-select-enable-primary nil
        interprogram-cut-function #'gui-select-text)
(setq kill-ring-max 200)

;; init

(use-package emacs
  :init
  (dolist (dir '("lisp" "site-lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path))
  (setq-default package-user-dir
                (expand-file-name
                 (format "elpa/%s.%s"
                         emacs-major-version emacs-minor-version)
                 user-emacs-directory))
  (package-activate-all)

  (defconst iris-emacs-cache-directory
    (concat user-emacs-directory ".cache/")
    "Copy by Spacemacs.")
  
  :config
  (require 'nano)
  (require 'lang-org)
  
  (use-package package
    :init
    (package-initialize)
    (require 'use-package-ensure)
    (require 'package)
    :config
    (setq package-vc-allow-build-commands t
          package-quickstart t
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
                             ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/"))))
  (use-package recentf
  :defer t
  :commands (recentf-save-list)
  :init
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                    (recentf-mode)
                                    (recentf-track-opened-file))))
  ;; Do not leave dangling timers when reloading the configuration.
  (when (and (boundp 'recentf-auto-save-timer)
               (timerp recentf-auto-save-timer))
    (cancel-timer recentf-auto-save-timer))
  (setq recentf-save-file (concat iris-emacs-cache-directory "recentf")
        recentf-max-saved-items 1000
        recentf-auto-cleanup 'never
        recentf-auto-save-timer (run-with-idle-timer 600 t
                                                     'recentf-save-list))
  :config
  (add-to-list 'recentf-exclude
               (recentf-expand-file-name iris-emacs-cache-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (when custom-file
      (add-to-list 'recentf-exclude (recentf-expand-file-name custom-file))))

(use-package savehist
  :init
  ;; Minibuffer history
  (setq savehist-file (concat iris-emacs-cache-directory "savehist")
        enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(search-ring
                                          regexp-search-ring
                                          extended-command-history
                                          kill-ring
                                          kmacro-ring
                                          log-edit-comment-ring)
        ;; We use an idle timer instead, as saving can cause
        ;; noticable delays with large histories.
          savehist-autosave-interval nil)
  (savehist-mode t))

(use-package saveplace
  :init
  ;; Save point position between sessions
  (setq save-place-file (concat iris-emacs-cache-directory "places"))
  (save-place-mode)
  (save-place-local-mode))

)

;; ends

(use-package gcmh
  :ensure t
  :diminish
  :init (setq gc-cons-threshold most-positive-fixnum)
  :hook (emacs-startup . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

;; Main

(use-package async
  :ensure t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1))

(use-package hl-line
  ;; :init (global-hl-line-mode t)
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2))))
  :hook ((prog-mode) . hl-line-mode))

(use-package vundo
  :ensure t
  :bind ("C-x C-u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols)
  :custom (set-face-attribute 'vundo-default nil :family "Unifont"))
(use-package undohist
  :ensure t
  :init (undohist-initialize))

(use-package treesit-auto
  :ensure t
  :config (global-treesit-auto-mode))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.1
        show-paren-when-point-in-periphery t))

(use-package smartparens
  :ensure t
  :init (show-smartparens-global-mode t)
  :hook
  ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (smartparens-mode . rainbow-delimiters-mode))

(use-package region-occurrences-highlighter
  :ensure t
  :bind (:map region-occurrences-highlighter-nav-mode-map
              ("M-n" . region-occurrences-highlighter-next)
              ("M-p" . region-occurrences-highlighter-prev))
  :hook ((prog-mode org-mode text-mode) . region-occurrences-highlighter-mode))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; ends

;; vertico

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :config (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s s" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :custom
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
)

(use-package vertico
  :ensure t
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package posframe :ensure t)
(use-package vertico-posframe
  :ensure t
  :init (vertico-posframe-mode 1)
  :config (setq vertico-multiform-commands
                '((consult-line
                   posframe
                   (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
                   (vertico-posframe-border-width . 10)
                   ;; NOTE: This is useful when emacs is used in both in X and
                   ;; terminal, for posframe do not work well in terminal, so
                   ;; vertico-buffer-mode will be used as fallback at the
                   ;; moment.
                   (vertico-posframe-fallback-mode . vertico-buffer-mode))
                  (t posframe))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ends

;; dired

(use-package dired
  :config
  (setq dired-movement-style 'cycle)
  (setq browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :config
  (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; ends

;; ProjectManager

(use-package magit :ensure t)
(use-package ghub :ensure t :demand t :after magit)
(use-package projectile :ensure t)
(use-package transient
  :ensure t
  :config
  (setq transient-history-file (concat iris-emacs-cache-directory "transient/history.el"))          
  (setq transient-levels-file (concat iris-emacs-cache-directory "transient/levels.el"))
  (setq transient-values-file (concat iris-emacs-cache-directory "transient/values.el"))
  )

;; ends

;; Menu

(use-package enlight
  :ensure t
  :hook (enlight . (lambda () (hl-line-mode nil)))
  :init (setopt initial-buffer-choice #'enlight)
  :custom
  (enlight-content
   (concat
    (propertize "MENU" 'face 'highlight)
    "\n\n"
    (enlight-menu
     '(("\nOrg Mode"
	("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("\nFolder"
	("Desktop folder" (dired "~/Desktop") "s")
	("Downloads folder" (dired "~/Downloads") "d"))
       ("\nInit"
	("init.el" (dired "~/.config/emacs/") "i"))
       ("\nOther"
	("Projects" project-switch-project "p"))))))
  )

;; ends

;; UI

(use-package nord-theme :ensure t :init (load-theme 'nord t nil))
(use-package doom-modeline :ensure t :init (doom-modeline-mode 1))
(use-package nerd-icons :ensure t)
(use-package nerd-icons-dired :ensure t :init (nerd-icons-dired-mode 1))
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode t)
  :config
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

(use-package indent-bars
  :ensure t
  :config
  (require 'indent-bars-ts)
  (setq indent-bars-display-on-blank-lines t
        indent-bars-width-frac 0.2
        indent-bars-zigzag nil
        indent-bars-highlight-current-depth nil
        indent-bars-pattern "|"
        indent-bars-prefer-character t)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.225))
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
				                       if_statement with_statement while_statement)))
  :hook
  ((prog-mode yaml-mode) . indent-bars-mode))

(use-package composite
  :init
  (global-auto-composition-mode nil)
  :hook
  ((prog-mode vterm-mode) . auto-composition-mode)
  :config
  (dolist (char/ligature-re
           `((?-  . ,(rx (or (or "-->" "-<<" "->>" "-|" "-~" "-<" "->") (+ "-"))))
             (?/  . ,(rx (or (or "/==" "/=" "/>" "/**" "/*") (+ "/"))))
             (?*  . ,(rx (or (or "*>" "*/") (+ "*"))))
             (?<  . ,(rx (or (or "<<=" "<<-" "<|||" "<==>" "<!--" "<=>" "<||" "<|>" "<-<" "<==" "<=<" "<-|" "<~>" "<=|" "<~~" "<$>" "<+>" "</>" "<*>" "<->" "<=" "<|" "<:" "<>"  "<$" "<-" "<~" "<+" "</" "<*")
                           (+ "<"))))
             (?:  . ,(rx (or (or ":?>" "::=" ":>" ":<" ":?" ":=") (+ ":"))))
             (?=  . ,(rx (or (or "=>>" "==>" "=/=" "=!=" "=>" "=:=") (+ "="))))
             (?!  . ,(rx (or (or "!==" "!=") (+ "!"))))
             (?>  . ,(rx (or (or ">>-" ">>=" ">=>" ">]" ">:" ">-" ">=") (+ ">"))))
             (?&  . ,(rx (+ "&")))
             (?|  . ,(rx (or (or "|->" "|||>" "||>" "|=>" "||-" "||=" "|-" "|>" "|]" "|}" "|=")
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

(use-package colorful-mode
  :ensure t
  :init (setq colorful-use-prefix t)
  :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode))
  :hook (after-init . global-colorful-mode))

;; ends

;; Quarto

(use-package ess :ensure t :after quarto-mode)
(use-package quarto-mode :ensure t :mode (("\\.Rmd" . poly-quarto-mode)))

;; ends

;; Term

(use-package eshell
    :hook (eshell-mode . completion-preview-mode)
    :config
    (setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
    (setq eshell-prompt-function
          (lambda nil
            (concat
             (if (string= (eshell/pwd) (getenv "HOME"))
                 (propertize "~" 'face `(:foreground "#99CCFF"))
               (replace-regexp-in-string
                (getenv "HOME")
                (propertize "~" 'face `(:foreground "#99CCFF"))
                (propertize (eshell/pwd) 'face `(:foreground "#99CCFF"))))
             (if (= (user-uid) 0)
                 (propertize " α " 'face `(:foreground "#FF6666"))
               (propertize " λ " 'face `(:foreground "#A6E22E"))))))
    
    (setq eshell-highlight-prompt nil)

    (defalias 'open 'find-file-other-window)
    (defalias 'clean 'eshell/clear-scrollback)
    
    (defun eshell/sudo-open (filename)
      "Open a file as root in Eshell."
      (let ((qual-filename (if (string-match "^/" filename)
                               filename
                             (concat (expand-file-name (eshell/pwd)) "/" filename))))
        (switch-to-buffer
         (find-file-noselect
      (concat "/sudo::" qual-filename)))))
    
    (defun eshell-other-window ()
      "Create or visit an eshell buffer."
      (interactive)
      (if (not (get-buffer "*eshell*"))
          (progn
            (split-window-sensibly (selected-window))
            (other-window 1)
            (eshell))
        (switch-to-buffer-other-window "*eshell*")))
    
    (global-set-key (kbd "<s-C-return>") 'eshell-other-window))

(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "zsh")
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "vterm @ %s" title) t))
  :hook
  (vterm-mode . (lambda()(set (make-local-variable 'buffer-face-mode-face) '(:family "FiraCode Nerd Font"))
                  (buffer-face-mode t)))
  (vterm-set-title-functions . vterm--rename-buffer-as-title))
(use-package multi-vterm :ensure t)
(use-package vterm-toggle
  :ensure t
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 ;;(display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 ;;(direction . bottom)
                 ;;(dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3))))

;; ends

;; LSP-BRIDGE

;; (use-package markdown-ts-mode :mode "\\.md\\'")

(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-open-command "firefox"
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-uppercase-checkbox t
        markdown-fontify-code-blocks-natively t
        markdown-gfm-additional-languages "Mermaid")
  :mode
  ("README\\.md\\'" . gfm-mode)
  :config
  (setq-default markdown-mode-font-lock-keywords
                (cl-remove-if
                 (lambda (item) (equal item '(markdown-fontify-tables)))
                 markdown-mode-font-lock-keywords))
  )
(use-package markdown-toc :ensure t :defer markdown-mode)

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
  :init (yas-global-mode t))

(use-package lsp-bridge
  :ensure nil
  :load-path "site-lisp/lsp-bridge/"
  :init
  (require 'lsp-bridge)
  (global-lsp-bridge-mode)
  :config
  (setq lsp-bridge-python-command "~/.config/emacs/.venv/bin/python3")
  (setq lsp-bridge-markdown-lsp-server 'marksman)
  (setq acm-enable-yas t)
  ;; (setq acm-enable-citre t)
  (setq acm-candidate-match-function 'orderless-flex)
  :custom
  (acm-enable-capf t))

;; ends

(setq initial-scratch-echo-area-message "iris")

;; init.el ends here
