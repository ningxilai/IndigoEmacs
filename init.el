;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;;; Commentary:

;;; Code:

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
  :custom
  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory)
                initial-scratch-echo-area-message "iris")
  (context-menu-mode t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  :config
  (use-package package
    :init
    (package-initialize)
    (require 'package)
    :config
    (setq-default package-vc-allow-build-commands t
                  package-quickstart t
                  native-comp-jit-compilation t
                  package-native-compile t
                  version-control t
                  package-enable-at-startup t
                  delete-old-versions t
                  package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                                     ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                                     ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))

  (use-package use-package
    :init
    (require 'use-package-ensure)
    :config
    (setq-default use-package-always-ensure nil
                  use-package-always-defer t
                  use-package-expand-minimally t
                  use-package-vc-prefer-newest t))
  
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (pixel-scroll-precision-mode 1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1)
  
  (global-font-lock-mode t)
  (delete-selection-mode t)
  (global-auto-revert-mode t)
  (electric-pair-mode t)
  (global-so-long-mode t)
  (global-subword-mode t)
  (global-prettify-symbols-mode t)
  (auto-revert-mode t)
  (display-time-mode t)
  (which-key-mode t)

  (set-face-attribute 'default (selected-frame)
                      :height 120 :weight 'light :family "Lilex Nerd Font") ;; IBM Plex Mono
  (set-face-attribute 'bold nil :weight 'regular)
  (set-face-attribute 'bold-italic nil :weight 'regular)
  
  (set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal))
  (set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
  (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))
  
  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

  (setq-default text-mode-ispell-word-completion nil
                auto-save-list-file-prefix t
                auto-save-default nil
                make-backup-files nil
                create-lockfiles nil
                vc-follow-symlinks t
                use-short-answers t
                package-quickstart t
                load-prefer-newer t
                save-interprogram-paste-before-kill t
                find-file-suppress-same-file-warnings t)
  
  (setq-default c-set-style 'linux
                flymake-mode nil
                warning-minimum-level :warning
                kill-ring-max 200
                tab-always-indent 'complete
                resize-mini-windows 'grow-only)
  
  (setq-default indent-tabs-mode nil
                ring-bell-function 'ignore
                select-enable-clipboard t
                system-time-locale "C"
              ;; nice scroll
                scroll-step 1
                scroll-preserve-screen-position t
                scroll-margin 3
                scroll-conservatively 10
                maximum-scroll-margin 0.3
                scroll-up-aggressively 0.0
                scroll-down-aggressively 0.0
                pixel-scroll-precision-interpolate-page t)
  
  (setopt x-select-enable-clipboard t
          x-select-enable-primary nil
          interprogram-cut-function #'gui-select-text)
  
  (require 'nano)

  :bind (("C-x k" . kill-current-buffer)
         ("C-x C-r" .  recentf-open)
         ("M-n" . make-frame))
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

(use-package no-littering
  :vc (no-littering :url "https://github.com/emacscollective/no-littering"
                    :rev :newest)
  :init (require 'no-littering)
  :custom
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  :config
  (use-package recentf
    :defer t
    :commands (recentf-save-list)
    :config (recentf-mode t)
    :custom
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory)))
  
    (use-package savehist
      :custom
      ;; Minibuffer history
      (setq-default enable-recursive-minibuffers t ; Allow commands in minibuffers
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
      :init (savehist-mode t))
    (use-package saveplace
      :init
      (save-place-mode)
      (save-place-local-mode))
    )

;; Main

(use-package async
  :ensure t
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  :init
  (dired-async-mode t)
  (async-bytecomp-package-mode t))

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
  :config (setq treesit--install-language-grammar-out-dir-history "tree-sitter"))

(use-package paren
  :ensure nil
  :config
  (setq show-paren-delay 0.1
        show-paren-when-point-in-periphery t))

(use-package smartparens
  :ensure t
  :init
  (show-smartparens-global-mode t)
  :hook
  ((prog-mode text-mode markdown-mode) . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package rainbow-delimiters
  :ensure t
  :hook (smartparens-mode . rainbow-delimiters-mode))

(use-package region-occurrences-highlighter
  :ensure t
  :bind (:map region-occurrences-highlighter-nav-mode-map
              ("M-n" . region-occurrences-highlighter-next)
              ("M-p" . region-occurrences-highlighter-prev))
  :hook ((prog-mode org-mode text-mode) . region-occurrences-highlighter-mode))

(use-package icomplete
  :config
  (setq-default icomplete-vertical-mode t
                icomplete-matches-format nil
                icomplete-scroll t
                icomplete-in-buffer t)
  
  (setopt icomplete-delay-completions-threshold 0
          icomplete-compute-delay 0
          icomplete-show-matches-on-no-input t
          icomplete-hide-common-prefix nil
          icomplete-prospects-height 9
          icomplete-separator " . "
          icomplete-with-completion-tables t
          icomplete-max-delay-chars 0)
  :custom
  (setq-default completion-preview-ignore-case t
                completion-ignore-case t
                completion-auto-help t)
  )

(use-package flycheck
  :ensure t
  :config (flycheck-mode t)
  :hook (prog-mode . flycheck-mode))

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
  :init
  (vertico-mode t)
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  )

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

;; ProjectManager

(use-package magit :defer t :ensure t)
(use-package ghub :defer t :ensure t :demand t :after magit)
(use-package projectile :defer t :ensure t)
(use-package transient :defer t :ensure t)

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

(use-package nord-theme
  :vc (nord-theme :url "https://github.com/nordtheme/emacs.git"
                  :rev :newest)
  :init (load-theme 'nord t nil))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))
(use-package nerd-icons
  :ensure t)
(use-package nerd-icons-dired
  :ensure t
  :defer dired
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode t)
  :custom
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
  (indent-bars-treesit-scope '((python
                                function_definition
                                class_definition
                                for_statement
				if_statement
                                with_statement
                                while_statement)))
  :hook
  ((prog-mode yaml-mode) . indent-bars-mode))

(use-package composite
  :init
  (global-auto-composition-mode t)
  :hook
  ((text-mode) . (lambda () (setq-local auto-composition-mode nil
                                   buffer-face-mode-face '(:family "IBM Plex Mono"))))
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

;; dired

(use-package dired
  :ensure nil
  :config
  (setq-default dired-movement-style 'cycle
                browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  (setopt dired-listing-switches "-l --almost-all --human-readable --group-directories-first --no-group")
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
   ("M-e" . dirvish-emerge-menu))
  :custom
  (setq-local dirvish-attributes           ; The order *MATTERS* for some attributes
              '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
              dirvish-side-attributes
              '(vc-state nerd-icons collapse file-size)))

;; ends

;; Quarto

(use-package ess
  :ensure t
  :after quarto-mode)
(use-package quarto-mode
  :defer t
  :ensure t
  :mode (("\\.Rmd" . poly-quarto-mode)))

;; ends

;; Term

(use-package eshell
  :ensure t
  :hook (eshell-mode . completion-preview-mode)
  :custom
  (setq-local eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
  (setq-default eshell-prompt-function
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
  
  (setq-local eshell-highlight-prompt nil)
  
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
  (setq-default vterm-shell "zsh")
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "vterm @ %s" title) t))
  :hook
  (vterm-mode . (lambda()(set (make-local-variable 'buffer-face-mode-face) '(:family "FiraCode Nerd Font"))
                  (buffer-face-mode t)))
  (vterm-set-title-functions . vterm--rename-buffer-as-title))
(use-package multi-vterm :ensure t)
(use-package vterm-toggle
  :ensure t
  :after vterm
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

;; eglot

;; (use-package markdown-ts-mode :mode "\\.md\\'")

(use-package markdown-mode
  :defer t
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

(use-package eglot-booster
    :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster")
    :after eglot
    :config
    (eglot-booster-mode)
    (setq-local eglot-booster-io-only t))

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

;; (use-package yasnippet
;;   :ensure t
;;   :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
;;   :init (yas-global-mode t))
;; (use-package lsp-bridge
;;   :vc nil
;;   :autoload global-lsp-bridge-mode
;;   :load-path "site-lisp/lsp-bridge/"
;;   :init
;;   (global-lsp-bridge-mode)
;;   :config
;;   (setq lsp-bridge-python-command "~/.config/emacs/.venv/bin/python3")
;;   (setq lsp-bridge-markdown-lsp-server 'marksman)
;;   (setq acm-enable-yas t)
;;   ;; (setq acm-enable-citre t)
;;   (setq acm-candidate-match-function 'orderless-flex)
;;   :custom
;;   (acm-enable-capf t))

;; ends

(use-package reader
  :vc nil
  :autoload reader-autoloads
  :load-path "./site-lisp/reader/")

;;; init.el ends here
