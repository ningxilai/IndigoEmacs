;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;;; Commentary:

;;; Code:

;; init

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))
  
  (eval-and-compile
    
    (defvar elpaca-installer-version 0.11)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
    (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
    (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                  :ref nil :depth 1 :inherit ignore
                                  :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                  :build (:not elpaca--activate-package)))
    (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           (default-directory repo))
      (add-to-list 'load-path (if (file-exists-p build) build repo))
      (unless (file-exists-p repo)
        (make-directory repo t)
        (when (<= emacs-major-version 28) (require 'subr-x))
        (condition-case-unless-debug err
            (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                      ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                      ,@(when-let* ((depth (plist-get order :depth)))
                                                          (list (format "--depth=%d" depth) "--no-single-branch"))
                                                      ,(plist-get order :repo) ,repo))))
                      ((zerop (call-process "git" nil buffer t "checkout"
                                            (or (plist-get order :ref) "--"))))
                      (emacs (concat invocation-directory invocation-name))
                      ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                            "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                      ((require 'elpaca))
                      ((elpaca-generate-autoloads "elpaca" repo)))
                (progn (message "%s" (buffer-string)) (kill-buffer buffer))
              (error "%s" (with-current-buffer buffer (buffer-string))))
          ((error) (warn "%s" err) (delete-directory repo 'recursive))))
      (unless (require 'elpaca-autoloads nil t)
        (require 'elpaca)
        (elpaca-generate-autoloads "elpaca" repo)
        (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
    (add-hook 'after-init-hook #'elpaca-process-queues)
    (elpaca `(,@elpaca-order))
    
    )

  (elpaca elpaca-use-package
	  ;; Enable Elpaca support for use-package's :ensure keyword.
	  (elpaca-use-package-mode))
  
  )

  (use-package emacs
    :init
    (dolist (dir '("lisp" "site-lisp"))
      (push (expand-file-name dir user-emacs-directory) load-path))
    (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
    :hook
    (elpaca-after-init . (lambda () (load custom-file 'noerror)))
    ((text-mode) . (lambda () (setq-local auto-composition-mode nil
                                     buffer-face-mode-face '(:family "IBM Plex Mono"))))

    :config
    
    (use-package fontset
      :init
      (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
      (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))
      :config
      (set-face-attribute 'default (selected-frame)
                          :height 120 :weight 'light :family "Lilex Nerd Font") ;; IBM Plex Mono
      (set-face-attribute 'bold nil :weight 'regular)
      (set-face-attribute 'bold-italic nil :weight 'regular)
      
      (set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal))
      (set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
      (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))
      
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
    
    (setq-default frame-title-format
                  '(:eval (concat
	                   (if (and buffer-file-name (buffer-modified-p)) "•")
	                   (buffer-name)
	                   (if buffer-file-name
		               (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")")) " - Emacs")))

    (eval-and-compile
      
      ;; copy by nano
      
      (defgroup nano-face nil "" :group 'faces)
      (defface nano-faded '((t)) "" :group 'nano-face)
      (defface nano-strong '((t)) "" :group 'nano-face)
      (defface nano-default '((t)) "" :group 'nano-face)
      (defface nano-faded-i '((t)) "" :group 'nano-face)
      (defface nano-strong-i '((t)) "" :group 'nano-face)
      (defface nano-default-i '((t)) "" :group 'nano-face)
      (defface nano-critical '((t)) "" :group 'nano-face)
      (defface nano-critical-i '((t)) "" :group 'nano-face)
      
      (defun nano-set-face (name &optional foreground background weight)
        "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT"
        
        (apply #'set-face-attribute `(,name nil
                                            ,@(when foreground `(:foreground ,foreground))
                                            ,@(when background `(:background ,background))
                                            ,@(when weight `(:weight ,weight))))
        (apply #'set-face-attribute `(,(intern (concat (symbol-name name) "-i")) nil
                                      :foreground ,(face-background 'nano-default)
                                      ,@(when foreground `(:background ,foreground))
                                      :weight regular)))
      
      (nano-set-face 'nano-strong "#ECEFF4" nil 'regular)
      (nano-set-face 'nano-faded "#90A4AE")
      (nano-set-face 'nano-critical "#EBCB8B")
      
      (setq-default header-line-format
                    '(:eval
                      (let ((prefix (cond (buffer-read-only     '("RO" . nano-default-i))
                                          ((buffer-modified-p)  '("**" . nano-critical-i))
                                          (t                    '("RW" . nano-faded-i))))
                            (mode (concat "(" (downcase (cond ((consp mode-name) (car mode-name))
                                                              ((stringp mode-name) mode-name)
                                                              (t "unknow")))
                                          " mode)"))
                            (coords (format-mode-line "%c:%l ")))
                        (list
                         (propertize " " 'face (cdr prefix)  'display '(raise -0.25))
                         (propertize (car prefix) 'face (cdr prefix))
                         (propertize " " 'face (cdr prefix) 'display '(raise +0.25))
                         (propertize (format-mode-line " %b ") 'face 'nano-strong)
                         (propertize mode 'face 'header-line)
                         (propertize " " 'display `(space :align-to (- right ,(length coords))))
                         (propertize coords 'face 'nano-faded)))))
      )
    
    (global-auto-composition-mode t)
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
    
    (context-menu-mode t)
    (tool-bar-mode -1)
    (menu-bar-mode -1)
    (blink-cursor-mode -1)
    (pixel-scroll-precision-mode 1)
    (scroll-bar-mode -1)
    (horizontal-scroll-bar-mode -1)
    
    (setq-default text-mode-ispell-word-completion nil
                  make-backup-files nil
                  create-lockfiles nil
                  vc-follow-symlinks t
                  use-short-answers t
                  load-prefer-newer t
                  truncate-lines nil
                  save-interprogram-paste-before-kill t
                  find-file-suppress-same-file-warnings t
                  project-mode-line nil)
    
    (setq-default c-set-style 'linux
                  warning-minimum-level :warning
                  kill-ring-max 200
                  tab-always-indent 'complete
                  resize-mini-windows 'grow-only
                  indent-tabs-mode nil
                  ring-bell-function 'ignore
                  select-enable-clipboard t
                  x-select-enable-clipboard t
                  x-select-enable-primary nil
                  interprogram-cut-function #'gui-select-text
                  initial-scratch-echo-area-message "iris")
    
    (setopt
     ;; nice scroll
     scroll-step 1
     scroll-preserve-screen-position t
     scroll-margin 3
     scroll-conservatively 10
     maximum-scroll-margin 0.3
     scroll-up-aggressively 0.0
     scroll-down-aggressively 0.0
     pixel-scroll-precision-interpolate-page t)
    
    (eval-and-compile
      
      ;; Copy by Seagle0128
      
      (defun childframe-workable-p ()
        "Whether childframe is workable."
        (and (>= emacs-major-version 26)
             (not noninteractive)
             (not emacs-basic-display)
             (or (display-graphic-p)
                 (featurep 'tty-child-frames))
             (eq (frame-parameter (selected-frame) 'minibuffer) 't)))
      
      (setopt blink-matching-paren-highlight-offscreen t
              show-paren-context-when-offscreen
              (if (childframe-workable-p) 'child-frame 'overlay))
      
      ;; copy by nano
      
      (defun nano-quit ()
        "Quit minibuffer from anywhere (code from Protesilaos Stavrou)."
        (interactive)
        (cond ((region-active-p) (keyboard-quit))
              ((derived-mode-p 'completion-list-mode) (delete-completion-window))
              ((> (minibuffer-depth) 0) (abort-recursive-edit))
              (t (keyboard-quit))))
      
      (global-set-key (kbd "C-g") 'nano-quit)
      
      (defun nano-kill ()
        "Delete frame or kill emacs if there is only one frame left."
        (interactive)
        (condition-case nil
            (delete-frame)
          (error (save-buffers-kill-terminal))))
      
      (global-set-key (kbd "C-x C-c") 'nano-kill)
      
      )

    (require 'lang-org)
    (require 'lang-chinese)
    
    :custom 
    (read-extended-command-predicate #'command-completion-default-include-p)
    (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
    
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
  :ensure t
  :demand t
  :init
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  :config
  (use-package recentf
    :commands (recentf-save-list)
    :config (recentf-mode t)
    :custom
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory)))
  
  (use-package savehist
    :config
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
    (save-place-local-mode)
    :config
    (setq-default
     auto-save-list-file-prefix t
     auto-save-visited-mode t
     delete-auto-save-files t
     auto-save-default t))
  )

;; Main

(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode t)
  :custom
  (dired-async-mode t))

(use-package hl-line
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2))))
  :hook prog-mode)

(use-package vundo
  :ensure t
  :bind ("C-x C-u" . vundo)
  :config (setq vundo-glyph-alist vundo-unicode-symbols)
  :custom (set-face-attribute 'vundo-default nil :family "Unifont"))
(use-package undohist
  :ensure t
  :custom (undohist-initialize))

(use-package paren
  :config
  (setq-default show-paren-delay 0.1
                show-paren-when-point-in-periphery t))

(use-package fingertip
  :ensure (:host github :repo "manateelazycat/fingertip")
  :hook (lisp-mode emacs-lisp-mode scheme-mode markdown-mode)
  :bind
  (:map fingertip-mode-map
        ("(" . fingertip-open-round)
        ("[" . fingertip-open-bracket)
        ("{" . fingertip-open-curly)
        (")" . fingertip-close-round)
        ("]" . fingertip-close-bracket)
        ("}" . fingertip-close-curly)
        ("=" . fingertip-equal)
        
        ("%" . fingertip-match-paren)
        ("\"" . fingertip-double-quote)
        ("'" . fingertip-single-quote)
        
        ("SPC" . fingertip-space)
        ("RET" . fingertip-newline)

        ("M-o" . fingertip-backward-delete)
        ("C-d" . fingertip-forward-delete)
        ("C-k" . fingertip-kill)
        
        ("M-\"" . fingertip-wrap-double-quote)
        ("M-'" . fingertip-wrap-single-quote)
        ("M-[" . fingertip-wrap-bracket)
        ("M-{" . fingertip-wrap-curly)
        ("M-(" . fingertip-wrap-round)
        ("M-)" . fingertip-unwrap)

        ("M-p" . fingertip-jump-right)
        ("M-n" . fingertip-jump-left)
        ("M-:" . fingertip-jump-out-pair-and-newline)
        
        ("C-j" . fingertip-jump-up)))

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

(use-package region-occurrences-highlighter
  :ensure t
  :bind (:map region-occurrences-highlighter-nav-mode-map
              ("M-n" . region-occurrences-highlighter-next)
              ("M-p" . region-occurrences-highlighter-prev))
  :hook ((prog-mode text-mode) . region-occurrences-highlighter-mode))

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
  )

(use-package completion
  :hook
  (prog-mode . completion-preview-mode)
  :config
  (setq-default completion-preview-ignore-case t
                completion-ignore-case t
                completion-auto-help t)
  :custom
  (add-to-list 'completion-ignored-extensions ".hi"))

(use-package flymake
  :hook
  (flymake-mode . prog-mode)
  (flymake-mode . flymake-flycheck-auto)
  :init
  (flymake-mode t)
  (setq-default flymake-no-changes-timeout nil
                flymake-fringe-indicator-position 'right-fringe
                flymake-show-diagnostics-at-end-of-line 'fancy
                eldoc-documentation-function 'eldoc-documentation-compose)
  :functions my-elisp-flymake-byte-compile
  :config
  
  (use-package flymake-flycheck
    :ensure t
  ;; Disable flycheck checkers for which we have flymake equivalents
    :config
    (use-package flycheck
      :ensure t
      :config
      (setq-default flycheck-disabled-checkers
                    (append (default-value 'flycheck-disabled-checkers)
                            '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck)))))
  
  ;; (use-package flycheck-eglot
  ;;   :ensure t
  ;;   :after eglot
  ;;   :config
  ;;   (global-flycheck-eglot-mode))
  
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile)
  )

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(use-package whitespace-cleanup-mode
  :ensure t
  :hook (elpaca-after-init . global-whitespace-cleanup-mode)
  :diminish t
  :init (setq-local show-trailing-whitespace t))

(use-package dired
  :ensure nil
  :config
  (setq-default dired-dwim-target t
                dired-recursive-deletes 'always
                dired-recursive-copies 'always)
  :init
  (setopt dired-movement-style 'cycle
          dired-listing-switches "-alh --group-directories-first --no-group")
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :config
  (setopt dirvish-attributes
          '(vc-state file-size git-msg subtree-state collapse file-time)
          dirvish-side-attributes
          '(vc-state nerd-icons collapse file-size)
          dirvish-mode-line-format '(:left (sort symlink) :right (vc-info yank index))
          dirvish-header-line-format '(:left (path) :right (free-space)))
  :init
  (dirvish-override-dired-mode)
  (dirvish-peek-mode)
  (dirvish-side-follow-mode)
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
  )

;; ends

;; Menu

(use-package enlight
  :ensure t
  :hook
  (enlight . (lambda () (hl-line-mode nil)))
  (elpaca-after-init . (lambda () (setopt initial-buffer-choice #'enlight)))
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
  :ensure t
  :init (load-theme 'nord t nil))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))
(use-package nerd-icons
  :ensure t)
(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))
(use-package nerd-icons-completion
  :ensure t
  :after vertico marginalia
  :config (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  :hook
  (vertico-mode . nerd-icons-completion-mode))
(use-package nerd-icons-corfu
  :ensure t
  :config
  ;; Optionally:
  (setq nerd-icons-corfu-mapping
        '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          ;; You can alternatively specify a function to perform the mapping,
          ;; use this when knowing the exact completion candidate is important.
          (file :fn nerd-icons-icon-for-file :face font-lock-string-face)
          ;; ...
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.
  
  ;; The Custom interface is also supported for tuning the variable above.
  )
(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode t)
  :custom
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attriute 'default :family)))

;; ends

;; vertico

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init (marginalia-mode t))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :bind ("M-/" . completion-at-point)
  :hook ((after-init . global-corfu-mode)
         (global-corfu-mode . corfu-popupinfo-mode)))

(use-package consult
  :ensure t
  :config (setq-default register-preview-delay 0.5)
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

(use-package vertico-posframe
  :ensure t
  :commands vertico-posframe-mode
  :hook (vertico-mode . vertico-posframe-mode)
  :custom
  (defun posframe-poshandler-frame-center-near-bottom (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (+ (plist-get info :parent-frame-height)
                (* 2 (plist-get info :font-height)))
             2)))
  (setopt vertico-posframe-poshandler
          #'posframe-poshandler-frame-center-near-bottom)
  :init
  (vertico-posframe-mode t)
  (setq-default vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; ends

;; Project

(use-package magit
  :defer t
  :ensure t
  :hook (magit-post-refresh . diff-hl-magit-post))

(use-package ghub
  :defer t
  :ensure t
  :after magit)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config (add-to-list 'projectile-project-root-files "stack.yaml")
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :ensure t
  :after projectile
  :init (counsel-projectile-mode))
  
;; (use-package project
;;   ;; https://emacs.liujiacai.net/post/010/
;;   ;; https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/
;;   :custom
;;   (defgroup project-local nil
;;     "Local, non-VC-backed project.el root directories."
;;     :group 'project)
  
;;   (defcustom project-local-identifier ".project"
;;     "You can specify a single filename or a list of names."
;;     :type '(choice (string :tag "Single file")
;;                    (repeat (string :tag "Filename")))
;;     :group 'project-local)

;;   (cl-defmethod project-root ((project (head local)))
;;     "Return root directory of current PROJECT."
;;     (cdr project))
  
;;   (defun project-local-try-local (dir)
;;     "Determine if DIR is a non-VC project DIR must include a file with the name determined by the variable `project-local-identifier' to be considered a project."
;;     (if-let* ((root (if (listp project-local-identifier)
;;                         (seq-some (lambda (n)
;;                                     (locate-dominating-file dir n))
;;                                   project-local-identifier)
;;                       (locate-dominating-file dir project-local-identifier))))
;;         (cons 'local root)))
  
;;   (setq-local project-find-functions '(project-local-try-local project-try-vc))
  
;;   (defun my/project-files-in-directory (dir)
;;     "Use `fd' to list files in DIR."
;;     (let* ((default-directory dir)
;;            (localdir (file-local-name (expand-file-name dir)))
;;            (command (format "fd -H -t f -0 . %s" localdir)))
;;       (project--remote-file-names
;;        (sort (split-string (shell-command-to-string command) "\0" t)
;;              #'string<))))
  
;;   (cl-defmethod project-files ((project (head local)) &optional dirs)
;;     "Override `project-files' to use `fd' in local projects."
;;     (mapcan #'my/project-files-in-directory
;;             (or dirs (list (project-root project)))))
;;   )

;; ends

;; Color & Face

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

(use-package colorful-mode
  :ensure t
  :init (setq-default colorful-use-prefix t)
  :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode))
  :hook (prog-mode . global-colorful-mode))

;; ends

;; Term

(use-package eshell
  :ensure nil
  :hook (eshell-mode . completion-preview-mode)
  :config
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
(use-package multi-vterm
  :ensure t)
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

;; lsp

(use-package lsp-mode
  :ensure t
  :diminish
  :defines (lsp-diagnostics-disabled-modes lsp-clients-python-library-directories)
  :autoload lsp-enable-which-key-integration
  :commands (lsp-format-buffer lsp-organize-imports)
  :preface
  (setq lsp-warn-no-matched-clients nil)
  ;; Performace tuning
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setenv "LSP_USE_PLISTS" "true")
  :hook ((prog-mode . (lambda ()
                        (unless (derived-mode-p
                                 'emacs-lisp-mode 'lisp-mode
                                 'makefile-mode 'snippet-mode
                                 'ron-mode)
                          (lsp-deferred))))
         ((markdown-mode yaml-mode yaml-ts-mode haskell-mode) . lsp-deferred)
         (lsp-mode . (lambda ()
                       ;; Integrate `which-key'
                       (lsp-enable-which-key-integration)
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t))))
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
              lsp-clients-python-library-directories '("~/uv/"))
     :config
     
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

         (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                        :face 'lsp-headerline-breadcrumb-separator-face))
         )
     :custom
     (setq-default lsp-headerline-breadcrumb-enable nil)
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
     :hook (lsp-mode . lsp-ui-mode)
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

(use-package dape
  :defer t
  :ensure t
  :init
  (setq dape-adapter-dir "var/debug-adapters/")
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
  (dape-buffer-window-arrangment 'right)
  )

(use-package consult-lsp
  :ensure t
  :bind (:map lsp-mode-map
              ("C-M-." . consult-lsp-symbols)))

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

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
  :init (yas-global-mode t))

;; ends

(use-package reader
  :vc t
  :autoload reader-autoloads
  :load-path "./site-lisp/reader/")

;;; init.el ends here
