;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;;; Commentary:

;;; Code:

;; init

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

  (eval-and-compile

    (require 'xdg)

    (defvar elpaca-installer-version 0.11)
    (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
    (defvar elpaca-builds-directory (expand-file-name "emacs/elpaca/builds/" (xdg-cache-home)))
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
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/")
    (add-to-list 'load-path "~/.local/share/emacs/site-lisp/")

    (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
    :hook

    (before-save . kill-whitespace)

    :config

    (load custom-file :no-error-if-file-is-missing)

    (use-package use-package
      :ensure nil
      :init
      (setq-default use-package-always-ensure t
                    use-package-always-defer t
                    use-package-expand-minimally t
                    use-package-vc-prefer-newest t)
      :custom
      (use-package-enable-imenu-support t)
      (use-package-expand-minimally nil)
      (use-package-compute-statistics t)
      (use-package-verbose nil))

    (use-package text-mode
      :ensure nil
      :hook
      ((text-mode) . (lambda () (progn (abbrev-mode)
                                  (visual-line-mode)
                                  (setq-default auto-composition-mode nil truncate-lines nil))))
      :init
      (delete-selection-mode t)
      (toggle-truncate-lines t)
      :custom
      (sentence-end-double-space nil)
      (scroll-error-top-bottom t)
      (save-interprogram-paste-before-kill t))

    (setq-default frame-title-format
                  '(:eval (concat
	                   (if (and buffer-file-name (buffer-modified-p)) "•")
	                   (buffer-name)
	                   (if buffer-file-name
		               (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")")) " - Emacs")))

    (setq-default
     ;; nice scroll
     scroll-step 1
     scroll-preserve-screen-position t
     scroll-margin 3
     scroll-conservatively 10
     maximum-scroll-margin 0.3
     scroll-up-aggressively 0.0
     scroll-down-aggressively 0.0)

    (setopt pop-up-windows nil
            resize-mini-windows 'grow-only
            text-mode-ispell-word-completion nil
            vc-follow-symlinks t
            save-interprogram-paste-before-kill t
            kill-ring-max 200
            indent-tabs-mode nil
            select-enable-clipboard t
            select-enable-primary nil
            interprogram-cut-function #'gui-select-text
            Man-notify-method 'pushy
            ;; x-select-enable-clipboard t
            ;; x-select-enable-primary nil
            xref-search-program 'rg
            word-wrap-by-category t
            global-auto-revert-non-file-buffers t
            elisp-fontify-semantically nil)

    (setq-default use-short-answers t
                  find-file-suppress-same-file-warnings t
                  ;; line-spacing 0
                  warning-minimum-level :warning
                  load-prefer-newer t
                  ring-bell-function 'ignore
                  dabbrev-check-all-buffers nil
    	          dabbrev-ignored-buffer-regexps '("\\`[ *]")
                  hide-ifdef-shadow t
	          hide-ifdef-initially t
                  split-width-threshold 120
                  split-height-threshold nil)

    (setq-default ibuffer-default-sorting-mode 'recency  ;; can use alphabetic)
	          ibuffer-use-other-window t                ;; ibuffer in other windows
	          ibuffer-jump-offer-only-visible-buffers t
	          ibuffer-human-readable-size t)

    (with-no-warnings

      ;; copy by ldbeth

      (defun switch-to-scratch-buffer (&optional arg)
        "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
        (interactive "P")
        (let ((exists (get-buffer "*enlight*")))
          (if arg
              (switch-to-buffer-other-window (get-buffer-create "*enlight*"))
            (switch-to-buffer (get-buffer-create "*enlight*")))
          (unless (or exists
                      (eq major-mode initial-major-mode))
            (funcall initial-major-mode))))

      (global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)

      (defun logging-disabled-command (&optional cmd keys)
        (unless cmd (setq cmd this-command))
        (message "%s was disabled." cmd)
        (call-interactively cmd nil keys))

      (setq-default disabled-command-function #'logging-disabled-command)

      ;; copy by emacs/wiki
      (defun whitespace-killer-delete-end-trailing-blank-lines ()
        "Deletes all blank lines at the end of the file."
        (interactive)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-max))
            (delete-blank-lines))))

      ;; https://www.emacswiki.org/emacs/auto-save.el
      (defun whitespace-killer-delete-trailing-whitespace-except-current-line ()
        "Delete trailing whitespace except current line."
        (interactive)
        (let ((begin (line-beginning-position))
              (end (line-end-position)))
          (save-excursion
            (when (< (point-min) begin)
              (save-restriction
                (narrow-to-region (point-min) (1- begin))
                (delete-trailing-whitespace)))
            (when (> (point-max) end)
              (save-restriction
                (narrow-to-region (1+ end) (point-max))
                (delete-trailing-whitespace))))))

      (defun kill-whitespace ()
        "Kill whitespace."
        (interactive)
        (if (derived-mode-p 'markdown-mode)
            (whitespace-killer-delete-end-trailing-blank-lines)
          (whitespace-killer-delete-trailing-whitespace-except-current-line)))

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

      ;; Copy by https://www.emacswiki.org/emacs/ToggleWindowSplit

      (defun toggle-window-split ()
        (interactive)
        (if (= (count-windows) 2)
            (let* ((this-win-buffer (window-buffer))
                   (next-win-buffer (window-buffer (next-window)))
                   (this-win-edges (window-edges (selected-window)))
                   (next-win-edges (window-edges (next-window)))
                   (this-win-2nd (not (and (<= (car this-win-edges)
                                               (car next-win-edges))
                                           (<= (cadr this-win-edges)
                                               (cadr next-win-edges)))))
                   (splitter
                    (if (= (car this-win-edges)
                           (car (window-edges (next-window))))
                        'split-window-horizontally
                      'split-window-vertically)))
              (delete-other-windows)
              (let ((first-win (selected-window)))
                (funcall splitter)
                (if this-win-2nd (other-window 1))
                (set-window-buffer (selected-window-group) this-win-buffer)
                (set-window-buffer (next-window) next-win-buffer)
                (select-window first-win)
                (if this-win-2nd (other-window 1))))))

      (global-set-key (kbd "C-x b") 'toggle-window-split)

      ;; copy by emacs-china

      ;; fronzen emacs situation

      (defun spray-mode-usr2-handler ()
        "Handle case where spray mode timer is left running when the w3m
buffer it is spraying is killed inadvertently instead of stopping
spray mode first and won't respond to C-g or other mechanisms.
This will stop spray mode via an external signal: pkill -USR2
emacs.
SRC https://emacs.stackexchange.com/a/70000/37266 ."
        (interactive)
        ;; arbitrary elisp you wish to execute:
        (message "Got USR2 signal")
        (spray-stop))
      (global-set-key [signal usr2] 'spray-mode-usr2-handler)
      ;; ............. ^ here we register the event handler that will
      ;; automatically be called when send-usr2-signal-to-emacs fires

      (defun send-usr2-signal-to-emacs ()
        "Send pkill -USR2 emacs without command line.
SRC https://emacs.stackexchange.com/a/70000/37266 ."
        (interactive)
        (signal-process (emacs-pid) 'sigusr2))
      (global-set-key (kbd "M-G") 'send-usr2-signal-to-emacs)

      )

    (require 'lang-org)
    (require 'lang-chinese)
    (require 'lang-commonlisp)
    (require 'lang-c-style)

    :custom

    (global-auto-revert-mode t)
    (global-so-long-mode t)
    (global-subword-mode t)
    (global-prettify-symbols-mode t)

    ;; (initial-scratch-echo-area-message "iris")
    ;; (initial-major-mode 'org-mode)
    ;; (initial-scratch-message (concat "#+title: foo bar\n"
    ;;     			     "#+subtitle: Scratch Buffer\n\n"
    ;;     			     "The text in this buffer is not saved "
    ;;     			     "when exiting Emacs!\n\n"))
    ;; (inhibit-startup-message nil)

    :bind (("C-x k" . kill-current-buffer)
           ("C-x C-r" .  recentf-open))
    )




(use-package async
  :ensure t
  :config
  (async-bytecomp-package-mode t))

(use-package cond-let
  :ensure (:host github
                 :repo "tarsius/cond-let"))

(use-package gcmh
  :ensure t
  :diminish
  :init (gcmh-mode)
  :commands gcmh-mode
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

(use-package no-littering
  :ensure t
  :init
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  :config

  (setq-default backup-directory-alist '(expand-file-name "backups/" no-littering-expand-var-file-name)
                make-backup-files nil
                version-control t
                delete-old-versions t
                create-lockfiles nil)

  (use-package recentf
    :ensure nil
    :commands (recentf-save-list)
    :config
    (recentf-mode t)
    :custom
    (auto-revert-verbose nil)        ;; not show message when file changes
    (auto-revert-mode-text "")
    (auto-revert-avoid-polling t)   ;; don't do pooling for autorevert (use notifications).)
    (global-auto-revert-mode 1)

    (ffap-bindings)
    (ffap-machine-p-known 'accept)   ;; stop ffap from pinging random hosts
    (ffap-require-prefix t)          ;; require prefix for ffap

    (recentf-max-saved-items 48)     ;; Max items saved
    (recentf-auto-cleanup nil)       ;; Make cleanup when idle for 10 seconds. (default 'mode)
    :config

    (run-with-idle-timer 10 nil #'recentf-cleanup)

    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory)))

  (use-package savehist
    :ensure nil
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
    :ensure nil
    :init
    (save-place-mode)
    (save-place-local-mode)
    :config
    (setq save-place-ignore-files-regexp  ;; Modified to add /tmp/* files
          (replace-regexp-in-string "\\\\)\\$" "\\|^/tmp/.+\\)$"
			            save-place-ignore-files-regexp t t))
    :custom
    (auto-save-list-file-prefix t)
    (auto-save-visited-mode t)
    (delete-auto-save-files t)
    (auto-save-default t)
    ))



(use-package display
    :ensure nil
    :config
    (display-time-mode t)
    :custom
    (display-time-default-load-average nil)
    (display-time-day-and-date t)
    (display-time-24hr-format t))

(use-package pixel-scroll
  :ensure nil
  :custom
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-scroll-down t)
  (pixel-scroll-precision-scroll-up t)
  (pixel-scroll-precision-interpolate-page t)
  :init
  (defun +pixel-scroll-interpolate-down (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
      (pixel-scroll-interpolate-down)))

  (defun +pixel-scroll-interpolate-up (&optional lines)
    (interactive)
    (if lines
        (pixel-scroll-precision-interpolate (* lines (pixel-line-height)))
    (pixel-scroll-interpolate-up)))

  (defalias 'scroll-up-command #'+pixel-scroll-interpolate-down)
  (defalias 'scroll-down-command #'+pixel-scroll-interpolate-up))

(use-package whitespace
  :ensure nil
  :init
  (setq-default which-func-update-delay 0.2
                show-trailing-whitespace nil)
  :config
  (setq whitespace-style '(faces tab-mark missing-newline-at-eof)
	whitespace-display-mappings `((tab-mark ?\t [,(make-glyph-code ?» 'whitespace-tab) ?\t] )))

  :custom
  (whitespace-mode -1)
  (indicate-empty-lines nil)
  :hook
  (prog-mode . (lambda () (progn (setq show-trailing-whitespace t) (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))))



(use-package vundo
  :ensure t
  :bind ("C-x C-u" . vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "Unifont"))

(use-package undohist
  :ensure t
  :config (undohist-initialize))

(use-package dogears
  :ensure t
  :hook
  (prog-mode . dogears-mode)
  (text-mode . dogears-mode)
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-idle 1
        dogears-limit 200
        dogears-position-delta 20)
  (setq dogears-functions '(find-file
                            recenter-top-bottom
                            other-window
                            switch-to-buffer
                            toggle-window-split
                            windmove-do-window-select
                            pager-page-down
                            pager-page-up
                            tab-bar-select-tab
                            pop-to-mark-command
                            pop-global-mark
                            goto-last-change
                            xref-go-back
                            xref-find-definitions
                            xref-find-references)))



(use-package show-paren
  :ensure nil
  :config
  (show-paren-mode t)
  :custom
  (show-paren-delay 0.1)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t))

(use-package mic-paren
  :ensure t
  :init (paren-activate))

(use-package electric
  :ensure nil
  :init
  (electric-pair-mode t)
  (electric-indent-mode t)
  (electric-layout-mode t)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (electric-pair-text-pairs '((34 . 34)
                              (8216 . 8217)
                              (8220 . 8221)
                              (?\“ . ?\”)
                              (?\{ . ?\})
                              (?\《. ?\》)
                              (?\「. ?\」)
                              (?\< . ?\>)
                              (?\【. ?\】))))

(use-package electric-spacing
  :ensure (:host github
                 :repo "walmes/electric-spacing")
  :config
  (defvar my-electic-pair-modes '(python-mode julia-mode org-mode latex-mode))
  (defun my-inhibit-electric-pair-mode (char)
    (not (member major-mode my-electic-pair-modes)))
  (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode))



(use-package which-key
  :ensure nil
  :init
  (which-key-mode t)
  :custom
  (which-key-add-key-based-replacements
    "C-x a" "abbrev"
    "C-x v" "vc"
    "C-x r" "rectangle||register"
    "C-x n" "narrow"
    "C-x p" "project"
    "C-x RET" "coding-system"
    "C-x @" "event-apply-modifier"
    "C-x ESC" "repeat-command"
    "C-x 8" "unicode"
    "C-x x" "buffer"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"
    "C-x w" "window"
    "C-x C-k" "kmacro")
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order)
  (which-key-separator " ")
  (which-key-prefix-prefix "… ")
  (which-key-max-display-columns 3)
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(use-package helpful
    :ensure t
    :bind
    (("C-h f" . helpful-function)
     ("C-h x" . helpful-command)
     ("C-h k" . helpful-key)
     ("C-h v" . helpful-variable)))

(use-package centered-cursor-mode
  :ensure t
  :diminish
  :custom
  (ccm-vpos-init '(round (* 239 (ccm-visible-text-lines)) 408))
  :hook (help-mode))

(use-package eldoc
  :ensure nil
  :init
  (setq-default eldoc-idle-delay 2
	        eldoc-print-after-edit t
	        eldoc-minor-mode-string nil
	        eldoc-echo-area-display-truncation-message nil
                eldoc-documentation-function 'eldoc-documentation-compose)
  :config
  (global-eldoc-mode -1)
  :hook
  ((emacs-lisp-mode lisp-interaction-mode ielm-mode) . turn-on-eldoc-mode))

(use-package eldoc-box
  :ensure t
  :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
  :custom
  (eldoc-box-lighter nil)
  (eldoc-box-only-multi-line t)
  (eldoc-box-clear-with-C-g t)
  :custom-face
  (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
  (eldoc-box-body ((t (:inherit tooltip))))
  :hook
  ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
  :config
  ;; Prettify `eldoc-box' frame
  (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
        (alist-get 'right-fringe eldoc-box-frame-parameters) 8))



(use-package hl-line
  :ensure nil
  :config
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2))))
  :hook prog-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)

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

(use-package region-occurrences-highlighter
    :ensure t
    :bind (:map region-occurrences-highlighter-nav-mode-map
                ("M-n" . region-occurrences-highlighter-next)
                ("M-p" . region-occurrences-highlighter-prev))
    :hook ((prog-mode text-mode) . region-occurrences-highlighter-mode))

(use-package whitespace-cleanup-mode
    :ensure t
    :diminish
    :hook
    (prog-mode . whitespace-cleanup-mode)
    (prog-mode . (lambda () (setq-local show-trailing-whitespace t))))

(use-package lisp-extra-font-lock
    :ensure (:host github
                   :repo "apr3vau/lisp-extra-font-lock")
    :config
    (global-font-lock-mode t)
    :hook ((emacs-lisp-mode lisp-mode) . (lambda () (lisp-extra-font-lock-global-mode 1)))
    :custom
    (lisp-extra-font-lock-quoted ((t :foreground "grey50"))))

(use-package highlight-function-calls
    :ensure t
    :hook
    (emacs-lisp-mode . highlight-function-calls-mode))

(use-package sly-el-indent
    :ensure (:host github
                   :repo "cireu/sly-el-indent"
                   :files ("*.el" "lib/sly-cl-indent.el"))
    :hook ((emacs-lisp-mode lisp-mode) . (lambda () (sly-el-indent-setup))))

(use-package aggressive-indent
    :ensure (:host github
                   :repo "Malabarba/aggressive-indent-mode")
    :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package macrostep
    :ensure (:host github
                   :repo "emacsorphanage/macrostep")
    :bind-keymap ("C-c e" . macrostep-expand))



(use-package icomplete
    :ensure nil
    :preface
    (advice-add 'completion-at-point
                :after #'minibuffer-hide-completions)
    :init
    (setq tab-always-indent 'complete)  ;; Starts completion with TAB
    :custom
    (icomplete-delay-completions-threshold 0)
    (icomplete-compute-delay 0)
    (icomplete-show-matches-on-no-input t)
    (icomplete-hide-common-prefix nil)
    (icomplete-prospects-height 10)
    (icomplete-separator " . ")
    (icomplete-with-completion-tables t)
    (icomplete-in-buffer t)
    (icomplete-max-delay-chars 0)
    (icomplete-scroll t)
    :bind (:map icomplete-minibuffer-map
                ("C-n" . icomplete-forward-completions)
                ("C-p" . icomplete-backward-completions)
                ("C-v" . icomplete-vertical-toggle)
                ("RET" . icomplete-force-complete-and-exit))
    :hook
    (elpaca-after-init . (lambda ()
                           (fido-mode 1)
                           (icomplete-mode -1)
                           (icomplete-vertical-mode -1)
                           )))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t)
  (uniquify-separator "/"))



(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode))

(use-package yasnippet-snippets
  :ensure  t
  :after yasnippet
  :config (add-hook 'yas-global-mode  #'yasnippet-snippets-initialize))



(use-package treesit
    :ensure nil
    :preface
    (setq treesit-enabled-modes t)

    ;; At 3 (the default), too many users think syntax highlighting is broken or
    ;; simply "looks off."
    (setq treesit-font-lock-level 4)

    (save-match-data
      (dolist (sym '(auto-mode-alist interpreter-mode-alist))
        (set
         sym (cl-loop for (src . fn) in (symbol-value sym)
                   unless (and (functionp fn)
                               (string-match "-ts-mode\\(?:-maybe\\)?$" (symbol-name fn)))
                   collect (cons src fn)))))

    :init
    (setopt treesit-language-source-alist
            '((awk . ("https://github.com/Beaglefoot/tree-sitter-awk.git"))
              (bash       . ("https://github.com/tree-sitter/tree-sitter-bash.git"))
              (bibtex . ("https://github.com/latex-lsp/tree-sitter-bibtex.git"))
              (blueprint . ("https://github.com/huanie/tree-sitter-blueprint.git"))
              (commonlisp . ("https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"))
              (c          . ("https://github.com/tree-sitter/tree-sitter-c.git"))
              (cmake      . ("https://github.com/uyha/tree-sitter-cmake.git"))
              (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp.git"))
              (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
              (css        . ("https://github.com/tree-sitter/tree-sitter-css.git"))
              (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git"))
              (go         . ("https://github.com/tree-sitter/tree-sitter-go.git"))
              (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
              (html       . ("https://github.com/tree-sitter/tree-sitter-html.git"))
              (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
              (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript.git"))
              (json       . ("https://github.com/tree-sitter/tree-sitter-json.git"))
              (latex . ("https://github.com/latex-lsp/tree-sitter-latex.git"))
              (make . ("https://github.com/tree-sitter-grammars/tree-sitter-make.git"))
              (nu . ("https://github.com/nushell/tree-sitter-nu.git"))
              (org . ("https://github.com/milisims/tree-sitter-org.git"))
              (markdown   . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" nil "tree-sitter-markdown/src"))
              (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown.git" nil "tree-sitter-markdown-inline/src"))
              (perl . ("https://github.com/ganezdragon/tree-sitter-perl.git"))
              (proto . ("https://github.com/mitchellh/tree-sitter-proto.git"))
              (python     . ("https://github.com/tree-sitter/tree-sitter-python.git"))
              (r . ("https://github.com/r-lib/tree-sitter-r.git"))
              (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby.git"))
              (rust       . ("https://github.com/tree-sitter/tree-sitter-rust.git"))
              (sql . ("https://github.com/DerekStride/tree-sitter-sql.git" "gh-page"))
              (surface . ("https://github.com/connorlay/tree-sitter-surface.git"))
              (toml       . ("https://github.com/tree-sitter/tree-sitter-toml.git"))
              (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "tsx/src"))
              (typst      . ("https://github.com/uben0/tree-sitter-typst.git"))
              (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript.git" nil "typescript/src"))
              (verilog . ("https://github.com/gmlarumbe/tree-sitter-verilog.git"))
              (vhdl . ("https://github.com/alemuller/tree-sitter-vhdl.git"))
              (vue . ("https://github.com/tree-sitter-grammars/tree-sitter-vue.git"))
              (wast . ("https://github.com/wasm-lsp/tree-sitter-wasm.git" nil "wast/src"))
              (wat . ("https://github.com/wasm-lsp/tree-sitter-wasm.git" nil "wat/src"))
              (wgsl . ("https://github.com/mehmetoguzderin/tree-sitter-wgsl.git"))
              (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml.git"))))

    :custom

    (toml-ts-mode-indent-offset 4)
    (cmake-ts-mode-indent-offset 4)
    (json-ts-mode-indent-offset 4)
    (go-ts-mode-indent-offset 4)

    (treesit-font-lock-level 4)
    (treesit--indent-verbose t)
    (treesit--font-lock-verbose nil)
    (major-mode-remap-alist
     '((c-mode          . c-ts-mode)
       (c++-mode        . c++-ts-mode)
       (c-or-c++-mode   . c-or-c++-ts-mode)
       (cmake-mode      . cmake-ts-mode)
       (conf-toml-mode  . toml-ts-mode)
       (csharp-mode     . csharp-ts-mode)
       (css-mode        . css-ts-mode)
       (java-mode       . java-ts-mode)
       (js-mode         . js-ts-mode)
       (js-json-mode    . json-ts-mode)
       (python-mode     . python-ts-mode)
       (sh-mode         . bash-ts-mode)
       (typescript-mode . typescript-ts-mode))))

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



;; (use-package flyspell
;;   :ensure nil
;;   :init
;;   (setq-default ispell-following-word t ;;Check word around point not only before
;; 	        ispell-quietly t)       ;; Suppress messages in ispell-word

;;   (setq-default flyspell-delay-use-timer t      ;; New flyspell behavior
;; 	        flyspell-use-meta-tab nil       ;; Not correct with M-TAB
;; 	        flyspell-mode-line-string nil   ;; Not show Fly in modeline
;; 	        flyspell-delay 1                ;; default 3
;; 	        flyspell-sort-corrections t     ;; Alphabetically sort corrections
;; 	        flyspell-issue-welcome-flag nil ;; no message on start
;; 	        flyspell-issue-message-flag nil) ;; no message when checking

;;   :config
;;   (ispell-set-spellchecker-params)
;;   (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)

;;   :custom
;;   (ispell-program-name "hunspell")
;;   (ispell-dictionary ews-hunspell-dictionaries)
;;   (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
;;   (org-fold-core-style 'overlays) ;; Fix Org mode bug

;;   :hook
;;   (prog-mode . flyspell-prog-mode)p
;;   (text-mode . turn-on-flyspell)
;;   :bind (:map flyspell-mode-map
;;               ("C-;" . flyspell-correct-wrapper) ;; flyspell-auto-correct-previous-word
;;               ("C-c f r" . flyspell-region)
;;               ("C-c f b" . flyspell-buffer)
;;               ("C-c f n" . flyspell-goto-next-error)
;;               ))

(use-package jinx
  :diminish
  :hook (text-mode . jinx-mode)
  :bind ("M-$" . jinx-correct)
  :custom (jinx-languages "en_US")
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc")))

(use-package flymake
  :ensure t
  :hook
  (flymake-mode . prog-mode)
  (flymake-mode . flymake-flycheck-auto)
  :init
  (setq-default flymake-fringe-indicator-position 'right-fringe
                flymake-no-changes-timeout 1.05
	        flymake-wrap-around nil
	        flymake-show-diagnostics-at-end-of-line 'short ;; 'fancy
	        flymake-mode-line-format nil
	        flymake-margin-indicators-string '((error "!" compilation-error)
						   (warning "!" compilation-warning)
						   (note "!" compilation-info)))
  :functions my-elisp-flymake-byte-compile
  :custom
  (flymake-mode t)
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile)
  :bind
  ("C-c k d" . flymake-show-diagnostic)
  ("C-c k b" . flymake-show-buffer-diagnostics)
  ("C-c k l" . flymake-switch-to-log-buffer))

(use-package flymake-flycheck
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers
                (append (default-value 'flycheck-disabled-checkers)
                        '(emacs-lisp emacs-lisp-checkdoc emacs-lisp-package sh-shellcheck))))



(use-package fontset
  :ensure nil
  :preface
  (set-face-attribute 'default (selected-frame)
                      :height 120
                      :weight 'light :family "Lilex Nerd Font") ;; SF Mono
  (set-face-attribute 'bold (selected-frame)
                      :weight 'regular)
  (set-face-attribute 'bold-italic (selected-frame)
                      :weight 'regular)
  :config

  (set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
  (set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

  (set-char-table-range composition-function-table ?f '(["\\(?:ff?[fijlt]\\)" 0 font-shape-gstring]))
  (set-char-table-range composition-function-table ?T '(["\\(?:Th\\)" 0 font-shape-gstring]))

  (set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal))
  (set-fontset-font t 'latin (font-spec :family "IBM Plex Mono" :weight 'light :slant 'normal))
  (set-fontset-font t 'greek (font-spec :family "Catrinity" :weight 'normal :slant 'normal))

  (set-fontset-font t 'emoji
                    (cond
                     ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
                     ((member "Symbola" (font-family-list)) "Symbola")
                     ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
                     ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
                     ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
                     ))

  (set-fontset-font t 'han
                    (cond
                     ((member "LXGW WenKai" (font-family-list)) "LXGW WenKai")
                     ((member "Zhuque Fangsong (technical preview)" (font-family-list)) "Zhuque Fangsong (technical preview)")
                     ((member "PingFang SC" (font-family-list)) "PingFang SC")
                     ((member "方正柳公权楷书 简繁" (font-family-list)) "方正柳公权楷书 简繁")
                     ((member "Sarasa Term SC Nerd" (font-family-list)) "Sarasa Term SC Nerd")
                     ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")
                     ((member "WenQuanYi Zen Hei" (font-family-list)) "WenQuanYi Zen Hei")
                     ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
                     ))

  (set-fontset-font t 'cjk-misc
                    (cond
                     ((member "Noto Serif CJK SC" (font-family-list)) "Noto Serif CJK SC")
                     ((member "Sarasa UI J" (font-family-list)) "Sarasa UI J")
                     ))
  (set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))
  (set-fontset-font t 'bopomofo (font-spec :family "Symbola" :weight 'normal :slant 'normal))

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
  :custom
  (face-font-rescale-alist `(("Symbola"             . 1.3)
                             ("Microsoft YaHei"     . 1.2)
                             ("WenQuanYi Zen Hei"   . 1.2)
                             ("Sarasa Term SC Nerd" . 1.2)
                             ("PingFang SC"         . 1.16)
                             ("Lantinghei SC"       . 1.16)
                             ("Kaiti SC"            . 1.16)
                             ("Yuanti SC"           . 1.16)
                             ("Apple Color Emoji"   . 0.91)))
  :custom-face
  (fixed-pitch ((t (:family "SF Mono"))))
  (fixed-pitch-serif ((t (:family "SF Mono")))) ;; New York
  (variable-pitch ((t (:family "SF Pro")))) ;; Helvetica Neue
  )

(use-package fontaine
  :ensure t
  :when (display-graphic-p)
  :hook (kill-emacs . fontaine-store-latest-preset)
  :init
  (setq fontaine-presets
        '((regular
           :default-height 120
           :default-weight regular
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.0)
          (large
           :default-height 140
           :default-weight normal
           :fixed-pitch-height 1.0
           :variable-pitch-height 1.05)
          (t
           :default-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
           :fixed-pitch-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
           :variable-pitch-family "Lilex Nerd Font Mono" ;; CaskaydiaCove Nerd Font Mono
           :italic-family "IBM Plex Mono" ;; CaskaydiaCove Nerd Font Mono Italic
           :blod-family "IBM Plex Serif" ;; CaskaydiaCove Nerd Font Mono Bold
           :variable-pitch-weight normal
           :bold-weight bold
           :italic-slant italic
           :line-spacing 0.1)))
  :config
  (require 'xdg)
  (setq fontaine-latest-state-file (expand-file-name "emacs/fontaine-latest-state.eld" (xdg-cache-home)))
  :custom
  ;; (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-set-preset 'regular))



(use-package transient
  :ensure (:host github :repo "magit/transient"))

(use-package hydra
  :ensure t)



(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :hook ((dired-mode . auto-revert-mode)
         (dired-mode . dired-omit-mode)
	 (dired-mode . dired-hide-details-mode))
  :custom
  (dired-async-mode t)
  (dired-dwim-target nil)
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'top)
  (dired-dwim-target 'dired-dwim-target-recent)
  (dired-isearch-filenames 'dwim)
  (dired-mouse-drag-files t)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-maybe-use-globstar t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-omit-files "^\\.[a-zA-Z0-9]+")

  (delete-by-moving-to-trash t)

  (image-dired-external-viewer "gimp")

  :bind
  (("C-c v i" . image-dired)
   (:map dired-mode-map
         ( "."     . dired-omit-mode)
         ("<mouse-2>" . dired-mouse-find-file)
         ("C-<return>" . image-dired-dired-display-external)))

  :init
  (setq-default dired-at-point-require-prefix t
                dired-movement-style 'cycle
                dired-listing-switches "-alh --group-directories-first --no-group" ;; "-agho --group-directories-first" "-goah --group-directories-first --time-style=long-iso"
                dired-auto-revert-buffer (lambda (dirname)
					 (and (not (file-remote-p dirname))
					      (dired-directory-changed-p dirname))))
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :custom
  (dirvish-attributes
   '(vc-state file-size git-msg subtree-state collapse file-time))
  (dirvish-side-attributes
   '(vc-state nerd-icons collapse file-size))
  (dirvish-mode-line-format
   '(:left (sort symlink) :right (vc-info yank index)))
  (dirvish-header-line-format
   '(:left (path) :right (free-space)))
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
   ("M-e" . dirvish-emerge-menu)))



(use-package enlight
  :ensure t
  :hook
  (enlight . (lambda () (hl-line-mode nil)))
  (doom-modeline-mode . (lambda () (setopt initial-buffer-choice #'enlight)))
  :config

  ;; copy by ldbeth

  (defun switch-to-scratch-buffer (&optional arg)
    "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
    (interactive "P")
    (let ((exists (get-buffer "*enlight*")))
      (if arg
          (switch-to-buffer-other-window (get-buffer-create "*enlight*"))
        (switch-to-buffer (get-buffer-create "*enlight*")))
      (unless (or exists
                  (eq major-mode initial-major-mode))
        (funcall initial-major-mode))))

  (global-set-key (kbd "C-c s") 'switch-to-scratch-buffer)
  :custom
  (enlight-content
   (concat
    (propertize "MENU" 'face 'highlight)
    "\n\n"
    (enlight-menu
     '(("\nOrg Mode"
	("Org-Agenda (current day)" (org-agenda nil "a") "a"))
       ("\nFolder"
	("dotfile folder" (dired "~/.config/hypr/") "h")
	("Downloads folder" (dired "~/Downloads") "d"))
       ("\nInit"
	("init.el" (dired "~/.config/emacs/") "i"))
       ("\nOther"
	("Projects" project-switch-project "p")))))))

(use-package doom-themes
  :ensure t
  :config
  (defun +nanolize (&rest args)
  (interactive)
  (let* ((background
          (plist-get (custom-face-attributes-get 'default nil)
                     :background))
         (mode-line
          (plist-get (custom-face-attributes-get 'mode-line nil)
                     :background))
         (mode-line-active
          (or (plist-get (custom-face-attributes-get 'mode-line-active nil)
                         :background)
              mode-line))
         (mode-line-inactive
          (or (plist-get (custom-face-attributes-get 'mode-line-inactive nil)
                         :background)
              mode-line)))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel))
      (set-face-attribute face nil :foreground background))
    (set-face-attribute 'mode-line-active nil
                        :box `(:line-width 1 :color ,mode-line-active :style nil))
    (set-face-attribute 'mode-line-inactive nil
                        :box `(:line-width 1 :color ,mode-line-inactive :style nil))))

  (+nanolize)
  (advice-add 'enable-theme :after '+nanolize)

  :custom
  (cursor-in-non-selected-windows nil)
  (window-divider-default-right-width 0)
  :hook (elpaca-after-init . (lambda () (progn (load-theme 'doom-nord t nil)
                                          (window-divider-mode 1)))))

(use-package doom-modeline
  :ensure t
  :hook
  ((elpaca-after-init . doom-modeline-mode)
  (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  (doom-modeline-mode . column-number-mode))
  :init
   ;; We display project info in the modeline ourselves
  (setq projectile-dynamic-mode-line nil)
  ;; Set these early so they don't trigger variable watchers
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        ;; Only show file encoding if it's non-UTF-8 and different line endings
        ;; than the current OSes preference
        doom-modeline-buffer-encoding 'nondefault
        doom-modeline-default-eol-type (if (featurep :system 'windows) 1 0)))

(use-package centaur-tabs
    :defer nil
    :ensure t
    :init
    (centaur-tabs-mode t)
    (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
    (centaur-tabs-headline-match)
    :preface
    (setq centaur-tabs-enable-key-bindings t)
    :config
    (setq centaur-tabs-style "bar"
          centaur-tabs-height 32
          centaur-tabs-set-icons t
          centaur-tabs-show-new-tab-button t
          centaur-tabs-set-modified-marker t
          centaur-tabs-show-navigation-buttons t
          centaur-tabs-set-bar 'under
          centaur-tabs-show-count nil
          ;; centaur-tabs-label-fixed-length 15
          ;; centaur-tabs-gray-out-icons 'buffer
          ;; centaur-tabs-plain-icons t
          x-underline-at-descent-line t
          centaur-tabs-left-edge-margin nil)
    ;; (centaur-tabs-enable-buffer-alphabetical-reordering)
    ;; (setq centaur-tabs-adjust-buffer-order t)
    (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
All buffer name start with * will group to \"Emacs\".
Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
       (cond
         ;; ((not (eq (file-remote-p (buffer-file-name)) nil))
         ;; "Remote")
         ((or (string-equal "*" (substring (buffer-name) 0 1))
              (memq major-mode '(magit-process-mode
                                 magit-status-mode
                                 magit-diff-mode
                                 magit-log-mode
                                 magit-file-mode
                                 magit-blob-mode
                                 magit-blame-mode
                                 )))
          "Emacs")
         ((derived-mode-p 'prog-mode)
          "Editing")
         ((derived-mode-p 'dired-mode)
          "Dired")
         ((memq major-mode '(helpful-mode
                             help-mode))
          "Help")
         ((memq major-mode '(org-mode
                             org-agenda-clockreport-mode
                             org-src-mode
                             org-agenda-mode
                             org-beamer-mode
                             org-indent-mode
                             org-bullets-mode
                             org-cdlatex-mode
                             org-agenda-log-mode
                             diary-mode))
          "OrgMode")
         (t
          (centaur-tabs-get-group-name (current-buffer))))))
    :hook
    (enlight-mode . centaur-tabs-local-mode)
    (term-mode . centaur-tabs-local-mode)
    (calendar-mode . centaur-tabs-local-mode)
    (org-agenda-mode . centaur-tabs-local-mode)
    (elpaca-after-init . centaur-tabs-buffer-groups)
    :bind
    ("C-<prior>" . centaur-tabs-backward)
    ("C-<next>" . centaur-tabs-forward)
    ("C-S-<prior>" . centaur-tabs-move-current-tab-to-left)
    ("C-S-<next>" . centaur-tabs-move-current-tab-to-right))



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
  :after corfu
  :ensure t
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
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



(use-package mwim
  :ensure t
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package filladapt
  :ensure t
  :custom (filladapt-mode t))

(use-package page-break-lines
  :ensure t
  :init (global-page-break-lines-mode t)
  :custom
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attriute 'default :family)))

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))



(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init (marginalia-mode t))

(use-package embark
  :ensure t
  :preface
  (eval-when-compile
    (defun posframe-display-buffer (buffer)
      (let ((default-fgc (face-attribute 'default :foreground))
	    (default-bgc (face-attribute 'default :background))
	    (hl (face-attribute 'highlight :background)))
	(when buffer (posframe-show
		      buffer
		      :position (point)
		      :poshandler 'posframe-poshandler-frame-center
		      :font-height 1.0
		      :font-width 1.0
		      :width 120
		      :height 20
		      :border-width 5
		      :left-fringe 20
		      :right-fringe 20
		      :border-color hl
		      :background-color default-bgc))))
    (defun embark-get-buffer-pos-display (orig-fun)
      (interactive)
      (let* ((orig-result (funcall orig-fun)))
	(lambda (&optional keymap targets prefix)
	  (let ((result (funcall orig-result keymap targets prefix)))
	    (when (and result (windowp result))
	      (posframe-display-buffer (window-buffer result))
	      (delete-window result))))))
    (advice-add #'embark-verbose-indicator :around #'embark-get-buffer-pos-display))
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("M-n" . embark-next-symbol)
  ("M-p" . embark-previous-symbol)
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (delete 'embark-target-flymake-at-point embark-target-finders))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :bind (:map minibuffer-mode-map
              ("C-c C-o" . embark-export)))

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-preview-current t)
  (corfu-quit-no-match 'separator) ;; or 't
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay '(0.4 . 0.2))
  :custom-face
  (corfu-border ((t (:inherit region :background unspecified))))
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  :bind
  ("M-/" . completion-at-point)
  ("M-SPC" . corfu-quick-complete)
  :hook ((prog-mode . corfu-mode)
         (corfu-mode . corfu-popupinfo-mode)))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  :hook
  (emacs-lisp-mode . (lambda ()
	               (setq-local completion-at-point-functions
			           (list (cape-capf-super
				          #'yasnippet-capf
				          #'cape-dabbrev
				          #'cape-file
				          #'elisp-completion-at-point
				          ))
			           cape-dabbrev-min-length 2
			           cape-dabbrev-check-other-buffers t
			           ))))

(use-package yasnippet-capf
  :after cape
  :ensure (:host github
           :repo "elken/yasnippet-capf")
  :init (add-to-list 'completion-at-point-functions #'yasnippet-capf))

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

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  :custom
  (list-matching-lines-jump-to-current-line t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package consult-lsp
  :ensure t
  :bind (:map lsp-mode-map
              ("C-M-." . consult-lsp-symbols)))

(use-package vertico
  :ensure t
  :init
  (vertico-mode t)
  (vertico-multiform-mode t)
  :config
  (add-to-list 'vertico-multiform-categories
             '(jinx grid (vertico-grid-annotate . 20) (vertico-count . 4)))
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha)
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize nil) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  )

(use-package vertico-posframe
  :ensure t
  :init (vertico-posframe-mode t)
  :commands
  vertico-posframe-mode
  :hook (vertico-mode)
  :config
  (defun posframe-poshandler-frame-center-near-bottom (info)
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (/ (+ (plist-get info :parent-frame-height)
                (* 2 (plist-get info :font-height)))
             3)))
  :custom
  (vertico-posframe-poshandler
   #'posframe-poshandler-frame-center-near-bottom)
  (vertico-posframe-parameters
   '((left-fringe  . 8)
     (right-fringe . 8))))

(use-package orderless
  :ensure t
  :config (setq orderless-style-dispatchers '(orderless-affix-dispatch))
  :custom
  (completion-styles '(flex orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package affe
  :demand t
  :after orderless
  :init
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (orderless-pattern-compiler input))
    (cons input (lambda (str) (orderless--highlight input str))))
  :config
  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler))



(use-package magit
  :commands magit-status
  :ensure t
  :hook (magit-post-refresh . diff-hl-magit-post))

(use-package diff-hl
  :ensure t
  :custom
  (global-diff-hl-mode t))

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package ghub
  :defer t
  :ensure t
  :after magit)

(use-package project
  :ensure nil
  :init
  (setq-default vc-follow-symlinks t          ;; Open links not open
	        vc-handled-backends '(Git Hg) ;; Only git or mercurial
	        vc-display-status nil         ;; No info on the modeline.
	        vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)" ;; disable vc on remotes
					     vc-ignore-dir-regexp
					     tramp-file-name-regexp))
  :config
  (add-to-list 'auto-mode-alist
	     '("\\.\\(ipe\\|qrc\\|svn\\)\\'" . xml-mode))
  :custom
  (project-mode-line t)
  (project-vc-include-untracked nil))

(use-package projection
  :ensure t
  :defer nil
  ;; Enable the `projection-hook' feature.
  :hook (elpaca-after-init . global-projection-hook-mode)

  ;; Require projections immediately after project.el.
  :config
  (with-eval-after-load 'project
    (require 'projection))

  ;; Uncomment if you want to disable prompts for compile commands customized in .dir-locals.el
  (put 'projection-commands-configure-project 'safe-local-variable #'stringp)
  (put 'projection-commands-build-project 'safe-local-variable #'stringp)
  (put 'projection-commands-test-project 'safe-local-variable #'stringp)
  (put 'projection-commands-run-project 'safe-local-variable #'stringp)
  (put 'projection-commands-package-project 'safe-local-variable #'stringp)
  (put 'projection-commands-install-project 'safe-local-variable #'stringp)

  ;; Access pre-configured projection commands from a keybinding of your choice.
  ;; Run `M-x describe-keymap projection-map` for a list of available commands.
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :ensure t
  ;; Allow interactively selecting available compilation targets from the current
  ;; project type.
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))



(use-package eshell
  :ensure nil
  :hook (eshell-mode . completion-preview-mode)
  :custom
  (eshell-history-append t)   ;; No override eshell history; append
  (eshell-highlight-prompt nil)
  (eshell-buffer-name "*Eshell*")

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

  (global-set-key (kbd "<s-C-return>") 'eshell-other-window)

  :custom
  (eshell-highlight-prompt nil))

(use-package vterm
  :ensure t
  :config
  (setq-default vterm-shell "zsh")
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (defun vterm--rename-buffer-as-title (title)
    (rename-buffer (format "vterm @ %s" title) t))
  :hook
  (shell-mode . ansi-color-for-comint-mode-on)
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



(use-package lsp-mode
  :ensure t
  :defer nil
  :commands (lsp lsp-deferred)
  :preface
  (setq lsp-warn-no-matched-clients nil)
  ;; Performace tuning
  ;; @see https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setenv "LSP_USE_PLISTS" "true")

  :init
  (setq-default lsp-use-plists t
                lsp-signature-auto-activate nil
                lsp-progress-spinner-type 'progress-bar-filled
                ;; For diagnostics
                lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)
                ;; For clients
                lsp-clients-python-library-directories '("~/uv/"))
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p
                           'emacs-lisp-mode
                           'lisp-mode
                           'makefile-mode
                           'snippet-mode
                           'ron-mode)
                    (lsp-deferred))))
   ((cmake-ts-mode markdown-mode yaml-mode yaml-ts-mode haskell-mode shell-script-mode) . lsp-deferred)
   (lsp-mode . (lambda ()
                 ;; Integrate `which-key'
                 (lsp-enable-which-key-integration)
                 (add-hook 'before-save-hook #'lsp-format-buffer t t)
                 (add-hook 'before-save-hook #'lsp-organize-imports t t)

                 (add-hook 'lsp-completion-mode-hook #'lsp-mode-setup-completion))))
  (imenu-after-jump . pulse-momentary-highlight-one-line)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ("C-c f" . lsp-format-region)
              ("C-c d" . lsp-describe-thing-at-point)
              ("C-c a" . lsp-execute-code-action)
              ("C-c r" . lsp-rename)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))

  :config
  (defun lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex

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
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-enable-links nil)                    ;; no clickable links
  (lsp-enable-folding nil)                  ;; use `hideshow' instead
  (lsp-enable-snippet nil)                  ;; no snippets, it requires `yasnippet'
  (lsp-enable-file-watchers nil)            ;; performance matters
  (lsp-enable-text-document-color nil)      ;; as above
  (lsp-enable-symbol-highlighting nil)      ;; as above
  (lsp-enable-on-type-formatting nil)       ;; as above
  (lsp-enable-indentation nil)
  (lsp-semantic-tokens-enable nil)          ;; optional
  (lsp-semantic-tokens-apply-modifiers nil) ;; don't override token faces
  (lsp-headerline-breadcrumb-enable nil)    ;; keep headline clean
  (lsp-modeline-code-actions-enable nil)    ;; keep modeline clean
  (lsp-modeline-diagnostics-enable nil)     ;; as above
  (lsp-modeline-workspace-status-enable nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                            :face 'lsp-headerline-breadcrumb-separator-face))
  (lsp-log-io nil)                          ;; debug only
  (lsp-auto-guess-root t)                   ;; Yes, I'm using projectile
  (lsp-completion-provider :none)           ;; don't add `company-capf' to `company-backends'
  (lsp-keep-workspace-alive nil)            ;; auto kill lsp server
  (lsp-eldoc-enable-hover nil)              ;; disable eldoc hover
  (lsp-completion-enable-additional-text-edit nil)

  ;; imenu

  (imenu-use-markers nil)
  (imenu-flatten 'annotation)
  (imenu-auto-rescan t)
  (imenu-max-item-length 256)
  )

(use-package lsp-ui
  :ensure t
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("C-c s-<return>" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook
  (lsp-mode)
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
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy))
  :custom
  (lsp-ui-doc-position 'top)
  (lsp-ui-sideline-show-diagnostics nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-delay 0.1)
  (lsp-ui-doc-show-with-cursor (not (display-graphic-p)))
  (lsp-ui-imenu-auto-refresh 'after-save)
  (lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                         ,(face-foreground 'font-lock-string-face)
                         ,(face-foreground 'font-lock-constant-face)
                         ,(face-foreground 'font-lock-variable-name-face)))
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning)))))

(use-package projectile
  :ensure t
  :hook
  (lsp-mode . (lambda () (projectile-mode +1)))
  :config (add-to-list 'projectile-project-root-files "stack.yaml"))

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :config
  (dap-tooltip-mode -1)
  (dap-auto-configure-mode 1)

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
    (dap-breakpoints-file ".cache/dap-breakpoints")
    (dap-utils-extension-path ".cache/dap-extension"))



(use-package shr-tag-pre-highlight
  :ensure t
  :after shr
  :config
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight))
  (when (version< emacs-version "26")
    (with-eval-after-load 'eww
      (advice-add 'eww-display-html :around
                  'eww-display-html--override-shr-external-rendering-functions)))

  (setq shr-tag-pre-highlight-lang-modes
        '(("ocaml" . tuareg) ("elisp" . emacs-lisp) ("ditaa" . artist)
          ("asymptote" . asy) ("dot" . fundamental) ("sqlite" . sql)
          ("calc" . fundamental) ("C" . c) ("cpp" . c++) ("C++" . c++)
          ("screen" . shell-script) ("shell" . sh) ("bash" . sh)
          ("rust" . rustic)
          ("awk" . bash)
          ("json" . "js")
          ;; Used by language-detection.el
          ("emacslisp" . emacs-lisp)
          ;; Used by Google Code Prettify
          ("el" . emacs-lisp))))

(use-package shrface
  :ensure t
  :init
  (setopt shr-cookie-policy nil
          shr-sliced-image-height 0.1)
  :config
  (defvar shrface-general-rendering-functions
    (append '((title . eww-tag-title)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta)
              (code . shrface-tag-code)
              (pre . shrface-shr-tag-pre-highlight))
            shrface-supported-faces-alist))

  (defvar shrface-nov-rendering-functions
    (append '((img . nov-render-img)
              (svg . nov-render-svg)
              (title . nov-render-title)
              (pre . shrface-shr-tag-pre-highlight)
              (code . shrface-tag-code)
              (form . eww-tag-form)
              (input . eww-tag-input)
              (button . eww-form-submit)
              (textarea . eww-tag-textarea)
              (select . eww-tag-select)
              (link . eww-tag-link)
              (meta . eww-tag-meta))
            shrface-supported-faces-alist))

  (defun shrface-shr-tag-pre-highlight (pre)
    "Highlighting code in PRE."
    (let* ((shr-folding-mode 'none)
           (shr-current-font 'default)
           (code (with-temp-buffer
                   (shr-generic pre)
                   ;; (indent-rigidly (point-min) (point-max) 2)
                   (buffer-string)))
           (lang (or (shr-tag-pre-highlight-guess-language-attr pre)
                     (let ((sym (language-detection-string code)))
                       (and sym (symbol-name sym)))))
           (mode (and lang
                      (shr-tag-pre-highlight--get-lang-mode lang))))
      (shr-ensure-newline)
      (shr-ensure-newline)
      (setq start (point))
      (insert
       ;; (propertize (concat "#+BEGIN_SRC " lang "\n") 'face 'org-block-begin-line)
       (or (and (fboundp mode)
                (with-demoted-errors "Error while fontifying: %S"
                  (shr-tag-pre-highlight-fontify code mode)))
         code)
       ;; (propertize "#+END_SRC" 'face 'org-block-end-line )
       )
      (shr-ensure-newline)
      (setq end (point))
      (pcase (frame-parameter nil 'background-mode)
        ('light
         (add-face-text-property start end '(:background "#D8DEE9" :extend t)))
        ('dark
         (add-face-text-property start end '(:background "#292b2e" :extend t))))
      (shr-ensure-newline)
      (insert "\n")))

  (defun shrface-remove-blank-lines-at-the-end (start end)
    "A fix for `shr--remove-blank-lines-at-the-end' which will remove image at the end of the document."
    (save-restriction
      (save-excursion
        (narrow-to-region start end)
        (goto-char end)
        (when (and (re-search-backward "[^ \n]" nil t)
                   (not (eobp)))
          (forward-line 1)
          (delete-region (point) (min (1+ (point)) (point-max)))))))

  (advice-add 'shr--remove-blank-lines-at-the-end :override #'shrface-remove-blank-lines-at-the-end)

  (if (string-equal system-type "android")
      (setq shrface-bullets-bullet-list '("▼" "▽" "▿" "▾"))
    (setq shrface-bullets-bullet-list '("▼" "▽" "▿" "▾")))
  (add-hook 'outline-view-change-hook 'shrface-outline-visibility-changed))

(use-package image-slicing
  :ensure
  (:host github
         :repo "ginqi7/image-slicing")
  :config
  (add-to-list 'shr-external-rendering-functions
             '(img . image-slicing-tag-img))
  (push #'image-slicing-mode eww-after-render-hook))

(use-package eww
  :ensure nil
  :config
  (require 'shrface)
  (defun shrface-eww-setup ()
    (unless shrface-toggle-bullets
      (shrface-regexp)
      (setq-local imenu-create-index-function #'shrface-imenu-get-tree)))

  (add-hook 'eww-after-render-hook #'eldoc-mode)
  (add-hook 'eww-after-render-hook #'eldoc-box-hover-mode)
  (add-hook 'eww-after-render-hook #'shrface-eww-setup))



(use-package writeroom-mode
  :ensure t
  :config
  ;; (setq writeroom-width 128
  ;;       writeroom-fringes-outside-margins t
  ;;       writeroom-fullscreen-effect t)
  :custom
  (writeroom-mode-line-toggle-position 'mode-line-format)
  (writeroom-bottom-divider-width 0)
  (writeroom-maximize-window t)
  (writeroom-mode-line nil)
  :bind (:map writeroom-mode-map
              ("C-M-<" . writeroom-decrease-width)
              ("C-M->" . writeroom-increase-width)
              ("C-M-=" . writeroom-adjust-width)))

(use-package visual-fill-column
  :commands visual-fill-column-mode
  :ensure (visual-fill-column
           :host codeberg
           :repo "tarsiiformes/visual-fill-column")
  :init
  (advice-add 'text-scale-increase :after #'visual-fill-column-adjust)
  (advice-add 'text-scale-decrease :after #'visual-fill-column-adjust)
  :config
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 100))

(use-package nov
  :ensure t
  :mode ("\\.[Ee][Pp][Uu][Bb]\\'" . nov-mode)
  :hook

  (nov-mode . (lambda () (progn
                      (face-remap-add-relative 'variable-pitch :family "Zhuque Fangsong (technical preview)" :height 1.0)
                      (text-scale-set +1)
                      (visual-line-mode 1)
                      (visual-fill-column-mode 1)
                      (writeroom-mode 1)
                      (toggle-frame-maximized))))

  :preface

  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))

  :config

  (defun shrface-nov-render-html ()
    (require 'eww)
    (let ((shrface-org nil)
          (shr-bullet (concat (char-to-string shrface-item-bullet) " "))
          (shr-table-vertical-line "|")
          (shr-width 7000) ;; make it large enough, it would not fill the column (use visual-line-mode/writeroom-mode instead)
          (shr-indentation 0) ;; remove all unnecessary indentation
          (tab-width 8)
          (shr-external-rendering-functions shrface-nov-rendering-functions)
          (shrface-toggle-bullets nil)
          (shrface-href-versatile t)
          (shr-use-fonts nil)           ; nil to use default font
          (shr-map nov-mode-map))

      ;; HACK: `shr-external-rendering-functions' doesn't cover
      ;; every usage of `shr-tag-img'
      (cl-letf (((symbol-function 'shr-tag-img) 'nov-render-img))
        (shr-render-region (point-min) (point-max)))
      ;; workaround, need a delay to update the header line
      ;; (run-with-timer 0.01 nil 'shrface-update-header-line)
      ;; workaround, show annotations when document updates
      ))

  (setq nov-render-html-function #'shrface-nov-render-html)

  (defun shrface-nov-setup ()
    (require 'shrface)
    (unless shrface-toggle-bullets
      (shrface-regexp))
    (set-visited-file-name nil t)
    (setq tab-width 8)
    (if (string-equal system-type "android")
        (setq-local touch-screen-enable-hscroll nil)))

  (add-hook 'nov-mode-hook #'shrface-nov-setup)

  :custom

  (nov-variable-pitch t)
  (nov-text-width 140)

  (nov-unzip-program (executable-find "bsdtar"))
  (nov-unzip-args '("-xC" directory "-f" filename)))

(use-package djvu
  :ensure t
  :mode ("\\.djvu\\'" . djvu-read-mode))

(use-package djvu3
  :after djvu
  :ensure (:host github :repo "dalanicolai/djvu3")
  :commands djvu-read-mode djvu-outline-mode djvu-occur-mode)



(use-package frimacs
  :ensure t
  :init
  (add-to-list 'load-path "~/.local/lib/fricas/emacs/")
  :hook
  (frimacs-process-mode . (lambda () (progn (require 'fricas-cpl) (require 'fricas)))))

(use-package idris2-mode
  :ensure (:host github
                 :repo "idris-community/idris2-mode")
  :custom (idris2-interpreter-path "~/.pack/bin/idris2"))



(use-package hyperbole
  :ensure (:host github :repo "emacsmirror/hyperbole")
  :init
  (setq-default hywiki-directory (concat no-littering-etc-directory "hywiki/"))
  :custom
  (hpath:external-display-alist-x
   (list (cons (format "\\.\\(%s\\)$"
                       hpath:external-file-suffixes)
               "open"))))

;; ends

;;; init.el ends here
