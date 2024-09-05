;;; init.el --- Isaac's Emacs configuration

;; Copyright (c) 2016-2022 Isaac Gu

;; Author: Isaac Gu <imjiaxi@gmail.com>
;; URL: https://github.com/gujiaxi/.emacs.d
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This is a personal Emacs configuration.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; -------------------------------------------------------------------
;; Package Bootstrap
;; -------------------------------------------------------------------

(put 'upcase-region 'disabled nil)

;; package repository
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                         ("org" . "https://mirrors.ustc.edu.cn/elpa/org/")
                         ))
(package-initialize)

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-interval 3)
  (setq auto-package-update-delete-old-versions t)
  )

;; Straight
(setq use-package-always-defer t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (load bootstrap-file nil 'nomessage))

;; -------------------------------------------------------------------
;; Basic settings
;; -------------------------------------------------------------------

;; Personal Infomation
(setq user-full-name "Isaac Gu")
(setq user-mail-address "imjiaxi@gmail.com")

;; set unicode encoding
(prefer-coding-system 'utf-8)

;; no lockfile
(setq create-lockfiles nil)

;; backup and autosave
(setq backup-directory-alist `((".*" . '(expand-file-name "backup" user-emacs-directory))))
(setq version-control t)
(setq delete-old-versions t)

;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/.Trash/")

;; clean startup
(setq inhibit-startup-message t)

;; initial buffer
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; no ring-bell
(setq ring-bell-function 'ignore)

;; nice scrolling
(setq scroll-margin 5)

;; use y/n instead of yes/no
(setq use-short-answers t)

;; word wrap for CJK
(setq word-wrap-by-category t)

;; enable syntax highlight
(global-font-lock-mode t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq-default default-tab-width 4)
(setq-default c-basic-offset 4)

;; delete selection
(delete-selection-mode t)

;; auto revert external changes
(global-auto-revert-mode t)

;; always load newest byte code
(setq load-prefer-newer t)

;; highlight current line
(global-hl-line-mode t)

;; display time in mode line
(display-time-mode t)
(setq system-time-locale "C")

;; set frame title
(setq frame-title-format "%b")

;; set a larger kill ring
(setq kill-ring-max 200)

;; use system clipboard
(setq save-interprogram-paste-before-kill t)

;; confirm before quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; suppress warnings
(setq find-file-suppress-same-file-warnings t)

;; custom directory
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;; -------------------------------------------------------------------
;; Built-in packages
;; -------------------------------------------------------------------

;; ----- customizations -----

;; abbrev [built-in]
(setq save-abbrevs nil)

;; calendar [built-in]
(setq calendar-location-name "Beijing, China")
(setq calendar-chinese-all-holidays-flag t)
(setq mark-diary-entries-in-calendar t)
(setq mark-holidays-in-calendar t)
(global-set-key (kbd "C-c k") 'calendar)

;; dired [built-in]
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")

;; display-line-numbers [built-in]
(setq display-line-numbers-type 'relative)
(mapc (lambda (hook) (add-hook hook 'display-line-numbers-mode))
      (list 'prog-mode-hook 'bibtex-mode-hook))

;; electric [built-in]
(electric-pair-mode t)
(electric-indent-mode t)
(electric-layout-mode t)

;; flyspell [built-in]
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; hideshow [built-in]
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; org-mode [built-in]
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(defun org-publish-site ()
  "A function for publishing a site.
The site configuration is defined in index.org."
  (interactive)
  (let ((index-file (expand-file-name "index.org" org-directory)))
    (find-file index-file)
    (org-babel-load-file index-file)
    (kill-this-buffer)))
(global-set-key (kbd "C-c p") 'org-publish-site)

;; savehist [built-in]
(savehist-mode t)

;; recentf [built-in]
(setq recentf-max-saved-items 500)
(recentf-mode t)

;; reftex [built-in]
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-horizontally t)

;; saveplace [built-in]
(save-place-mode t)

;; use-package [built-in]
(setq use-package-always-ensure t)

(use-package good-scroll
      :diminish
      :hook (after-init . good-scroll-mode)
      :bind (([remap next] . good-scroll-up-full-screen)
             ([remap prior] . good-scroll-down-full-screen)))

;; -------------------------------------------------------------------
;; Flymake
;; -------------------------------------------------------------------

;; flymake [built]
(use-package flymake
  :hook (prog-mode . flymake-mode))

;; -------------------------------------------------------------------
;; Prog Mode
;; -------------------------------------------------------------------

(use-package haskell-mode)

(use-package lua-mode
  :init 
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  )
  
;; -------------------------------------------------------------------
;; TeX
;; -------------------------------------------------------------------

;; auctex
(use-package tex
  :ensure auctex
  :after latex
  :config
  (add-to-list 'TeX-command-list '("Latexmk" "latexmk -xelatex -quiet %s" TeX-run-command nil t
                                   :help "Run latexmk"))
  (add-to-list 'LaTeX-clean-intermediate-suffixes "\\.fdb_latexmk")
  (with-eval-after-load 'evil
    (evil-set-initial-state 'TeX-output-mode 'emacs))
  :hook ((LaTeX-mode . turn-on-reftex)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . TeX-fold-mode))
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-clean-confirm nil))

(use-package auctex)
(use-package math-preview
   :custom (math-preview-command "~/node_modules/math-preview/"))

;;; dashboard
(setq initial-buffer-choice "~/.emacs.d/dashboard.org")

;; -------------------------------------------------------------------
;; Org
;; -------------------------------------------------------------------

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook
            '(lambda ()
               (visual-line-mode 1)))
  (setq org-startup-with-inline-images t)
  :init
  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window
	org-support-shift-select t)
  
  ;; I like to press enter to follow a link. mouse clicks also work.
  (setq org-return-follows-link t)
  )

(use-package org-modern
  :config
  (with-eval-after-load 'org (global-org-modern-mode))
  )

(use-package org-superstar
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
  )

(use-package htmlize
  :ensure t)
   
;; -------------------------------------------------------------------
;; UI-Packages
;; -------------------------------------------------------------------

;; ----- color theme -----

;; themes
(use-package doom-themes
  :init   (load-theme 'doom-nord t)
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

(use-package solaire-mode
  :init (solaire-global-mode +1))

;; mode-line
(use-package mini-modeline  
  :defer (add-hook 'nyan-mode 'mini-modeline-mode)
  :after nyan-mode
  :init (mini-modeline-mode t))

;; nyan-mode
(use-package nyan-mode
  :init (nyan-mode t))

;; -------------------------------------------------------------------
;; Completion System
;; -------------------------------------------------------------------

;; Vertico
(use-package vertico
  :init (vertico-mode)
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  :config
  )
  (use-package marginalia
    :init (marginalia-mode)
    :custom (marginalia-annotator-registry
             '((command marginalia-annotate-binding builtin none))))
  (use-package consult
    :custom (consult-preview-key "M-.")
    :bind (("C-x b" . consult-buffer)
           ("C-s" . consult-line)
           ("M-y" . consult-yank-pop)
           ("C-c i" . consult-imenu)))

;; -------------------------------------------------------------------
;; Auto Complete
;; -------------------------------------------------------------------

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-on-exact-match nil)
  (corfu-preview-current nil)
  :config (global-corfu-mode)
  )
  (use-package cape
    :config
    (add-to-list 'completion-at-point-functions #'cape-abbrev)
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-keyword)
    )
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless partial-completion)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))
    :custom (completion-styles '(substring orderless))
  )

;; -------------------------------------------------------------------
;; LSP (Language Server Protocol)
;; -------------------------------------------------------------------

;; eglot
(use-package eglot
  :custom
  (eglot-autoreconnect t)
   ;(eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  :hook (haskell-mode . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c C-e f" . eglot-format)
         ("C-c C-e i" . eglot-code-action-organize-imports))
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
 )

;; -------------------------------------------------------------------
;; Markdown
;; -------------------------------------------------------------------

;; markdown-mode
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom
  (markdown-command "pandoc --quiet --mathjax --no-highlight -f markdown")
  (markdown-css-paths '("view-source:https://sile-typesetter.org/main.css"))
  )


;; -------------------------------------------------------------------
;; Web
;; -------------------------------------------------------------------

;; web-mode
(use-package web-mode
  :mode (("\\.html?$" . web-mode)
         ("\\.jsx?$"  . web-mode)
         ("\\.php$"   . web-mode)
         ("\\.s?css$"  . web-mode)))

;; -------------------------------------------------------------------
;; Other packages
;; -------------------------------------------------------------------

;; Var

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; pdf
(use-package pdf-tools
  :init (pdf-tools-install)  ; Standard activation command
        (pdf-loader-install) ; On demand loading, leads to faster startup time
	)
(use-package pdf-view-restore
  :after pdf-tools
  :config
  (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode)
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore")
  )
		
;; aggressive-indent
(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode t))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :config (which-key-mode))

;; indent

;; This ensures that pressing Enter will insert a new line and indent it.
(global-set-key (kbd "RET") #'newline-and-indent)

;; Indentation based on the indentation of the previous non-blank line.
(setq-default indent-line-function #'indent-relative-first-indent-point)

;; In modes such as `text-mode', calling `newline-and-indent' multiple times
;; removes the indentation. The following fixes the issue and ensures that text
;; is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq-default indent-line-ignored-functions '())

(use-package indent-bars
  
  :commands indent-bars-mode
  :straight (indent-bars
             :type git
             :host github
             :repo "jdtsmith/indent-bars")
  :hook ((yaml-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)
         (python-mode . indent-bars-mode)
         (python-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-prefer-character nil))

(use-package outline-indent
  
  :commands (outline-indent-minor-mode
             outline-indent-insert-heading)
  :hook ((yaml-mode . outline-indent-minor-mode)
         (yaml-ts-mode . outline-indent-minor-mode)
         (python-mode . outline-indent-minor-mode)
         (python-ts-mode . outline-indent-minor-mode))
  :custom
  (outline-indent-ellipsis "->")
  :config (outline-indent-insert-heading))

(use-package dtrt-indent
  
  :commands (dtrt-indent-global-mode
             dtrt-indent-mode
             dtrt-indent-adapt
             dtrt-indent-undo
             dtrt-indent-diagnosis
             dtrt-indent-highlight)
  :config
  (dtrt-indent-global-mode))

(use-package block-nav
  )

;; Addon Utils

(use-package amx
  
  :init (amx-mode 1))

(use-package switch-window
	
	:config
	(setq switch-window-input-style 'minibuffer)
	(setq switch-window-increase 4)
	(setq switch-window-threshold 2)
	(setq switch-window-shortcut-style 'qwerty)
	(setq switch-window-qwerty-shortcuts
		  '("0" "1" "2" "3" "4" "5" "6"))
	:bind
	([remap other-window] . switch-window))

(use-package async)

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package format-all
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;; UI-Addons-Packages

(use-package page-break-lines
  :defer (add-hook 'after-init 'page-break-line-mode)
  :diminish (page-break-lines-mode visual-line-mode))

(use-package all-the-icons)

(use-package dimmer)

;; DeadGrep
(use-package deadgrep)

;; format
(use-package format-all
  :straight (:host github :repo "lassik/emacs-format-all-the-code")
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

;; typo
(use-package typo-mode
  :straight (:host github :repo "jorgenschaefer/typoel")
  :config
  (typo-global-mode 1)
  (add-hook 'text-mode-hook 'typo-mode)
  )

;; Eshell

(use-package fzf
  :bind
    ;; Don't forget to set keybinds!
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        ;; fzf/grep-command "rg --no-heading -nH"
        fzf/grep-command "grep -nrH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package eat
  :straight
  (:type git
		 :host codeberg
		 :repo "akib/emacs-eat"
		 :files ("*.el" ("term" "term/*.el") "*.texi"
				 "*.ti" ("terminfo/e" "terminfo/e/*")
				 ("terminfo/65" "terminfo/65/*")
				 ("integration" "integration/*")
				 (:exclude ".dir-locals.el" "*-tests.el")))
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  )

(use-package vterm
  :init (setq vterm-toggle-fullscreen-p nil)
  :config (setq vterm-shell "zsh"))
(use-package vterm-toggle)

;; Addon

(use-package dogears
  :straight t
  :hook (after-init . dogears-mode)
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
  (setq dogears-functions '(find-file recenter-top-bottom
                                      other-window switch-to-buffer
                                      aw-select toggle-window-split
                                      windmove-do-window-select
                                      pager-page-down pager-page-up
                                      tab-bar-select-tab
                                      pop-to-mark-command
                                      pop-global-mark
                                      goto-last-change
                                      xref-go-back
                                      xref-find-definitions
                                      xref-find-references)))

(use-package writeroom-mode
  :config
  (add-hook 'writeroom-mode #'logos-focus-mode)
  (add-hook 'writeroom-mode #'markdown-mode-hook)
  )
(use-package logos)

;; -------------------------------------------------------------------
;; Player
;; -------------------------------------------------------------------

;(use-package emms
;  :init (emms-all)
;  :config (setq emms-player-list '(emms-player-vlc))
;  )

;; -------------------------------------------------------------------
;; Other settings
;; -------------------------------------------------------------------

  ;; environment path
  (let ((envpath '("/usr/local/bin/" "~/.cargo/bin/" "~/.dotnet/tools/" "~/.local/bin/")))
    (setenv "PATH" (mapconcat 'identity (add-to-list 'envpath (getenv "PATH") t) ":"))
    (setq exec-path (append envpath exec-path)))
  ;; pair
  (global-prettify-symbols-mode t)
  (setq electric-pair-pairs '(
                              (?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")
                              ))
  (electric-pair-mode t)
  (global-hl-line-mode t)

  ;; Eshell
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

(setq eshell-highlight-prompt t)

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

;; symbols
(global-prettify-symbols-mode +1)

;; fonts
(setq fonts '("IBM Plex Mono" "Noto Sans Mono CJK JP"))
(set-fontset-font t 'unicode "Noto Sans" nil 'prepend)
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default nil :font
                    (format "%s:pixelsize=%d" (car fonts) 14))

;;; init.el ends here
