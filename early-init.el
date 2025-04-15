;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: nil; -*-
;;;Commentary:
;;;copy by seagle0128/CentaurEmacs && ltylty/.emacs.d && https://coldnight.github.io/dump-brain-with-emacs/

;;; Code:

(tab-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq gc-cons-threshold (* 100 1024 1024))
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)

(setq frame-inhibit-implied-resize t)

;; (setq default-frame-alist '((fullscreen . maximized)))
(setq default-frame-alist '((height . 49)
                            (width . 144)
                            (alpha-background . 100)))

(setq flymake-show-diagnostics-at-end-of-line 'short)

(setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))
(setq-default indent-tabs-mode nil)

(setq create-lockfiles t)
(setq ring-bell-function 'ignore)
;; nice scrolling
(setq scroll-step 1
      scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 10
      maximum-scroll-margin 0.3
      scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)

;; word wrap for CJK
(setq word-wrap-by-category t)
;; enable syntax highlight
(global-font-lock-mode t)
;; delete selection
(delete-selection-mode t)
;; auto revert external changes
(global-auto-revert-mode t)

(savehist-mode 1)  

(global-prettify-symbols-mode +1)

;; highlight current line
(global-hl-line-mode t)
;; display time in mode line
(display-time-mode t)

;; always load newest byte code
(setq load-prefer-newer t)

(setq system-time-locale "C")
;; set frame title
(setq frame-title-format "%b")
;; set a larger kill ring
(setq kill-ring-max 200)
;; use system clipboard
(setq save-interprogram-paste-before-kill t)
;; suppress warnings
(setq find-file-suppress-same-file-warnings t)

;; (add-to-list 'default-frame-alist
;; 	     '(font .  "IBM Plex Mono"))

(setopt use-short-answers t)

(setq native-comp-deferred-compilation t
      native-comp-jit-compilation t
      package-enable-at-startup nil
      use-package-always-ensure t
      package-native-compile t
      version-control t
      delete-old-versions t)

(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			user-emacs-directory))

(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ChineseLanguage
(set-language-environment "utf-8")
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8)
      locale-coding-system 'utf-8
      file-name-coding-system 'utf-8
      default-buffer-file-coding-system 'utf-8
      slime-net-coding-system 'utf-8-unix)

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")   

;; (setq warning-minimum-level :emergency)

;;; early-init.el ends here
