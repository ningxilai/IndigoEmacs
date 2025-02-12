;;; package-enable-at-startup -*- no-byte-compile:t -*-
;; early-init.el

;; seagle0128/CentaurEmacs && ltylty/.emacs.d

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq gc-cons-threshold (* 100 1024 1024))

(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)
(setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))
(setq default-frame-alist '((fullscreen . maximized)))

(setopt use-short-answers t)

(setq native-comp-deferred-compilation t ;; obsolete since 29.1
      native-comp-jit-compilation t)

(setq gc-cons-threshold (* 32 1024 1024))

;; (add-to-list 'default-frame-alist
;; 	     '(font .  "IBM Plex Mono"))

(setq package-enable-at-startup t)

(setq-default mode-line-format t)

(setq inhibit-startup-message t)

(set-language-environment "UTF-8")


;; set unicode encoding
(prefer-coding-system 'utf-8)

;; no lockfile
(setq create-lockfiles nil)

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

;; suppress warnings
(setq find-file-suppress-same-file-warnings t)

;;; early-init.el ends here
