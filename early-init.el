;; early-init --- early-init.el -*- no-byte-compile:t -*-
;;;Commentary:
;;;copy by seagle0128/CentaurEmacs && ltylty/.emacs.d

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
                            (alpha-background . 90)))

(setq flymake-show-diagnostics-at-end-of-line 'short)

(setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))
(setq-default indent-tabs-mode nil)

(set-language-environment "UTF-8")
;; set unicode encoding
(prefer-coding-system 'utf-8)
;; no lockfile
(setq create-lockfiles t)
;; no ring-bell
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

(setq warning-minimum-level :emergency)

;;; early-init.el ends here
