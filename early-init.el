;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: nil; -*-
;;;Commentary:
;;;copy by seagle0128/CentaurEmacs && ltylty/.emacs.d && https://coldnight.github.io/dump-brain-with-emacs/

;;; Code:

;; GC

(setq gc-cons-threshold (* 64 1024 1024))
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode)
(setq frame-inhibit-implied-resize t)

(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       'garbage-collect))

;; Activate / Deactivate modes

(tool-bar-mode -1)
(tab-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(icomplete-vertical-mode 1)
(pixel-scroll-precision-mode 1)

;; nice scrolling
(setq scroll-step 1
      scroll-preserve-screen-position t
      scroll-margin 3
      scroll-conservatively 10
      maximum-scroll-margin 0.3
      scroll-up-aggressively 0.0
      scroll-down-aggressively 0.0
      pixel-scroll-precision-interpolate-page t)

(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;;  (defalias 'yes-or-no-p 'y-or-n-p)

(setq flyspell-mode 1)

(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              select-enable-clipboard t)

;; Frame / windows layout & behavior

;; set frame title
; (setq frame-title-format "%b")
(setq frame-title-format
      '(:eval (concat
	       (if (and buffer-file-name (buffer-modified-p)) "•")
	       (buffer-name)
	       (if buffer-file-name
		   (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")")) " - Emacs"))
      )

(setq default-frame-alist
      '((height . 44)
        (width  . 81)
        (left-fringe . 0)
        (right-fringe . 0)
        (internal-border-width . 32)
        (vertical-scroll-bars . nil)
        (bottom-divider-width . 0)
        (right-divider-width . 0)
        (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)

(setq-default pop-up-windows nil)

(setq flymake-show-diagnostics-at-end-of-line 'short)

; (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))

(setq create-lockfiles nil)

(savehist-mode +1)
(save-place-mode +1)
(auto-revert-mode +1)
(recentf-mode +1)
(which-key-mode +1)

(setopt project-mode-line t)
(global-prettify-symbols-mode +1)
(global-completion-preview-mode +1)
;; highlight current line
(global-hl-line-mode t)
;; display time in mode line
(display-time-mode t)

;; always load newest byte code
(setq load-prefer-newer t)

(setq system-time-locale "C")

;; set a larger kill ring
(setq kill-ring-max 200)
;; use system clipboard
(setq save-interprogram-paste-before-kill t)
;; suppress warnings
(setq find-file-suppress-same-file-warnings t)

;; (add-to-list 'default-frame-alist
;; 	     '(font .  "IBM Plex Mono"))

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

;; word wrap for CJK
(setq word-wrap-by-category t)
;; enable syntax highlight
(global-font-lock-mode t)
;; delete selection
(delete-selection-mode t)
;; auto revert external changes
(global-auto-revert-mode t)

(setq warning-minimum-level :warning)

;;; early-init.el ends here
