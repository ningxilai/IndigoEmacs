;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: t; -*-
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

;;  (defalias 'yes-or-no-p 'y-or-n-p)

(setq flyspell-mode 1)

(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              x-select-enable-clipboard t)

;; set frame title
; (setq frame-title-format "%b")
(setq frame-title-format
      '(:eval (concat
	       (if (and buffer-file-name (buffer-modified-p)) "•")
	       (buffer-name)
	       (if buffer-file-name
		   (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")")) " - Emacs"))
      )

(setq create-lockfiles nil)

(setopt project-mode-line t)

;; display time in mode line
(display-time-mode t)

;; always load newest byte code
(setq load-prefer-newer t)

;; set a larger kill ring
(setq kill-ring-max 200)

;; use system clipboard
(setq save-interprogram-paste-before-kill t)

;; suppress warnings
(setq find-file-suppress-same-file-warnings t)

;; (add-to-list 'default-frame-alist
;; 	     '(font .  "IBM Plex Mono"))

(setq warning-minimum-level :warning)

(setq package-enable-at-startup nil)

;;; early-init.el ends here
