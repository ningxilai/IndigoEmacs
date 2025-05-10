;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

;; load custom setting

(setopt use-short-answers t)

(defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(setq initial-buffer-choice t)
(setq initial-scratch-echo-area-message "iris")

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

(auto-save-visited-mode 1)

(setq delete-by-moving-to-trash t
      auto-save-timeout 5
      delete-auto-save-files t
      backup-directory '(("." . "~/.backup"))) ;trash-directory "~/Trash/

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; ends

;; require

(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(require 'nano)

(require 'elpaca-init)
(elpaca elpaca-use-package (elpaca-use-package-mode))
(setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
(elpaca-wait)

(require 'base)
(require 'color)
(require 'programming)
(require 'markup)

;; ends

;; init.el ends here
