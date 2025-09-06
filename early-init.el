;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: t; -*-

;;; Code:

;; (require 'xdg)

;; (startup-redirect-eln-cache
;;  (expand-file-name  "emacs/eln-cache/" (xdg-cache-home)))

(setq-default gc-cons-threshold most-positive-fixnum)

(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))

;; --- Frame / windows layout & behavior --------------------------------------
(setopt default-frame-alist
        '((height . 50)
          (width  . 80)
          (alpha-background . 90)
          (left-fringe . 0)
          (right-fringe . 0)
          (internal-border-width . 32)
          (horizontal-scroll-bars . nil)
          (vertical-scroll-bars . nil)
          (undecorated-round . t)
          (frame-inhibit-implied-resize . t))
        bottom-divider-width nil
        right-divider-width nil)

(modify-frame-parameters (selected-frame) default-frame-alist)

(when (display-graphic-p)
  (context-menu-mode -1))

(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode -1))

(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; --- Sane settings ------ CJK && UTF-8 ---------------------------------------
(set-language-environment "utf-8")
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default pathname-coding-system 'utf-8
              default-buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8)
      locale-coding-system 'utf-8
      file-name-coding-system 'utf-8)

(setq-default system-time-locale "C")

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;; (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))

;; (setq-default frame-title-format "%b")


;; (setq-default header-line-format '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))

(setq-default package-enable-at-startup nil
              native-comp-async-report-warnings-errors t ;; 'silent
              debug-on-error t)

;;; early-init.el ends here
