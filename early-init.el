;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: t; -*-

;;; Code:

;; (require 'xdg)

;; (startup-redirect-eln-cache
;;  (expand-file-name  "emacs/eln-cache/" (xdg-cache-home)))

(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))

;; --- Frame / windows layout & behavior --------------------------------------
(setq-default default-frame-alist
              '((height . 50)
                (width  . 80)
                (left-fringe . 0)
                (right-fringe . 0)
                (internal-border-width . 32)
                (vertical-scroll-bars . nil)
                (bottom-divider-width . 0)
                (right-divider-width . 0)
                (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

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

(setq word-wrap-by-category t)
  
(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;; (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))

;; (setq-default frame-title-format "%b")


;; (setq-default header-line-format '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))

;;; early-init.el ends here
