;; nano-emacs.el --- NANO Emacs (minimal version)     -*- lexical-binding: t -*-

;; Copyright (c) 2025  Nicolas P. Rougier
;; Released under the GNU General Public License 3.0
;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/nano-emacs

;; This is NANO Emacs in 256 lines, without any dependency 
;; Usage (command line):  emacs -Q -l nano.el

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default (selected-frame)
                    :height 120 :weight 'light :family "Lilex Nerd Font") ;; IBM Plex Mono
(set-face-attribute 'bold nil :weight 'regular)
(set-face-attribute 'bold-italic nil :weight 'regular)

(set-fontset-font t 'unicode (font-spec :family "Unifont" :weight 'normal :slant 'normal ))
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal :slant 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '((height . 49)
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

;; --- Activate / Deactivate modes --------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(pixel-scroll-precision-mode 1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(global-font-lock-mode t)
(delete-selection-mode t)
(global-auto-revert-mode t)
(electric-pair-mode t)
(global-so-long-mode t)
(global-subword-mode t)
(global-prettify-symbols-mode t)
(auto-revert-mode t)
(recentf-mode t)
(display-time-mode t)
(which-key-mode t)

;; --- Minimal NANO (not a real) theme ----------------------------------------
(defface nano-default '((t)) "")   (defface nano-default-i '((t)) "")
(defface nano-highlight '((t)) "") (defface nano-highlight-i '((t)) "")
(defface nano-subtle '((t)) "")    (defface nano-subtle-i '((t)) "")
(defface nano-faded '((t)) "")     (defface nano-faded-i '((t)) "")
(defface nano-salient '((t)) "")   (defface nano-salient-i '((t)) "")
(defface nano-popout '((t)) "")    (defface nano-popout-i '((t)) "")
(defface nano-strong '((t)) "")    (defface nano-strong-i '((t)) "")
(defface nano-critical '((t)) "")  (defface nano-critical-i '((t)) "")

(defun nano-set-face (name &optional foreground background weight)
  "Set NAME and NAME-i faces with given FOREGROUND, BACKGROUND and WEIGHT"

  (apply #'set-face-attribute `(,name nil
                                ,@(when foreground `(:foreground ,foreground))
                                ,@(when background `(:background ,background))
                                ,@(when weight `(:weight ,weight))))
  (apply #'set-face-attribute `(,(intern (concat (symbol-name name) "-i")) nil
                                :foreground ,(face-background 'nano-default)
                                ,@(when foreground `(:background ,foreground))
                                :weight regular)))

(defun nano-link-face (sources faces &optional attributes)
  "Make FACES to inherit from SOURCES faces and unspecify ATTRIBUTES."

  (let ((attributes (or attributes
                        '( :foreground :background :family :weight
                           :height :slant :overline :underline :box))))
    (dolist (face (seq-filter #'facep faces))
      (dolist (attribute attributes)
        (set-face-attribute face nil attribute 'unspecified))
      (set-face-attribute face nil :inherit sources))))

;; --- Minibuffer completion --------------------------------------------------
(setq tab-always-indent 'complete
      icomplete-delay-completions-threshold 0
      icomplete-compute-delay 0
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      icomplete-prospects-height 9
      icomplete-separator " . "
      icomplete-with-completion-tables t
      icomplete-in-buffer t
      icomplete-max-delay-chars 0
      icomplete-scroll t
      icomplete-vertical-mode t
      resize-mini-windows 'grow-only
      icomplete-matches-format nil)

(setq completion-preview-ignore-case t
      completion-ignore-case t
      completion-auto-help t)

;; --- Minimal key bindings ---------------------------------------------------
(defun nano-quit ()
  "Quit minibuffer from anywhere (code from Protesilaos Stavrou)"

  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

(defun nano-kill ()
  "Delete frame or kill emacs if there is only one frame left"

  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x C-c" #'nano-kill)
(bind-key "C-x C-r" #'recentf-open)
(bind-key "C-g" #'nano-quit)
(bind-key "M-n" #'make-frame)
(bind-key "C-z"  nil) ;; No suspend frame
(bind-key "C-<wheel-up>" nil) ;; No text resize via mouse scroll
(bind-key "C-<wheel-down>" nil) ;; No text resize via mouse scroll

;; Copy by Seagle0128

(defun childframe-workable-p ()
  "Whether childframe is workable."
 (and (>= emacs-major-version 26)
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

(setq blink-matching-paren-highlight-offscreen t
      show-paren-context-when-offscreen
      (if (childframe-workable-p) 'child-frame 'overlay))

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
(setq-default pathname-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8)
      locale-coding-system 'utf-8
      file-name-coding-system 'utf-8
      default-buffer-file-coding-system 'utf-8
      slime-net-coding-system 'utf-8-unix)

(setq word-wrap-by-category t)

(setenv "LC_CTYPE" "UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "LANG" "en_US.UTF-8")

(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              select-enable-clipboard t
              system-time-locale "C"
              ;; nice scroll
              scroll-step 1
              scroll-preserve-screen-position t
              scroll-margin 3
              scroll-conservatively 10
              maximum-scroll-margin 0.3
              scroll-up-aggressively 0.0
              scroll-down-aggressively 0.0
              pixel-scroll-precision-interpolate-page t)

;; --- Header & Mode-Lines & Title---------------------------------------------

;; (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))

;; (setq frame-title-format "%b")

(setq frame-title-format
      '(:eval (concat
	       (if (and buffer-file-name (buffer-modified-p)) "•")
	       (buffer-name)
	       (if buffer-file-name
		   (concat " (" (directory-file-name (abbreviate-file-name default-directory)) ")")) " - Emacs")))

;; (setq-default header-line-format '("GC: " (:eval (number-to-string gcs-done)) " - " (:eval (number-to-string gc-elapsed)) "s"))

(setopt project-mode-line t)
(setq-default header-line-format
  '(:eval
    (let ((prefix (cond (buffer-read-only     '("RO" . nano-default-i))
                        ((buffer-modified-p)  '("**" . nano-critical-i))
                        (t                    '("RW" . nano-faded-i))))
          (mode (concat "(" (downcase (cond ((consp mode-name) (car mode-name))
                                            ((stringp mode-name) mode-name)
                                            (t "unknow")))
                        " mode)"))
          (coords (format-mode-line "%c:%l ")))
      (list
       (propertize " " 'face (cdr prefix)  'display '(raise -0.25))
       (propertize (car prefix) 'face (cdr prefix))
       (propertize " " 'face (cdr prefix) 'display '(raise +0.25))
       (propertize (format-mode-line " %b ") 'face 'nano-strong)
       (propertize mode 'face 'header-line)
       (propertize " " 'display `(space :align-to (- right ,(length coords))))
       (propertize coords 'face 'nano-faded)))))

)

(provide 'nano)
