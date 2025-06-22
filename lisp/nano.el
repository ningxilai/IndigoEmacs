;; nano.el --- NANO Emacs (minimal version) -*- lexical-binding: nil -*-

;;; Commentary:

;; Copyright (c) 2025  Nicolas P. Rougier
;; Released under the GNU General Public License 3.0
;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/nano-emacs

;;; Code:

;; Copy by Seagle0128

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

(defun nano-quit ()
  "Quit minibuffer from anywhere (code from Protesilaos Stavrou)"
  
  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

(global-set-key (kbd "C-g") 'nano-quit)

(defun nano-kill ()
  "Delete frame or kill emacs if there is only one frame left"
  
  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(global-set-key (kbd "C-x C-c") 'nano-kill)

;; --- Minimal key bindings ---------------------------------------------------

;; (bind-key "C-z"  nil) ;; No suspend frame
;; (bind-key "C-<wheel-up>" nil) ;; No text resize via mouse scroll
;; (bind-key "C-<wheel-down>" nil) ;; No text resize via mouse scroll

;; --- Minimal NANO (not a real) theme ----------------------------------------
  (defface nano-default '((t)) "" :group 'nano-faces-light)   (defface nano-default-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-highlight '((t)) "" :group 'nano-faces-light) (defface nano-highlight-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-subtle '((t)) "" :group 'nano-faces-light)    (defface nano-subtle-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-faded '((t)) "" :group 'nano-faces-light)     (defface nano-faded-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-salient '((t)) "" :group 'nano-faces-light)   (defface nano-salient-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-popout '((t)) "" :group 'nano-faces-light)    (defface nano-popout-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-strong '((t)) "" :group 'nano-faces-light)    (defface nano-strong-i '((t)) "" :group 'nano-faces-dark)
  (defface nano-critical '((t)) "" :group 'nano-faces-light)  (defface nano-critical-i '((t)) "" :group 'nano-faces-dark)

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

;; --- Header & Mode-Lines & Title---------------------------------------------

;; (setq-default mode-line-format (add-to-list 'mode-line-format '(:eval (if (buffer-modified-p) " ●" " ○"))))

;; (setq-default frame-title-format "%b")

(setq-default frame-title-format
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

(provide 'nano)
;;; nano.el ends.here
