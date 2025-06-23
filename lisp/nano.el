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
  "Quit minibuffer from anywhere (code from Protesilaos Stavrou)."
  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

(global-set-key (kbd "C-g") 'nano-quit)

(defun nano-kill ()
  "Delete frame or kill emacs if there is only one frame left."
  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(global-set-key (kbd "C-x C-c") 'nano-kill)

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

(defgroup nano-faces-dark nil t :group 'faces)

(defface nano-default-i nil "" :group 'nano-faces-dark)
(defface nano-faded-i nil "" :group 'nano-faces-dark)
(defface nano-critical-i nil "" :group 'nano-faces-dark)

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
