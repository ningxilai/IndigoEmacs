;; load custom setting  -*- lexical-binding: t; -*-

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

(defun childframe-workable-p ()
  "Whether childframe is workable."
 (and (>= emacs-major-version 26)
       (not noninteractive)
       (not emacs-basic-display)
       (or (display-graphic-p)
           (featurep 'tty-child-frames))
       (eq (frame-parameter (selected-frame) 'minibuffer) 't)))

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


(setopt blink-matching-paren-highlight-offscreen t
      show-paren-context-when-offscreen
      (if (childframe-workable-p) 'child-frame 'overlay))

(auto-save-visited-mode 1)

(setq delete-by-moving-to-trash t
      auto-save-timeout 5
      delete-auto-save-files t
      backup-directory '(("." . "~/.backup"))) ;trash-directory "~/Trash/

(provide 'addons)

;; ends
