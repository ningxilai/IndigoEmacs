;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; require
  
(dolist (dir '("lisp"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(let ((gc-cons-threshold most-positive-fixnum)
      (file-name-handler-alist nil))

(require 'package-manager)
(require 'addons)
(require 'base)
(require 'ui)
(require 'programming)
(require 'markup)
(require 'typer)

)

;; ends

;; Startup time

(setq init-start-time (current-time))

(let ((init-time (float-time (time-subtract (current-time) init-start-time)))
      (total-time (string-to-number (emacs-init-time "%f"))))
  (message (concat
    (propertize "Startup time: " 'face 'bold)
    (format "%.2fs " init-time)
    (propertize (format "(+ %.2fs system time)"
                        (- total-time init-time)) 'face 'shadow))))

;; ends

(setopt initial-buffer-choice t)
(setq initial-scratch-echo-area-message "iris")

;; init.el ends here
