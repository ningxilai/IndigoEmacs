;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: t; -*-

;;; Code:

;; (require 'xdg)

;; (startup-redirect-eln-cache
;;  (expand-file-name  "emacs/eln-cache/" (xdg-cache-home)))

(startup-redirect-eln-cache
 (convert-standard-filename
  (expand-file-name  "var/eln-cache/" user-emacs-directory)))

(setq package-enable-at-startup t)

;;; early-init.el ends here
