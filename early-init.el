;; early-init --- early-init.el -*- no-byte-compile:t; lexical-binding: t; -*-

;;; Code:

(require 'xdg)

(startup-redirect-eln-cache
 (expand-file-name  "emacs/eln-cache/" (xdg-cache-home)))

(setq package-enable-at-startup t)

;;; early-init.el ends here
