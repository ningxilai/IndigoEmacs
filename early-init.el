;;; early-init.el --- early bird  -*- no-byte-compile: t -*-

;;by CentaurEmacs/early-init.el

(setq native-comp-deferred-compilation t ;; obsolete since 29.1
      native-comp-jit-compilation t)

(setq gc-cons-threshold (* 32 1024 1024))

(add-to-list 'default-frame-alist
	     '(font .  "IBM Plex Mono"))

(setq package-enable-at-startup t)

(push '(menu-bar-lines . 0) default-frame-alist)

(push '(tool-bar-lines . 0) default-frame-alist)

(push '(vertical-scroll-bars) default-frame-alist)

(setq-default mode-line-format t)

(setq inhibit-startup-message t)

(set-language-environment "UTF-8")
;;; early-init.el ends here
