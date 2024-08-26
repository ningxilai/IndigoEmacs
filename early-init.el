;;;by CentaurEmacs/early-init.el && ItyIty/VanillaDoom/early-init.el

(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

(setq package-enable-at-startup t)

(setq use-package-enable-imenu-support t)

(prefer-coding-system 'utf-8)

(set-charset-priority 'unicode)

(setq system-time-locale "C")

(setq frame-inhibit-implied-resize t)

(push '(menu-bar-lines . 0) default-frame-alist)

(push '(tool-bar-lines . 0) default-frame-alist)

(push '(vertical-scroll-bars) default-frame-alist)

(setq-default mode-line-format t)

(setq gc-cons-threshold most-positive-fixnum)

(setq inhibit-startup-message t)

(add-to-list 'default-frame-alist
	     '(font . "IBM Plex Mono"))

;;end
