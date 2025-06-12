;; -*- lexical-binding: t; -*-

(setq package-user-dir
      (expand-file-name
       (format "elpa/%s.%s"
           emacs-major-version emacs-minor-version)
       user-emacs-directory))

(require 'use-package-ensure)
(require 'package)

(setq package-quickstart t
      use-package-always-ensure nil
      use-package-always-defer t
      use-package-expand-minimally t
      use-package-vc-prefer-newest t
      native-comp-deferred-compilation t
      native-comp-jit-compilation t
      package-native-compile t
      version-control t
      package-enable-at-startup t
      delete-old-versions t
      package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

;; (use-package borg
;;   :ensure t
;;   :init
;;   (if (require 'borg-elpa nil t)
;;       (borg-elpa-initialize)
;;     (package-activate-all)
;;     ))

(provide 'package-init)

;;; ends here
