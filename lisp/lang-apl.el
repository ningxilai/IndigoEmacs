(use-package gnu-apl-mode
  :ensure t
  :config
  (setq-default gnu-apl-show-tips-on-start nil)
  :hook
  (gnu-apl-mode . (lambda ()
                    (set-input-method "APL-Z")
                    (setq-local buffer-face-mode-face '(:family "APL386 Unicode")) (buffer-face-mode)
                    ;; https://github.com/abrudz/APL386
                    ;; https://aplwiki.com/wiki/Fonts
                    )
                )
  (gnu-apl-interactive-mode . (lambda ()
                                (set-input-method "APL-Z")
                                (setq-local buffer-face-mode-face '(:family "BQN386 Unicode")) (buffer-face-mode)
                                ;; (require 'apl)
                                ;; (set-input-method "apl-ascii")
                                ;; https://www.metalevel.at/unicapl/
                                )
                            )
  )
