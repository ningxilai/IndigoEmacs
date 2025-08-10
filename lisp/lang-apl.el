(use-package gnu-apl-mode
  :ensure t
  :config
  (setq-default gnu-apl-show-tips-on-start nil)
  :hook
  (gnu-apl-mode . (lambda ()
                    (progn
                      (set-input-method "APL-Z")
                      (setq-local buffer-face-mode-face '(:family "APL386 Unicode" :height 125))
                      (buffer-face-mode)
                      ;; https://github.com/abrudz/APL386
                      ;; https://aplwiki.com/wiki/Fonts
                      (electric-pair-mode -1)
                      )
                    )
                )
  (gnu-apl-interactive-mode . (lambda ()
                                (progn
                                  (set-input-method "APL-Z")
                                  (setq-local buffer-face-mode-face
                                              '(:family "BQN386 Unicode" :height 125))
                                  (buffer-face-mode)
                                  (electric-pair-mode -1)
                                  )
                                )
                            )
  )

(use-package dyalog-mode
  :ensure t
  :init
  (setq dyalog-fix-whitespace-before-save t
        dyalog-leading-spaces 0)
  :hook
  (dyalog-mode .
               (lambda () (progn
                            (setq-local buffer-face-mode-face
                                        '(:family "BQN386 Unicode" :height 125))
                            (buffer-face-mode)
                            )
                 )
               )
  :interpreter
  ("dyalogscript\\(\\.bash\\)?" . dyalog-mode)
  :mode
  (("\\.apl[afno]" . dyalog-mode))
  :config
  (modify-syntax-entry ?# ". 1" dyalog-mode-syntax-table)
  (modify-syntax-entry ?! ". 2<" dyalog-mode-syntax-table))

(use-package j-mode
  :ensure (:host github
                 :repo "LdBeth/j-mode"
                 :files ("*.el"))
  :mode (("\\.ij[rsp]$" . j-mode)
         ("\\.ijt$" . j-lab-mode))
  :init
  (setq-default j-console-cmd "jconsole")
  :hook
  (inferior-j-mode . (lambda () (electric-pair-mode -1)))
  :custom
  (j-verb-face ((t (:foreground "Red"))))
  (j-adverb-face ((t (:foreground "Green"))))
  (j-conjunction-face ((t (:foreground "Blue"))))
  (j-other-face ((t (:foreground "Black"))))
  )
