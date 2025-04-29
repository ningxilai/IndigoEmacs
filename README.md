Indigo Emacs
--------------------
Install:

```bash
git clone --depth 1 https://github.com/ningxilai/IndigoEmacs $XDG_CONFIG_HOME/emacs
```
--------------------
Acknowledge:

- IsaacGu .emacs.d
- seagle0128 Centaur
- snackon Witchmacs / pprobst yukimacs
- ltylty .emacs.d
- rougier nano-emacs
- EmacsChina forum Emacs-general
- ![Cold's World](https://coldnight.github.io/dump-brain-with-emacs/)
- etc..

--------------------
Configure.Bak:

``` elisp

(use-package company
  :ensure t
  :defer t
  :config
  (setq company-backends '((:separate company-capf company-dabbrev-code))
	company-global-modes '(not shell-mode)
                                        ; company-minimum-prefix-length 1
        )
  )

(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; Use Company backends as Capfs.
  (setq-local completion-at-point-functions
              (mapcar #'cape-company-to-capf
                      (list #'company-files #'company-keywords #'company-dabbrev))))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))
  :init
  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  ;; Enable optional extension modes:
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (setq corfu-auto t
      corfu-quit-no-match 'separator))

;; (setq text-mode-ispell-word-completion nil) or (customize-set-variable 'text-mode-ispell-word-completion nil) => https://github.com/minad/corfu/discussions/457

(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :defer t
  :bind ("C-s" . consult-line))

(use-package vertico
  :ensure t
  :defer t
  :custom
  (vertico-cycle t)
  :hook (after-init . vertico-mode))

(use-package marginalia
  :ensure t
  :defer t
  :hook (after-init . marginalia-mode))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package embark
  :ensure t
  :defer t
  :bind
  (:map minibuffer-mode-map
	("C-c C-e" . embark-export)
	("C-c C-a" . embark-act)))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-ediff-dwim-show-on-hunks t))

(use-package diff-hl
  :vc (:url "https://github.com/dgutov/diff-hl")
  :hook (magit-post-refresh-hook . diff-hl-magit-post-refresh))

(use-package rg :ensure t
  :init (setq rg-enable-default-bindings t)
  :bind ("C-c s" . rg))

```
