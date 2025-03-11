(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize) ;; You might already have this line

;; -------------------------------------

;; backup and autosave
(savehist-mode 1)

(setq version-control t)
(setq delete-old-versions t)

;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/Trash/")

;; ----------------------------------------

(use-package doom-themes
  :ensure t
  :init (load-theme 'doom-one t))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(use-package smart-mode-line
  :ensure t
  :defer (sml/setup)
  :init (setq sml/theme 'respectful))
(use-package nyan-mode
  :ensure t
  :defer t
  :hook (after-init . nyan-mode))

(use-package page-break-lines
  :ensure t
  :hook (after-init-hook . page-break-lines-mode)
  :diminish (page-break-lines-mode visual-line-mode)
  :init (page-break-lines-mode t)
  )

;; -------------------------------------

(use-package desktop
  :config
  (setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
  (setq desktop-restore-frames t) ; don't restore any frame
  :init
  (desktop-save-mode 1)
  )

(use-package company
  :ensure t
  :defer t
  :hook (after-init . global-company-mode)
  :config
  (setq company-backends '((company-capf company-dabbrev-code))
	company-minimum-prefix-length 1
	company-dabbrev-code-ignore-case t
	company-dabbrev-code-modes t
	company-dabbrev-code-everywhere t
	company-dabbrev-code-completion-styles '(basic substring flex)))

(use-package good-scroll
  :diminish
  :hook (after-init . good-scroll-mode)
  :bind (([remap next] . good-scroll-up-full-screen)
         ([remap prior] . good-scroll-down-full-screen)))

(use-package auto-save
  :vc (:url "https://github.com/manateelazycat/auto-save")
  :hook
  (text-mode . auto-save-enable)
  (prog-mode . auto-save-enable)
  :config
  (setq auto-save-silent t)   ; quietly save
  (setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving
  (setq auto-save-disable-predicates
        '((lambda ()
            (string-suffix-p
             "gpg"
             (file-name-extension (buffer-name)) t)))))

(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global))
  :hook
  (prog-mode-hook  . outline-minor-mode)
  (prog-mode-hook . hs-minor-mode)
  )

;; ----------------------------------------

(use-package org
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook
            '(lambda ()
               (visual-line-mode 1)))
  (setq org-startup-with-inline-images t)
  :init
  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window
	org-support-shift-select t)

  ;; I like to press enter to follow a link. mouse clicks also work.
  (setq org-return-follows-link t)
  )

(use-package org-modern
  :ensure t
  :config
  (with-eval-after-load 'org (global-org-modern-mode))
  )

(use-package htmlize
  :ensure t
  )

;; ----------------------------------------

;; rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; indent

;; This ensures that pressing Enter will insert a new line and indent it.
(global-set-key (kbd "RET") #'newline-and-indent)

;; Indentation based on the indentation of the previous non-blank line.
(setq-default indent-line-function #'indent-relative-first-indent-point)

;; In modes such as `text-mode', calling `newline-and-indent' multiple times
;; removes the indentation. The following fixes the issue and ensures that text
;; is properly indented using `indent-relative' or
;; `indent-relative-first-indent-point'.
(setq-default indent-line-ignored-functions '())

(use-package indent-bars
  :commands indent-bars-mode
  :hook ((yaml-mode . indent-bars-mode)
         (yaml-ts-mode . indent-bars-mode)
         (python-mode . indent-bars-mode)
         (python-ts-mode . indent-bars-mode))
  :custom
  (indent-bars-prefer-character nil))

(use-package outline-indent
  :ensure t
  :commands (outline-indent-minor-mode
             outline-indent-insert-heading)
  :hook ((yaml-mode . outline-indent-minor-mode)
         (yaml-ts-mode . outline-indent-minor-mode)
         (python-mode . outline-indent-minor-mode)
         (python-ts-mode . outline-indent-minor-mode))
  :custom
  (outline-indent-ellipsis "->")
  :config (outline-indent-insert-heading))

(use-package block-nav
  :ensure t)

;; -------------------------------------

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package format-all
  :ensure t
  :commands format-all-mode
  :hook (prog-mode . format-all-mode)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))

(use-package dimmer
  :ensure t
  :init (dimmer-mode t)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-helm)
  )

(use-package dogears
  :ensure t
  :hook (after-init . dogears-mode)
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar))
  :config
  (setq dogears-idle 1
        dogears-limit 200
        dogears-position-delta 20)
  (setq dogears-functions '(find-file recenter-top-bottom
                                      other-window switch-to-buffer
                                      aw-select toggle-window-split
                                      windmove-do-window-select
                                      pager-page-down pager-page-up
                                      tab-bar-select-tab
                                      pop-to-mark-command
                                      pop-global-mark
                                      goto-last-change)))

;; -------------------------------------

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :config (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))
;; web-mode
(use-package web-mode
  :ensure t
  :mode (("\\.html?$" . web-mode)
         ("\\.jsx?$"  . web-mode)
         ("\\.php$"   . web-mode)
         ("\\.s?css$"  . web-mode)))

;; epub reader
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . my-nov-setup)
  :init
  (defun my-nov-setup ()
    "Setup `nov-mode' for better reading experience."
    (visual-line-mode 1)
    (centaur-read-mode)
    (face-remap-add-relative 'variable-pitch :family "Times New Roman" :height 1.5))
  :config
  (with-no-warnings
    ;; WORKAROUND: errors while opening `nov' files with Unicode characters
    ;; @see https://github.com/wasamasa/nov.el/issues/63
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (let* ((name (nov-content-unique-identifier-name content))
             (selector (format "package>metadata>identifier[id='%s']"
                               (regexp-quote name)))
             (id (car (esxml-node-children (esxml-query selector content)))))
        (and id (intern id))))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier)))

;; -------------------------------------

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package async
  :ensure t
  )

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure t
  )

(use-package counsel
  :ensure t
  :bind ("M-x" . counsel-M-x)
  ( "C-x C-f" . counsel-find-file))
(use-package ivy
  :ensure t
  :init
  (counsel-mode 1)
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "))
(use-package swiper
  :ensure t
  :bind ("C-s" . 'swiper))

;; -------------------------------------

(use-package lsp-mode
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (markdown-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-ivy)

(use-package typo
  :ensure t
  )

(use-package writeroom-mode
  :ensure t
  :hook (markdown-mode . writeroom-mode)
  )

;; -------------------------------------
;; shell-var
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))
;; environment path
(let ((envpath '("/usr/local/bin/" "~/.cargo/bin/" "~/.dotnet/tools/" "~/.local/bin/" "~/.cabal/bin")))
  (setenv "PATH" (mapconcat 'identity (add-to-list 'envpath (getenv "PATH") t) ":"))
  (setq exec-path (append envpath exec-path)))
;; pair
(global-prettify-symbols-mode t)
(use-package electric
  :config
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  (setq electric-pair-pairs '(
                              (?\{ . ?\})
                              (?\( . ?\))
                              (?\[ . ?\])
                              (?\" . ?\")
                              ))
  :init
  (electric-pair-mode t)
  (electric-indent-mode t)
  (electric-layout-mode t)
  )

(global-hl-line-mode t)

;; Eshell

(use-package eat
  :ensure t
  :hook
  ;; For `eat-eshell-mode'.
  (eshell-load-hook . eat-eshell-mode-hook)
  ;; For `eat-eshell-visual-command-mode'.
  (eshell-load-hook . eat-eshell-visual-command-mode-hook)
  )

(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)

(defun eshell/sudo-open (filename)
  "Open a file as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

(defun eshell-other-window ()
  "Create or visit an eshell buffer."
  (interactive)
  (if (not (get-buffer "*eshell*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (eshell))
    (switch-to-buffer-other-window "*eshell*")))

(global-set-key (kbd "<s-C-return>") 'eshell-other-window)

;; symbols
(global-prettify-symbols-mode +1)

;; fonts
(setq fonts '("IBM Plex Mono" "Noto Sans Mono CJK JP"))
(set-fontset-font t 'unicode "Noto Sans" nil 'prepend)
(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default nil :font
                    (format "%s:pixelsize=%d" (car fonts) 14))

;; -------------------------------------

;; abbrev [built-in]
(setq save-abbrevs nil)

;; calendar [built-in]
(global-set-key (kbd "C-c k") 'calendar)

;; dired [built-in]
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)
(setq dired-listing-switches "-alh")

;; display-line-numbers [built-in]
(setq display-line-numbers-type 'relative)

;; flyspell [built-in]
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; hideshow [built-in]
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; org-mode [built-in]
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; recentf [built-in]
(setq recentf-max-saved-items 500)
(recentf-mode t)

;; saveplace [built-in]
(save-place-mode t)

;; flymake [built]
(use-package flymake
  :hook (prog-mode . flymake-mode))

;; -------------------------------------


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes nil)
 '(package-selected-packages nil)
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
