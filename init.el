;;; init.el --- my emacs init file -*- lexical-binding:t; -*-

(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(package-initialize) ;; You might already have this line

;; -------------------------------------
;; move to trash
(setq delete-by-moving-to-trash t)
(setq trash-directory "~/Trash/")

;;  (defalias 'yes-or-no-p 'y-or-n-p)
;; ----------------------------------------

(use-package doom-themes
 :ensure t
 :init (load-theme 'doom-one t)
 :config (setq-default mode-line-format t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package page-break-lines
  :ensure t
  ;; :hook (after-init-hook . page-break-lines-mode)
  :diminish (page-break-lines-mode visual-line-mode)
  :init (page-break-lines-mode t)
  )

(use-package async
  :ensure t)

;; -------------------------------------

(use-package desktop
  :config
  (setq desktop-load-locked-desktop nil) ; popup dialog ask user, don't load anyway
  (setq desktop-restore-frames nil) ; don't restore any frame
  :init
  (desktop-save-mode 1))

(use-package good-scroll
  :ensure t
  :diminish
  :init (good-scroll-mode 1)
  :bind (([remap next] . good-scroll-up-full-screen)
         ([remap prior] . good-scroll-down-full-screen)))

 ;; Save all tempfiles in $TMPDIR/emacs$UID/                                                        
    (defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
    (setq backup-directory-alist
        `((".*" . ,emacs-tmp-dir)))
    (setq auto-save-file-name-transforms
        `((".*" ,emacs-tmp-dir t)))
    (setq auto-save-list-file-prefix
        emacs-tmp-dir)

(use-package editorconfig
  :ensure t
  :init (editorconfig-mode +1))

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
  :ensure t
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

;; -------------------------------------

(use-package dogears
  :ensure t
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

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

;; ---------------------------------------

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

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act))
  )

(use-package format-all
  :ensure nil
  :commands format-all-mode
  :hook (add-hook 'before-save-hook 'format-all-buffer)
  :config
  (setq-default format-all-formatters
                '(("C"     (astyle "--mode=c"))
                  ("Shell" (shfmt "-i" "4" "-ci")))))
                  

(use-package writeroom-mode
  :ensure t
  :hook (markdown-mode . writeroom-mode))

(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (markdown-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom (lsp-completion-provide :none))

(use-package lsp-ui
  :ensure t  :commands lsp-ui-mode)

;; -------------------------------------
;; shell-var
;(use-package exec-path-from-shell
;  :ensure t
;  :init (when (daemonp)
;          (exec-path-from-shell-initialize)))

;; environment path
;; envpath '("/usr/local/bin/" "~/.cargo/bin/" "~/.dotnet/tools/" "~/.local/bin/" "~/.cabal/bin")

;; pair
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

;; fonts

(set-face-attribute 'default nil :font "Fira Code")

(set-fontset-font t 'han (font-spec :family "LXGW WenKai" :weight 'normal))
(set-fontset-font t 'kana (font-spec :family "Sarasa Gothic" :weight 'normal :slant 'normal))

(set-face-attribute 'default (selected-frame) :height 120)

;;----------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
