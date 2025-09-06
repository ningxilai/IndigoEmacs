;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ess
  :ensure t
  :after quarto-mode
  :config
  (add-hook 'ess-mode-hook #'electric-spacing-mode))

(use-package electric-spacing
  :ensure (:host github
                 :repo "walmes/electric-spacing")
  :config
  (defvar my-electic-pair-modes '(python-mode julia-mode org-mode latex-mode))
  (defun my-inhibit-electric-pair-mode (char)
    (not (member major-mode my-electic-pair-modes)))
  (setq electric-pair-inhibit-predicate #'my-inhibit-electric-pair-mode))

(use-package quarto-mode
  :ensure t
  :mode
  ("\\.Rmd\\'" . poly-markdown-mode)
  ("\\.qmd\\'" . poly-markdown-mode)
  :hook (poly-markdown-mode . (lambda()(set-input-method "rime")))
  :config
  (use-package poly-R
    :ensure t))

(use-package grip-mode
  :ensure t
  :config (setq grip-command 'go-grip) ;; auto, grip, go-grip or mdopen // uv tool install grip
  :bind (:map markdown-mode-command-map
              ("C-c m g" . grip-mode)))

;; (use-package fountain-mode
;;   :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :custom
  (markdown-open-command "firefox")
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-gfm-uppercase-checkbox t)
  (markdown-gfm-additional-languages "Mermaid")
  :mode
  ("README\\.md\\'" . gfm-mode)
  :config
  (setq-default markdown-mode-font-lock-keywords
                (cl-remove-if
                 (lambda (item) (equal item '(markdown-fontify-tables)))
                 markdown-mode-font-lock-keywords)))

;; (use-package markdown-toc :ensure t :defer markdown-mode)

(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))
  ;; copy by site-lisp/config/init-markdown-mode.el
  (setq-default markdown-ts--treesit-settings
                (treesit-font-lock-rules
                 :language 'markdown-inline
                 :override t
                 :feature 'delimiter
                 '([ "[" "]" "(" ")" ] @shadow)

                 :language 'markdown
                 :feature 'paragraph
                 '([((setext_heading) @font-lock-function-name-face)
                    ((atx_heading) @font-lock-function-name-face)
                    ((thematic_break) @shadow)
                    ((indented_code_block) @font-lock-comment-face)
                    (list_item (list_marker_star) @font-lock-constant-face)
                    (list_item (list_marker_plus) @font-lock-constant-face)
                    (list_item (list_marker_minus) @font-lock-constant-face)
                    (list_item (list_marker_dot) @font-lock-constant-face)
                    (fenced_code_block (fenced_code_block_delimiter) @font-lock-doc-face)
                    (fenced_code_block (code_fence_content) @font-lock-comment-face)
                    ((block_quote_marker) @font-lock-comment-face)
                    (block_quote (paragraph) @font-lock-comment-face)
                    (block_quote (block_quote_marker) @font-lock-comment-face)
                    ])

                 :language 'markdown-inline
                 :feature 'paragraph-inline
                 '([
                    ((image_description) @link)
                    ((link_destination) @font-lock-comment-face)
                    ((code_span) @font-lock-comment-face)
                    ((emphasis) @underline)
                    ((strong_emphasis) @bold)
                    (inline_link (link_text) @link)
                    (inline_link (link_destination) @font-lock-comment-face)
                    (shortcut_link (link_text) @link)])))
  )

;; (use-package pyim
;;   :defer t
;;   :init
;;   (setq-default default-input-method "pyim")
;;   :config
;;   (setq-local pyim-page-tooltip 'minibuffer
;;               pyim-punctuation-translate-p '(auto))
;;   :bind ("C-\\" . toggle-input-method)
;;   :hook (text-mode . (lambda ()
;;                        (pyim-greatdict-enable)
;;                        (pyim-tsinghua-dict-enable)
;;                        (pyim-basedict-enable)))
;;   )

;; (use-package pyim-tsinghua-dict
;;   :vc (pyim-tsinghua-dict :url "https://github.com/redguardtoo/pyim-tsinghua-dict.git"
;;                           :rev :newest))
;; (use-package pyim-basedict
;;   :vc (pyim-basedict :url "https://github.com/tumashu/pyim-basedict.git"
;;                      :rev :newest))
;; (use-package pyim-greatdict
;;   :vc (pyim-greatdict :url "https://github.com/tumashu/pyim-greatdict.git"
;;                       :rev :newest))

(use-package rime
  :ensure (:host github :repo "DogLooksGood/emacs-rime")
  :init (setq default-input-method "rime"
              rime-librime-root "~/Desktop/librime/build/")
  :config
  (setq-default rime-disable-predicates
                '(rime-predicate-after-alphabet-char-p
                  rime-predicate-current-uppercase-letter-p
                  rime-predicate-prog-in-code-p))
  (setq-default rime-inline-predicates '(rime-predicate-space-after-cc-p
                                         rime-predicate-current-uppercase-letter-p
                                         rime-predicate-punctuation-after-ascii-p))

  ;; (defun rime-lib-finalize() nil)
  ;; (add-hook 'kill-emacs-hook #'rime-lib-finalize)

  :hook
  (elpaca-after-init . (lambda ()
                         (setq-default rime-emacs-module-header-root "/usr/include" ;; "~/.local/include"
                                       rime-show-candidate 'minibuffer
                                       rime-user-data-dir "~/.config/rime")))

  (post-command . (lambda ()
                    (progn
                      (defvar input-method-cursor-color "Orange"
                        "Default cursor color if using an input method.")

                      (defvar default-cursor-color (frame-parameter nil 'cursor-color)
                        "Default text cursor color.")

                      (defun change-cursor-color-on-input-method ()
                        "Set cursor color depending on whether an input method is used or not."
                        (interactive)
                        (set-cursor-color (if current-input-method
                                              input-method-cursor-color
                                            default-cursor-color)))
                      )
                    ))
  :custom
  (rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#c4cdd3"
            :font "LXGW Wenkai"
            :internal-border-width 10))
  :bind (:map rime-active-mode-map ("<tab>" . rime-inline-ascii)))

(use-package typoel
  :ensure (typo :host github
                :repo "jorgenschaefer/typoel")
  :hook (text-mode . typo-mode))

(use-package bm
  :ensure (:host github :repo "joodland/bm")
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)
  ;; where to store persistant files
  (setq bm-repository-file (expand-file-name "/bm" elpaca-repos-directory))
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  ;; Loading the repository from file when on start up.
  (add-hook 'after-init-hook 'bm-repository-load)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages
  ;; that automatically reverts the buffer (like vc after a check-in).
  ;; This can easily be avoided if the package provides a hook that is
  ;; called before the buffer is reverted (like `vc-before-checkin-hook').
  ;; Then new bookmarks can be saved before the buffer is reverted.
  ;; Make sure bookmarks is saved before check-in (and revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)))

(use-package chinese-conv
  :ensure t
  :defer t)

(use-package pangu-spacing
  :ensure t
  :config
  (global-pangu-spacing-mode t)
  :hook
  (org-mode . (lambda () (setq-local pangu-spacing-real-insert-separtor t))))

(use-package cns
  :ensure (:host github :repo "kanglmf/emacs-chinese-word-segmentation")
  :config
  (setq-default cns-prog "cnws"
                cns-dict-directory (expand-file-name "repos/emacs-chinese-word-segmentation/cppjieba/dict/" elpaca-directory)
                cns-recent-segmentation-limit 20
                cns-debug t)
  :hook (find-file))

;; Default, comment out the providers you don't need.
(use-package fanyi
    :ensure t
    :custom
    (fanyi-providers '(;; 海词
                       fanyi-haici-provider
                       ;; 有道同义词词典
                       fanyi-youdao-thesaurus-provider
                       ;; Etymonline
                       fanyi-etymon-provider
                       ;; Longman
                       fanyi-longman-provider
                       ;; English-English dictionary
                       fanyi-etymon-provider
                       fanyi-longman-provider)))

(use-package gt
    :ensure (:host github :repo "lorniu/gt.el")
    :custom
    (gt-langs '(en zh))
    (gt-default-translator '(gt-translator :engines (gt-youdao-dict-engine))))

(provide 'lang-chinese)
;;; lang-chinese.el ends here
