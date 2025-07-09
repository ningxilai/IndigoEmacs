;; Lang-Chinese --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package ess
  :ensure t
  :after quarto-mode)

(use-package quarto-mode
  :ensure t
  :mode
  ("\\.Rmd\\'" . poly-markdown-mode)
  ("\\.qmd\\'" . poly-markdown-mode)
  :hook (poly-markdown-mode . (lambda()(set-input-method "rime")))
  :config
  (use-package poly-R
    :ensure t))

(use-package markdown-ts-mode
  :ensure nil
  :mode "\\.md\\'"
  :custom
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

(use-package rime
  :ensure t
  :init (setq default-input-method "rime")
  :config
  (setq rime-emacs-module-header-root "~/.local/include"
        rime-show-candidate 'posframe
        rime-user-data-dir "~/.config/rime")
  (custom-set-variables
   '(rime-disable-predicates
     '(rime-predicate-after-alphabet-char-p
       rime-predicate-current-uppercase-letter-p
       rime-predicate-prog-in-code-p)))
  :custom
  (rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "LXGW Wenkai"
            :internal-border-width 10)))

;; (use-package pyim-tsinghua-dict
;;   :vc (pyim-tsinghua-dict :url "https://github.com/redguardtoo/pyim-tsinghua-dict.git"
;;                           :rev :newest))
;; (use-package pyim-basedict
;;   :vc (pyim-basedict :url "https://github.com/tumashu/pyim-basedict.git"
;;                      :rev :newest))
;; (use-package pyim-greatdict
;;   :vc (pyim-greatdict :url "https://github.com/tumashu/pyim-greatdict.git"
;;                       :rev :newest))

(use-package chinese-conv
  :ensure t
  :defer t)
(use-package pangu-spacing
  :defer t
  :ensure t
  :init
  (global-pangu-spacing-mode t)
  :hook
  (org-mode . (lambda () (setq-local pangu-spacing-real-insert-separtor t))))

(use-package cns
  :vc (cns :url "https://github.com/kanglmf/emacs-chinese-word-segmentation.git"
           :rev :newest)
  :init
  (setq-default cns-prog "cnws"
              cns-dict-directory "var/cns-dict/"
              cns-recent-segmentation-limit 20
              cns-debug nil)
  :hook (find-file))

;; (use-package markdown-toc :ensure t :defer markdown-mode)

(provide 'lang-chinese)
;;; lang-chinese.el ends here
