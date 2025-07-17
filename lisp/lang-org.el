;; -*- lexical-binding: t; -*-

;; Org

(use-package org
  :ensure nil
  :config
  (setq
           ;; Edit settings
           org-auto-align-tags nil
           org-tags-column 0
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           org-startup-with-inline-images t
           org-src-window-setup 'current-window
           ;; Org styling, hide markup etc.
           org-hide-emphasis-markers t
           org-hide-leading-stars t
           org-pretty-entities t
           org-ellipsis "…")
  (setq
   org-todo-keywords
   '((sequence
      "TODO(t)"
      "WAIT(w)"
      "|"                 ; Separates "active" and "inactive" states.
      "DONE(d)"
      "CANCELLED(c)" )))
  :hook ((org-mode . org-indent-mode)
         ;; (org-mode . org-toggle-pretty-entities)
         (org-mode . (lambda () (visual-line-mode t))))
  :custom
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq-local
   org-catch-invisible-edits 'show-and-error
   org-agenda-tags-column 0)
  )

(use-package org-contrib
  :ensure (:host github :repo "emacsmirror/org-contrib"))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode))

(use-package toc-org
  :ensure t
  :after org-mode
  :hook (org-mode . toc-org-mode))

(use-package htmlize
  :ensure t)

(provide 'lang-org)

;; ends here
