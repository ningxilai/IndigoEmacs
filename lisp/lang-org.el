;; -*- lexical-binding: t; -*-

;; Org

(use-package org
  :ensure t
  :config (setq
           ;; Edit settings
           org-auto-align-tags nil
           org-tags-column 0
           org-catch-invisible-edits 'show-and-error
           org-special-ctrl-a/e t
           org-insert-heading-respect-content t
           
           ;; Org styling, hide markup etc.
           org-hide-emphasis-markers t
           org-pretty-entities t
           org-agenda-tags-column 0
           org-ellipsis "…"))
(use-package org-contrib :ensure t)
(use-package htmlize
  :ensure t)
(use-package org-modern
  :ensure t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         (org-modern-mode . (lambda ()
                              "Adapt `org-modern-mode'."
                              ;; Disable Prettify Symbols mode
                              (setq prettify-symbols-alist nil)
                              (prettify-symbols-mode -1)))))

(provide 'lang-org)

;; ends here
