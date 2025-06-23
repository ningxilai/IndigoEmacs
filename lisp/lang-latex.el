;; Lang-LaTeX --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package latex-change-env
  :ensure t
  :after latex-mode
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env)))

(use-package latex-mode
  :defer t
  :no-require t
  :ensure auctex
  :commands LaTeX-mode
  :mode "\\.tex" "\\.stex" "\\.texi"
  :init
  ;; PDF View
  (setq-default TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (setopt TeX-auto-save t
          TeX-parse-self t
          TeX-show-compilation t)
  (setq-local TeX-output-view-style  '(("^pdf$" "." "xpdf %o %(outpage)"))
              reftex-plug-into-AUCTeX t
              LaTeX-command "latexmk -shell-escape -bibtex -pdf -g -f %f"
              ;; "latex -shell-escape --synctex=1"
              TeX-command-default "XeLaTeX")
  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-PDF-mode t
                TeX-source-correlate-start-server t
                TeX-source-correlate-mode t
                TeX-source-correlate-method '((dvi . source-specials)
                                              (pdf . synctex)))
  :custom
  (defun TeX-auto-compile()
    (let* ((master (TeX-master-file))
           (process (and (stringp master) (TeX-process master))))
      (when (and (processp process)
                 (eq (process-status process) 'run))
        (delete-process process))
      (add-hook 'after-save-hook
                (lambda()
                  (let ((TeX-command-extra-options "-shell-escape -synctex=1 -interaction=nonstopmode"))
                    (TeX-command-run-all nil))
                  nil t))))
  (add-hook 'LaTeX-mode-hook #'TeX-auto-compile)
  :hook
  (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  ;; Font Setting
  (LaTeX-mode . (lambda () (setq-local buffer-face-mode-face '(:family "JetBrains Mono")) (buffer-face-mode)))
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . (lambda () (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))))
  :bind (:map LaTeX-mode-map
              ("C-c C-g" . pdf-sync-forward-search))
  )

(use-package preview-auto
  :ensure t
  :after latex
  :hook (latex-mode . preview-auto-setup)
  :config
  (setopt preview-protect-point t
          preview-locating-previews-message nil
          preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)
  
  ;; Uncomment the following only if you have followed the above
  ;; instructions concerning, e.g., hyperref:

  ;; (preview-LaTeX-command-replacements
  ;;  '(preview-LaTeX-disable-pdfoutput))
  )

(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex))

(use-package pdf-tools
  :ensure t
  :hook
  (pdf-view-mode . (lambda () (line-number-mode nil)))
  :config
  (pdf-tools-install :no-query)
  (setq-default pdf-sync-backward-display-action t
                pdf-sync-forward-display-action t))

(provide 'lang-latex)
;;; lang-latex.el ends here
