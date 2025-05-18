;; -*- lexical-binding: t; -*-

(use-package latex-change-env
  :ensure t
  :after latex
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env)))

(use-package latex
  :defer t
  :ensure auctex
  :commands (TeX-latex-mode)
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-show-compilation t)
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'xetex)
  (with-eval-after-load 'tex-mode
    ;; "latexmk -shell-escape -bibtex -xelatex -g -f %f"
    (add-to-list 'tex-compile-commands '("xelatex %f" t "%r.pdf"))
    (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))) ;; https://emacs-china.org/t/auctex-setup-synctex-with-pdf-tools-not-working/11257/2
  
  (setq TeX-output-view-style (quote (("^pdf$" "." "xpdf %o %(outpage)"))))
  (setq TeX-command-default "XeLaTeX")
  
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
                TeX-source-correlate-start-server t
                TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
;; PDF View
  (setq pdf-sync-backward-display-action t
        pdf-sync-forward-display-action t
        TeX-source-correlate-mode t
        TeX-source-correlate-method '((dvi . source-specials)
                                      (pdf . synctex))
        TeX-source-correlate-start-server t
        reftex-plug-into-AUCTeX t)
  (with-eval-after-load 'latex
    (define-key LaTeX-mode-map 
                (kbd "C-c C-g") #'pdf-sync-forward-search))
  (add-hook 'LaTeX-mode
          (defun init-latex-mode ()
            "Stuff to do when opening `LaTeX-mode' files."
            (add-save-hook 'after-save-hook
                           (lambda ()
                             (TeX-command-run-all nil))
                             nil t)))
  :hook
  (pdf-view-mode-hook . (lambda() (line-number-mode -1)))
  (TeX-after-TeX-LaTeX-command-finished-hook . TeX-revert-document-buffer)
  ;; Font Setting
  (LaTeX-mode-hook . (lambda () (setq buffer-face-mode-face '(:family "Hack")) (buffer-face-mode)))
  )

(use-package preview-auto
  :ensure t
  :after latex
  :hook (LaTeX-mode . preview-auto-setup)
  :config
  (setq preview-protect-point t)
  (setq preview-locating-previews-message nil)
  (setq preview-leave-open-previews-visible t)
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
  (LaTeX-mode-hook . turn-on-cdlatex)
  (LaTeX-mode-hook . turn-on-reftex))

(provide 'lang-latex)
