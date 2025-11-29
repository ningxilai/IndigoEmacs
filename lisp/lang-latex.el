;; -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package latex-change-env
  :ensure t
  :after latex-mode
  :bind (:map LaTeX-mode-map ("C-c r" . latex-change-env)))

(use-package LaTeX-mode
  :ensure (auctex)
  :commands LaTeX-mode
  :mode "\\.tex" "\\.stex" "\\.texi"
  :init
  ;; PDF View
  (setq-default TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
                TeX-view-program-selection '((output-pdf "PDF Tools")))
  :config
  (setopt TeX-auto-save t
          TeX-parse-self t
          TeX-show-compilation t
          TeX-electric-math '("$" . "$")
          TeX-electric-sub-and-superscript t)
  (setopt TeX-output-view-style  '(("^pdf$" "." "xpdf %o %(outpage)"))
          LaTeX-babel-hyphen nil
          LaTeX-indent-level 4
          LaTeX-item-indent 0
          LaTeX-command "latexmk -shell-escape -bibtex -pdf -g -f %f"
          ;; "latex -shell-escape --synctex=1"
          TeX-command-default "XeLaTeX")
  (setq-default reftex-cite-prompt-optional-args t   ; Prompt for empty optional arguments in cite
                reftex-cite-format 'biblatex
                reftex-plug-into-AUCTeX t
                reftex-insert-label-flags t
                reftex-save-parse-info t
                reftex-enable-partial-scans t
                reftex-use-multiple-selection-buffers t)
  (eval-after-load 'reftex '(reftex-isearch-minor-mode))
  (setq-default TeX-master nil
                TeX-engine 'xetex
                TeX-PDF-mode t
                TeX-source-correlate-start-server t
                TeX-source-correlate-mode t
                TeX-source-correlate-method '((dvi . source-specials)
                                              (pdf . synctex)))

  (defun move-line-region-down (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines down."
    (interactive "*p")
    (move-text-internal arg))

  (defun move-line-region-up (arg)
    "Move region (transient-mark-mode active) or current line
  arg lines up."
    (interactive "*p")
    (move-text-internal (- arg)))

  

  (defun description (beg end)
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{description}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item[ ]"))
    (end-of-buffer)
    (insert "\\end{description}\n")))

(defun enumerate (beg end)
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{enumerate}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item "))
    (end-of-buffer)
    (insert "\\end{enumerate}\n")))

(defun itemize (beg end)
 "wrap the active region in an 'itemize' environment,
  converting hyphens at the beginning of a line to \item"
  (interactive "r")
  (save-restriction
    (narrow-to-region beg end)
    (beginning-of-buffer)
    (insert "\\begin{itemize}\n")
    (while (re-search-forward "^- " nil t)
      (replace-match "\\\\item "))
    (end-of-buffer)
    (insert "\\end{itemize}\n")))



:hook
  (tex-after-compilation-finished-functions . tex-revert-document-buffer)
  ;; font setting
  (LaTeX-mode . (lambda ()
                  (setq buffer-face-mode-face
                        '(:family "jetbrains mono"))
                  (buffer-face-mode)
                  (visual-line-mode)))
  (LaTeX-mode . (lambda ()
                (turn-on-reftex)
                (LaTeX-math-mode 1)
                (auto-fill-mode 1)           ;; It causes issues and M-q saves the day.
                (TeX-source-correlate-mode 1))) ;; open PDF in the edditing page
  (LaTeX-mode . (lambda ()
                  (add-to-list 'TeX-command-list '("xelatex" "%`xelatex --synctex=1%(mode)%' %t" tex-run-tex nil t))))
  :bind (:map latex-mode-map
              ("c-c c-g" . pdf-sync-forward-search)
              ("M-<down>" . move-line-region-down)
              ("M-<up>" . move-line-region-up))
  )

(use-package preview-auto
  :ensure t
  :after LaTeX-mode
  :init (preview-auto-setup)
  :config
  (setopt preview-protect-point t
          preview-locating-previews-message nil
          preview-leave-open-previews-visible t)
  :custom
  (preview-auto-interval 0.1)

  ;; Uncomment the following only if you have followed the above
  ;; instructions concerning, e.g., hyperref:

  ;; (preview-LaTeX-command-replacements625. inv(0)625. inv(0)625. inv(0)
  ;;  '(preview-LaTeX-disable-pdfoutput))
  )

(use-package cdlatex
  :ensure t
  :hook
  (LaTeX-mode . turn-on-cdlatex))

(use-package parsec
  :ensure (:host github :repo "cute-jumper/parsec.el"))
(use-package latex-math-preview
  :ensure (:host github :repo "emacsmirror/latex-math-preview"))

(use-package calc-textrail
  :ensure (:host github
                 :repo "bohonghuang/calc-textrail")
  :hook (calc-mode . calc-textrail-mode))

(use-package laas
  :ensure t
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt")))

  ;; copy by cireu

  (defun cm/calc-int (exp)
    (require 'calc)
    (require 'calc-lang)
    (ignore-errors
      (calc-create-buffer)
      (calc-radians-mode)
      (calc-latex-language nil)
      (calc-eval
       (concat "integ("
               exp
               ")"))))

  ;; # -*- mode: snippet -*-
  ;; # name: doint
  ;; # key: doint
  ;; # --
  ;; ${1:Enter Your Formula here$(when yas-moving-away-p (cm/calc-int yas-text))}$0

  )

;; **** Useful keybindings for viewing PDFs
;; |------------------------------------------+-----------------|
;; | Display                                  |                 |
;; |------------------------------------------+-----------------|
;; | Zoom in / Zoom out                       | ~+~ / ~-~       |
;; | Fit height / Fit width / Fit page        | ~H~ / ~W~ / ~P~ |
;; | Trim margins (set slice to bounding box) | ~s b~           |
;; | Reset margins                            | ~s r~           |
;; | Reset z oom                              | ~0~             |
;; |------------------------------------------+-----------------|
;;
;; **** Useful keybindings for navigating PDFs
;;
;; |-----------------------------------------------+-----------------------|
;; | Navigation                                    |                       |
;; |-----------------------------------------------+-----------------------|
;; | Scroll Up / Down by Page-full                 | ~space~ / ~backspace~ |
;; | Scroll Up / Down by Line                      | ~C-n~ / ~C-p~         |
;; | Scroll Right / Left                           | ~C-f~ / ~C-b~         |
;; | First Page / Last Page                        | ~<~ / ~>~             |
;; | Next Page / Previous Page                     | ~n~ / ~p~             |
;; | First Page / Last Page                        | ~M-<~ / ~M->~         |
;; | Incremental Search Forward / Backward         | ~C-s~ / ~C-r~         |
;; | Occur (list all lines containing a phrase)    | ~M-s o~               |
;; | Jump to Occur Line                            | ~RETURN~              |
;; | Pick a Link and Jump                          | ~F~                   |
;; | Incremental Search in Links                   | ~f~                   |
;; | History Back / Forwards                       | ~l~ / ~r~             |
;; | Display Outline                               | ~o~                   |
;; | Jump to Section from Outline                  | ~RETURN~              |
;; | Jump to Page                                  | ~M-g g~               |
;; | Store position / Jump to position in register | ~m~ / ~'~             |
;; |-----------------------------------------------+-----------------------|

(use-package pdf-tools
  :ensure t
  :preface (setq doc-view-continuous t
                 doc-view-resolution 144
                 large-file-warning-threshold (* 50 (expt 2 20)))
  :init
  (setq-default pdf-sync-backward-display-action t
                pdf-sync-forward-display-action t)
  :custom
  (pdf-view-use-scaling t)
  (pdf-view-use-imagemagick t)
  (pdf-view-resize-factor 1.1)
  (pdf-view-display-size 'fit-page) ;; 'fit-width
  (pdf-annot-activate-created-annotations t)
  :hook
  (pdf-view-mode . (lambda () (progn (line-number-mode nil)
                                (pdf-view-themed-minor-mode)
                                (pdf-view-auto-slice-minor-mode)
                                (pdf-isearch-minor-mode))))
  :bind (:map pdf-view-mode-map
         ("C-s" . isearch-forward))
  :config
  (pdf-tools-install :no-query))

(use-package bibtex
  :ensure nil
  :mode "\\.bib"
  :config
  (defun get-bibtex-from-doi (doi)
    "Get a BibTeX entry from the DOI"
    (interactive "MDOI: ")
    (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
      (with-current-buffer
          (url-retrieve-synchronously
           (format "http://dx.doi.org/%s"
                   (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
        (switch-to-buffer (current-buffer))
        (goto-char (point-max))
        (setq bibtex-entry
              (buffer-substring
               (string-match "@" (buffer-string))
               (point)))
        (kill-buffer (current-buffer))))
    (insert (decode-coding-string bibtex-entry 'utf-8))
    (define-key bibtex-mode-map (kbd "C-c C-b") 'get-bibtex-from-doi)
    (bibtex-fill-entry))
  ;; I want run the above function to define it upon entry into a Bibtex file.

  (defun add-doi ()
    (interactive)
    (progn
      (setq doi-to-query (read-string "DOI "))
      (find-file "~/Documents/global.bib")
      (end-of-buffer)
      (doi-insert-bibtex doi-to-query)
      )
    )
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))

  :hook
  (bibtex-mode . (lambda ()
                   (get-bibtex-from-doi nil))))

(use-package bibtex-completion
  :ensure t
  :config
  (setq bibtex-completion-pdf-field "file"
        bibtex-completion-bibliography '("~/Documents/global.bib")
        bibtex-completion-library-path '("~/0papersLabeled/")
        bibtex-completion-notes-path "~/Documents/notes/"
        bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))
  :custom
  (bibtex-autokey-year-length 4)
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-titlewords 2)
  (bibtex-autokey-titlewords-stretch 1)
  (bibtex-autokey-titleword-length 5))

(use-package ebib
  :ensure t
  :config
  (setq ebib-bibtex-dialect 'biblatex))

(use-package biblio
  :defer t
  :bind
  (:map biblio-selection-mode-map
        ("C-j" . biblio--selection-next)
        ("C-k" . biblio--selection-previous)))

(provide 'lang-latex)
;;; lang-latex.el ends here
