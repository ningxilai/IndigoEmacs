;; -*- lexical-binding: t; -*-

;; Org

(use-package org
    :ensure (org :host github
                 :repo "bzg/org-mode"
                 :branch "main")
    :defines org-mode-map
    :config
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "INITIATED(I)"
           "WAITING(w)"
           "SOMEDAY(smd)"
           "|"
           "DONE(d)"  ; Task successfully completed
           "CANCELLED(c)"
           "KILL(k)") ; Task was cancelled, aborted, or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . org-todo-active)
          ("STRT" . org-todo-active)
          ("[?]"  . org-todo-onhold)
          ("WAIT" . org-todo-onhold)
          ("HOLD" . org-todo-onhold)
          ("PROJ" . org-todo-project)
          ("NO"   . org-todo-cancel)
          ("KILL" . org-todo-cancel)))

    ;; pdflatex is not very efficient, but only pdflatex supports tikz

  (setq org-latex-logfiles-extensions
        '("lof" "lot" "tex~" "aux" "idx" "log" "out"
          "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
          "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
          "tex" "bcf"))

  (setq org-latex-classes
        '("ews"
          "\\documentclass[11pt, twoside, hidelinks]{memoir}
        \\setstocksize{9.25in}{7.5in}
        \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
        \\setlrmarginsandblock{1.5in}{1in}{*}
        \\setulmarginsandblock{1in}{1.5in}{*}
        \\checkandfixthelayout
        \\layout
        \\setcounter{tocdepth}{0}
        \\renewcommand{\\baselinestretch}{1.25}
        \\setheadfoot{0.5in}{0.75in}
        \\setlength{\\footskip}{0.8in}
        \\chapterstyle{bianchi}
        \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
        \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
        \\setsubsubsecheadstyle{\\normalfont\\centering}
        \\pagestyle{myheadings}
        \\usepackage[font={small, it}]{caption}
        \\usepackage{ccicons}
        \\usepackage{ebgaramond}
        \\usepackage[authoryear]{natbib}
        \\bibliographystyle{apalike}
        \\usepackage{svg}
\\hyphenation{mini-buffer}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list
   'org-preview-latex-process-alist
   '((dvisvgm
      :programs ("xelatex" "dvisvgm")
      :description "xdv > svg"
      :message "you need to install the programs: xelatex and dvisvgm."
      :image-input-type "xdv"
      :image-output-type "svg"
      :image-size-adjust (1.0 . 1.0)
      :latex-compiler ("xelatex --no-pdf -interaction nonstopmode -output-directory %o %f")
      :image-converter ("dvisvgm %f --libgs=/lib/libgs.so -e -n -b min -c %S -o %O")))
   '((imagemagick
     :programs ("latex" "convert")
     :description "pdf > png"
     :message "you need to install the programs: xelatex and imagemagick."
     :image-input-type "pdf"
     :image-output-type "png"
     :image-size-adjust (1.0 . 1.0)
     :latex-compiler ("xelatex -interaction nonstopmode -output-directory %o %f")
     :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O"))))

  (setq org-preview-latex-default-process 'dvisvgm) ; 'imagemagick
  (setq-local org-format-latex-header (concat "% xelatex\n" org-format-latex-header))
  (plist-put org-format-latex-options :scale 1.2)

  ;; This must be deferred after `org-preview-latex-process-alist' is set

  ;; patch for org-9.7.* to display transparent png image.
  ;; it seems there is an Emacs bug to display transparency.
  (when (<= (string-to-number org-version) 9.7)
    (define-advice org--make-preview-overlay
        (:override (beg end image &optional imagetype))
      "Build an overlay between BEG and END using IMAGE file.
   Argument IMAGETYPE is the extension of the displayed image,
   as a string.  It defaults to \"png\"."
      (let ((ov (make-overlay beg end))
            (imagetype (or (intern imagetype) 'png)))
        (overlay-put ov 'org-overlay-type 'org-latex-overlay)
        (overlay-put ov 'evaporate t)
        (overlay-put ov
                     'modification-hooks
                     (list (lambda (o _flag _beg _end &optional _l)
                             (delete-overlay o))))
        (overlay-put ov
                     'display
                     (list 'image :type imagetype :file image :ascent 'center :mask 'heuristic))))
    )

  (mapc
   (lambda (kv)
     (setf (alist-get (car kv) org-src-lang-modes) (cdr kv)))
   '(("haskell" . haskell-ts)
     ("python" . python-ts)
     ("scheme" . scheme)
     ("lilypond" . LilyPond)))

  :hook
  (org-mode . org-margin-mode)
  (org-mode . org-toggle-pretty-entities)
  (org-mode . (lambda ()(progn
                     (visual-line-mode t)
                     (org-link-preview t))))

  (org-after-refile-insert . (lambda ()
                               (when (bound-and-true-p org-capture-is-refiling)
                                 (save-buffer))))
  (org-capture-mode . (lambda ()
                        (setq header-line-format
                              (format "%s%s%s"
                                      (propertize (abbreviate-file-name (buffer-file-name (buffer-base-buffer)))
                                                  'face 'font-lock-string-face)
                                      org-eldoc-breadcrumb-separator
                                      header-line-format))))

  :custom
  (org-emphasis-alist
   '(("*" bold)
     ("/" org-emphasis-italic)
     ("_" underline)
     ("=" org-emphasis-verbatim org-verbatim verbatim)
     ("~" org-emphasis-code org-code verbatim)
     ("+" (:strike-through t))))

  (org-directory "etc/org")
  (org-id-locations-file '(expand-file-name ".orgids" org-directory))
  (org-list-allow-alphabetical t)

  (calendar-week-start-day t) ;; test
  (org-agenda-files (list org-directory))
  (org-agenda-deadline-faces
     '((1.001 . error)
       (1.0 . org-warning)
       (0.5 . org-upcoming-deadline)
       (0.0 . org-upcoming-distant-deadline)))
     ;; Don't monopolize the whole frame just for the agenda
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-span 10)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-day "-3d")
  (org-agenda-inhibit-startup t)
  (org-agenda-tags-column 0)

  (org-agenda-custom-commands
   '(("P"
      "List of all projects"
      tags
      "LEVEL=2/PROJ")

     ("E"
      "Agenda, next actions and waiting"
      ((agenda "" ((org-agenda-overriding-header "Next three days:")
                   (org-agenda-span 3)
                   (org-agenda-start-on-weekday nil)))
       (todo "NEXT" ((org-agenda-overriding-header "Next Actions:")))
       (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))
       ))))

  (org-modules '(ol-doi ol-w3m ol-bbdb ol-bibtex ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-eww))
  (org-export-backends '(ascii html latex man md odt texinfo))
  (org-export-with-sub-superscripts nil)

  (org-fontify-inline-src-blocks t)
  (org-fontify-todo-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-block-delimiter-line t)
  (org-fontify-whole-heading-line t)
  (org-fontify-done-headline t)

  (org-hide-emphasis-markers t)
  (org-hide-leading-stars t)
  (org-hide-macro-markers t)
  (org-hide-block-startup nil)

  (org-format-latex-options '(:foreground default :background "Transparent" :scale 1.0 :html-foreground auto :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (org-highlight-latex-and-related '(native latex script entities))
  (org-image-actual-width nil) ;; '450
  (org-latex-prefer-user-labels t)
  (org-latex-compiler "xelatex")
  (org-latex-listings 'minted)
  (org-latex-src-block-backend 'minted)
  (org-latex-packages-alist '(("" "color") ("" "minted") ("" "parskip") ("" "tikz") ("" "amsfonts") ("" "amsmath") ("" "amsthm") ("fontset=macnew,UTF8" "ctex")))
  (org-latex-pdf-process
   (if (executable-find "latexmk")
       '("latexmk -shell-escape -bibtex -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
       "pdflatex -interaction nonstopmode -output-directory %o %f"
       "bibtex %b")
     ))

  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-support-shift-select t)
  (org-startup-folded 'showeverything)
  (org-startup-with-inline-images t)
  (org-startup-indented nil)
  (org-use-sub-superscripts '{})

  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)
     org-refile-use-outline-path 'file
     org-outline-path-complete-in-steps nil))

  (org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯")))

  (org-auto-align-tags nil)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)
  ;; org-startup-indented t

  (org-src-preserve-indentation nil)  ; use native major-mode indentation
  (org-src-window-setup 'current-window)
  (org-src-tab-acts-natively nil)     ; we do this ourselves

  (org-ellipsis "…")
  (org-tags-column 0)
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-image-actual-width '(800))
  (org-catch-invisible-edits 'showeverything) ;; 'show-and-error
  ;; org-indent-mode-turns-on-hiding-stars nil

  (org-archive-subtree-save-file-p t)
  (org-num-skip-tags '("export" "nonum"))

  (org-effort-property "EFFORT")
  (org-id-locations-file-relative t)
  (org-id-link-to-org-use-id t)
  (org-display-remote-inline-images 'download) ; TRAMP urls

  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y")

  (org-html-validation-link nil)

  (org-indirect-buffer-display 'new-frame)
  (org-enforce-todo-dependencies t)
  (org-fold-catch-invisible-edits 'show)

  (org-columns-default-format
   "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")

  (org-imenu-depth 4)
  (org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . shadow)))

  (org-startup-tuncated t)

  :custom-face
  (org-document-title ((t (:height 1.50 :underline nil))))
  (org-level-1 ((t (:height 1.40))))
  (org-level-2 ((t (:height 1.30))))
  (org-level-3 ((t (:height 1.20))))
  (org-level-4 ((t (:height 1.20))))
  (org-level-5 ((t (:height 1.15))))
  (org-level-6 ((t (:height 1.10))))
  (org-level-7 ((t (:height 1.05))))
  (org-level-8 ((t (:height 1.05))))
  (org-tag ((t (:inherit 'fixed-pitch))))
  (org-date ((t (:inherit 'fixed-pitch))))
  (org-todo ((t (:inherit 'fixed-pitch))))
  (org-done ((t (:inherit 'fixed-pitch))))
  (org-drawer ((t (:inherit 'fixed-pitch))))
  (org-ellipsis ((t (:inherit 'fixed-pitch))))
  (org-property-value ((t (:inherit 'fixed-pitch))))
  (org-special-keyword ((t (:inherit 'fixed-pitch))))
  (org-headline-done ((t (:inherit 'variable-pitch))))

  (org-block ((t (:inherit 'fixed-pitch))))

  (org-num-face ((t (:inherit 'org-special-keyword :underline nil :weight bold))))

  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  (:map org-mode-map ;; override keybindings
        ("C-'" . avy-goto-char)
        ("C-S-<left>"  . windower-move-border-left)
        ("C-S-<right>" . windower-move-border-right)
        ("C-S-<up>"    . windower-move-border-above)
        ("C-S-<down>"  . windower-move-border-below))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit)
        ("C-c C-'" . separedit))
  )

(use-package org-babel
  :ensure nil
  :config
  (setq ;; You don't need my permission (just be careful, mkay?)
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        ;; Our :lang common-lisp module uses sly, so...
        org-babel-lisp-eval-fn #'sly-eval)
  (setq org-babel-load-languages
        '((emacs-lisp . t) (gnuplot . t) (shell . t) (latex . t) (lilypond . t) (lisp . t) (dot . t)))
  (setq org-src-block-faces
        '(("emacs-lisp" (:background "#EEE2FF" :inherit fixed-pitch :extend t))
          ("python" (:background "#e5ffb8" :inherit fixed-pitch :extend t))))
  )

(use-package org-capture
  :ensure nil
  :custom
  (org-capture-templates
   '(("f" "Fleeting note" item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Web Paste" entry
      (file "~/Org/Paste.org")
      "* [[%:link][%:description]] \n %U \n %:initial \n")
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?"))))

(use-package org-protocol-capture-html
  :ensure (:host github
                 :repo "alphapapa/org-protocol-capture-html"))

(use-package org-attach
  :ensure nil
  :init
  (setq-local org-attach-store-link-p 'attached     ; store link after attaching files
              org-attach-use-inheritance t) ; inherit properties from parent nodes
  :commands (org-attach-delete-one
             org-attach-delete-all
             org-attach-new
             org-attach-open
             org-attach-open-in-emacs
             org-attach-reveal-in-emacs
             org-attach-url
             org-attach-set-directory
             org-attach-sync)
  :config
  (unless org-attach-id-dir
    ;; Centralized attachments directory by default
    (setq-default org-attach-id-dir (expand-file-name ".attach/" org-directory))
    (add-to-list 'projectile-globally-ignored-directories org-attach-id-dir)))

(use-package org-cite
  :ensure nil
  :config
  (setq org-cite-global-bibliography
        (ensure-list
         (or (bound-and-true-p citar-bibliography)
             (bound-and-true-p bibtex-completion-bibliography)))
        ;; Setup export processor; default csl/citeproc-el, with biblatex for
        ;; latex
        org-cite-export-processors '((latex biblatex) (t csl))
        org-support-shift-select t)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package org-contrib
  :ensure (:host github :repo "emacsmirror/org-contrib"))

(use-package toc-org
  :ensure t
  :after org-mode
  :hook (org-mode . toc-org-mode))

(use-package htmlize
  :ensure t)



(use-package org-download
  :ensure t
  :config
  (advice-add
   #'org-download--dir-1
   :override ;; this does not work for temporary buffers,
   (lambda () (concat "./" (file-name-base (buffer-file-name)) ".assets")))
  :custom
  (org-download-heading-lvl nil)
  (org-download-screenshot-method "spectacle -br -o %s") ;; 'directory
  (org-download-image-dir "./img")
  :bind
  (:map org-mode-map
        :prefix-map org-download-cmd-map
        :prefix "C-c y"
        ("c" . org-download-clipboard)
        ("e" . org-download-edit)
        ("i" . org-download-image)
        ("s" . org-download-screenshot)
        ("y" . org-download-yank))
  :hook (org-mode . org-download-enable))

;; (use-package writegood-mode
;;   :ensure t
;;   :bind
;;   (("C-c w s r" . writegood-reading-ease))
;;   :hook
;;   (text-mode . writegood-mode))

;; (use-package titlecase
;;   :ensure t
;;   :bind
;;   (("C-c s t" . titlecase-dwim)
;;    ("C-c s c" . ews-org-headings-titlecase)))

;; (use-package lorem-ipsum
;;   :ensure t
;;   :custom
;;   (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
;;   :init
;;   (setq lorem-ipsum-sentence-separator
;;         (if sentence-end-double-space "  " " "))
;;   :bind
;;   (("C-c w s i" . lorem-ipsum-insert-paragraphs)))

(use-package org-margin
  :ensure (:host github :repo "rougier/org-margin")
  :hook (org-mode . org-margin-mode))

(use-package valign
  :ensure t
  :custom (valign-fancy-bar t)
  :hook (org-mode . valign-mode))



(use-package org-edit-indirect
  :ensure t
  :hook (org-mode . org-edit-indirect-mode))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-inside-latex t)
  (org-appear-autokeywords t)
  (org-appear-autoentities t)
  (org-appear-autoemphasis t)
  (org-appear-autosubmarkers t)
  (org-appear-autolinks 'just-brackets))

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :config
  (require 'nerd-icons)

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file_o"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  " ; need this because the default padding is too low for these icons
     :tag "has:files"))
  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-link"
              :face 'nerd-icons-orange
              :v-adjust 0.01)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-codicon
              "nf-cod-note"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "    "
     :tag "has:notes"))
  (defvar citar-indicator-cited-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-circle_o"
              :face 'nerd-icon-green)
     :function #'citar-is-cited
     :padding "  "
     :tag "is:cited"))

  (setq citar-indicators
        (list citar-indicator-files-icons
              citar-indicator-notes-icons
              citar-indicator-links-icons))
  :custom
  (citar-bibliography '("~/Documents/global.bib"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :init (citar-embark-mode))

(use-package org-ref
  :ensure t
  :bind (:map org-mode-map
         ("C-c h c" . org-ref-insert-cite-function)
         ("C-c h r" . org-ref-insert-ref-function)
         ("C-c h l" . org-ref-insert-label-function)
         ("C-c h d" . 'doi-add-bibtex-entry)
         ("C-c h a" . org-ask-location))
  :config
  (defun org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 9)))
         (hd (condition-case nil
                 (car (org-refile-get-location "Tag" nil t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
  (end-of-line)))

(provide 'lang-org)

;; ends here
