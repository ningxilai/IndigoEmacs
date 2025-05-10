;; Color  -*- lexical-binding: t; -*-

;; Color

(use-package hl-line
  :ensure nil
  :init
  (setq hl-line-sticky-flag nil)
  ;; Highlight starts from EOL, to avoid conflicts with other overlays
  (setq hl-line-range-function (lambda () (cons (line-end-position)
                                           (line-beginning-position 2)))))
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (font-lock-add-keywords nil
    `((,page-delimiter ;; variable with the regexp (usually "^\f" or "^^L")
        0
        (prog1 nil
          ;; don't display ^L
          (compose-region (match-beginning 0) (match-end 0) "")
          ;; make an overlay (like in hl-line)
          (let ((pdl (make-overlay (line-beginning-position)
                                   (line-beginning-position 2))))
            ;; :background has to be different from the background color
            ;; gray1 here is just a little different from black
            (overlay-put pdl 'face '(:underline "gray30" :background "gray1"))
            (overlay-put pdl 'modification-hooks
                         ;; these arguments are received from modification-hooks
                         '((lambda (overlay after-p begin end &optional length)
                             (delete-overlay overlay))))
            (overlay-put pdl 'insert-in-front-hooks
                         '((lambda (overlay after-p begin end &optional length)
                             (delete-overlay overlay)))))) t)))))


(use-package symbol-overlay :ensure t :init (setq symbol-overlay-mode t))

(provide 'color)
