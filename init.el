(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(load-file (expand-file-name "config.el" user-emacs-directory))

(setq gc-cons-threshold (* 100 1024 1024))

(setq fonts '("IBM Plex Mono" "Noto Sans Mono CJK SC"))
  (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 14))

(use-package dashboard
  :ensure t
  :defer t
    :hook (after-init . 'dashboard-mode-hook)
  :preface
  (defun update-config ()
    "Update Witchmacs to the latest version."
    (interactive)
    (let ((dir (expand-file-name user-emacs-directory)))
      (if (file-exists-p dir)
          (progn
            (message "Witchmacs is updating!")
            (cd dir)
            (shell-command "git pull")
            (message "Update finished. Switch to the messages buffer to see changes and then restart Emacs"))
        (message "\"%s\" doesn't exist." dir))))

  (defun create-scratch-buffer ()
    "Create a scratch buffer"
    (interactive)
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-banner-logo-title "W I T C H M A C S - The cutest Emacs distribution!")
  (setq dashboard-startup-banner "~/.emacs.d/marivector.png")
  (setq dashboard-center-content t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-init-info t)
  (setq dashboard-init-info (format "%d packages loaded in %s"
                                    (length package-activated-list) (emacs-init-time)))
  (setq dashboard-set-footer t)
)

