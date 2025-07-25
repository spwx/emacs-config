;;
;; General Setting
;;

;; Set font
(when (display-graphic-p)
  (add-to-list 'default-frame-alist
               '(font . "VictorMono Nerd Font-18")))

;; Jump to a help window when one is opened
(setopt help-window-select t)

;; Turn off the bell
(setopt ring-bell-function 'ignore)

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Store recently opened files
(recentf-mode 1)
(setopt recentf-max-menu-items 25)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't create annoying files
(setopt make-backup-files nil) ;; Disable backup~ files
(setopt auto-save-default nil) ;; Disable #autosave# files
(setopt create-lockfiles nil) ;; Disable .#lockfile

;; Delete trailing whitespace when saving a file
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Setup the package manager
(load-file (expand-file-name "use_package_config.el" user-emacs-directory))

;; Vim keys
(load-file (expand-file-name "evil_config.el" user-emacs-directory))

;; Org configuration
(load-file (expand-file-name "org_config.el" user-emacs-directory))

;; Autocomplete and Minibuffer configuration
(load-file (expand-file-name "completions_config.el" user-emacs-directory))

;; Theme
(use-package ef-themes
  :config
  (load-theme 'ef-maris-dark t))

;; Modeline
(use-package doom-modeline
  :custom (column-number-mode t)
  :init (doom-modeline-mode 1))

;; Key mapping hints
(use-package which-key
  :custom (which-key-add-column-padding 4) ; or any larger number
  :init (which-key-mode))

;; Better undo
(use-package undo-fu)
(use-package undo-fu-session
  :config (undo-fu-session-global-mode))

;; Show number of search results
(use-package anzu
  :config (global-anzu-mode +1))
(use-package evil-anzu
  :after evil)

;; Better navigation
(use-package avy
  :general
  (:keymaps 'global :states 'normal "s" #'avy-goto-char-timer)
  (:keymaps 'global :states 'normal "gs" #'avy-resume))

;; Colorful delimeters
(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)
         (conf-mode . rainbow-delimiters-mode)))

;; Spell checking
(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :general
  (:keymaps 'global :states 'normal "z=" #'jinx-correct))

;; Snippets
(use-package tempel
  :bind
    (:map tempel-map
	    ("<tab>" . tempel-next)
	    ("TAB" . tempel-next)
	    ("<backtab>" . tempel-previous))
  :general
  (:keymaps 'global :states '(normal visual) "gt" #'tempel-insert)
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; More Snippets
(use-package tempel-collection :after tempel)


;;
;; Custom Variable Configuration
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("/Users/spw/org/logs/2025/07-July/00-july_tasks.org"
     "/Users/spw/Documents/org/logs/2025/07-July/20250725.org") nil nil "Customized with use-package org")
 '(package-selected-packages nil)
 '(package-vc-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
