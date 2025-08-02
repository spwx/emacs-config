;;
;; General Setting
;;

;; Set font
(when (display-graphic-p)
  (add-to-list 'default-frame-alist
               '(font . "VictorMono Nerd Font-18")))

;; *** THIS MUST COME FIRST ***
;; Setup the package manager
(load-file (expand-file-name "use-package-config.el" user-emacs-directory))

;; Emacs built-ins configuration
(load-file (expand-file-name "emacs-config.el" user-emacs-directory))

;; Vim (evil) key mappings configuration
(load-file (expand-file-name "evil-config.el" user-emacs-directory))

;; Org-mode configuration
(load-file (expand-file-name "org-config.el" user-emacs-directory))

;; Autocomplete and Minibuffer configuration
(load-file (expand-file-name "completions-config.el" user-emacs-directory))

;; Theme
(use-package ef-themes
  :config
  (load-theme 'ef-maris-dark t))

;; Pretty Modeline
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

;; Colorful delimiters
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

;; Snippet Engine
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

;; Snippets
(use-package tempel-collection :after tempel)

;; Git gutters
(use-package diff-hl
  :general
  (my/leader-keys
    "g" '(:ignore t :wk "Git")
    "gd" '(diff-hl-show-hunk :wk "Diff hunk")
    "gn" '(diff-hl-next-hunk :wk "Next hunk")
    "gp" '(diff-hl-previous-hunk :wk "Previous hunk")
    "gs" '(diff-hl-stage-dwim :wk "Stage hunk")
    "gr" '(diff-hl-revert-hunk :wk "Revert hunk")
    "gu" '(diff-hl-unstage-file :wk "Unstage all"))
  :init
  ;; Activate diff-hl-mode
  (global-diff-hl-mode)
  ;; Update gutters on the fly
  (diff-hl-flydiff-mode)
  :custom
  (diff-hl-show-staged-changes nil))

;; Magit!
(use-package magit
  :general
  (my/leader-keys
    "g" '(:ignore t :wk "Git")
    "gg" '(magit :wk "Magit!"))
  :custom
  ;; Make Magit full screen
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  ;; Turns on magit nerd-icons
  (magit-format-file-function #'magit-format-file-nerd-icons))

;; Setup treesitter
(use-package treesit-auto
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))
