;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t -*-
;;
;; General Setting
;;

;; *** THIS MUST COME FIRST ***
;; Setup the package manager
(load-file (expand-file-name "use-package-config.el" user-emacs-directory))

;; Inherit PATH from shell (fixes macOS GUI Emacs not seeing shell paths)
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

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

;; Better help buffers
(use-package helpful)

;; Better undo
(use-package undo-fu)
(use-package undo-fu-session
  :config (undo-fu-session-global-mode))

;; GC optimization - restores gc-cons-threshold after startup
(use-package gcmh
  :init (gcmh-mode 1))

;; Show number of search results
(use-package anzu
  :config (global-anzu-mode +1))
(use-package evil-anzu
  :after (evil anzu))

;; Better navigation
(use-package avy
  :general
  (:keymaps 'global :states 'normal "s" #'avy-goto-char-timer)
  (:keymaps 'global :states 'normal "gs" #'avy-resume))

;; Colorful delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         ;; (text-mode . rainbow-delimiters-mode)
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
    "gg" '(magit :wk "Magit!"))
  :custom
  ;; Make Magit full screen
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  ;; Turns on magit nerd-icons
  (magit-format-file-function #'magit-format-file-nerd-icons))

;; Terminal Emulator
(use-package eat
  :general
  (my/leader-keys
    "t" '(eat :wk "Eat (terminal)"))
  :config
  (add-hook 'eat-exit-hook
          (lambda (_process)
			;; Get the window of the current buffer
            (let ((win (get-buffer-window (current-buffer))))
			  ;; If the current buffer is alive, kill it
              (when (buffer-live-p (current-buffer))
                (kill-buffer (current-buffer)))
			  ;; If there is more than 1 window, delete the window the current
			  ;; buffer was in
              (when (and win (> (length (window-list)) 1))
                (delete-window win)))))
  (when (eq system-type 'darwin)
	(define-key eat-semi-char-mode-map (kbd "C-h")  #'eat-self-input)
	(define-key eat-semi-char-mode-map (kbd "<backspace>") (kbd "C-h"))))

;; Try to configure indentation per file
(use-package dtrt-indent
  :config (dtrt-indent-global-mode 1))

;; Tree-sitter for better syntax highlighting and structural editing
;; NOTE: Disabled - causes lag when scrolling through files in find-file
;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install nil)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode t))

;; Language mode packages (for non-tree-sitter modes)
(use-package rust-mode :defer t)
(use-package typescript-mode :defer t)
(use-package yaml-mode :defer t)
(use-package json-mode :defer t)

;; LSP support via Eglot (built-in)
(use-package eglot
  :ensure nil
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (html-mode . eglot-ensure)
         (css-mode . eglot-ensure)
         (json-mode . eglot-ensure)
         (json-ts-mode . eglot-ensure)
         (yaml-mode . eglot-ensure)
         (yaml-ts-mode . eglot-ensure)
         (sh-mode . eglot-ensure)
         (bash-ts-mode . eglot-ensure))
  :config
  ;; Language server configurations
  ;; Python: ty (default), alternatives: pyright, pylsp
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ty" "server")))
  ;; TypeScript/JavaScript: typescript-language-server
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode)
                 . ("typescript-language-server" "--stdio")))
  ;; Rust: rust-analyzer
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer")))
  ;; Bash: bash-language-server
  (add-to-list 'eglot-server-programs
               '((sh-mode bash-ts-mode) . ("bash-language-server" "start")))
  ;; HTML/CSS: vscode-langservers-extracted
  (add-to-list 'eglot-server-programs
               '(html-mode . ("vscode-html-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(css-mode . ("vscode-css-language-server" "--stdio")))
  ;; JSON: vscode-json-languageserver
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode) . ("vscode-json-language-server" "--stdio")))
  ;; YAML: yaml-language-server
  (add-to-list 'eglot-server-programs
               '((yaml-mode yaml-ts-mode) . ("yaml-language-server" "--stdio")))
  :general
  (my/leader-keys
    :keymaps '(python-mode-map python-ts-mode-map
               js-mode-map js-ts-mode-map typescript-mode-map typescript-ts-mode-map tsx-ts-mode-map
               rust-mode-map rust-ts-mode-map
               sh-mode-map bash-ts-mode-map
               html-mode-map css-mode-map
               json-mode-map json-ts-mode-map
               yaml-mode-map yaml-ts-mode-map)
    "l" '(:ignore t :wk "LSP")
    "la" '(eglot-code-actions :wk "Code actions")
    "lr" '(eglot-rename :wk "Rename")
    "lf" '(eglot-format :wk "Format")
    "ld" '(xref-find-definitions :wk "Definition")
    "lD" '(xref-find-references :wk "References")
    "lh" '(eldoc :wk "Hover doc")))
