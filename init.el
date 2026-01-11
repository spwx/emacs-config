;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t -*-
;;
;; General Setting
;;

;; *** THIS MUST COME FIRST ***
;; Setup the package manager
(load-file (expand-file-name "use-package-config.el" user-emacs-directory))

;; Theme
(use-package ef-themes
  :config
  (load-theme 'ef-maris-dark t))

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

;; Pretty Modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Key mapping hints
(use-package which-key
  :custom (which-key-add-column-padding 4) ; or any larger number
  :init (which-key-mode))

;; Better help buffers
(use-package helpful
  :defer t
  :commands (helpful-symbol helpful-callable helpful-variable helpful-key helpful-at-point))

;; Better undo
(use-package undo-fu :defer t)
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
(use-package yaml-mode :defer t)
(use-package json-mode :defer t)

;; Helper for displaying diagnostic buffers at the bottom
(defun my/add-bottom-window-rule (buffer-regexp)
  "Add a display rule to show BUFFER-REGEXP in a bottom side window."
  (add-to-list 'display-buffer-alist
               `(,buffer-regexp
                 (display-buffer-reuse-window display-buffer-in-side-window)
                 (side . bottom)
                 (reusable-frames . visible)
                 (window-height . 0.25))))

;; Flycheck - syntax checking
(use-package flycheck
  :hook (rustic-mode . flycheck-mode)
  :general
  (my/leader-keys
    :keymaps 'flycheck-mode-map
    "le" '(flycheck-toggle-error-list :wk "Toggle errors"))
  :config
  (my/add-bottom-window-rule (rx bos "*Flycheck errors*" eos))
  ;; Toggle the Flycheck error list window
  (defun flycheck-toggle-error-list ()
    "Toggle the Flycheck error list window."
    (interactive)
    (let ((window (get-buffer-window "*Flycheck errors*")))
      (if window
          (quit-window nil window)
        (call-interactively #'flycheck-list-errors)))))

;; LSP Mode - Language Server Protocol client
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; Performance tuning
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  ;; Disable features we don't use (using Corfu + Tempel instead)
  (lsp-completion-provider :none)  ; Don't use company-mode
  (lsp-enable-snippet nil)         ; Don't use yasnippet
  ;; Rust-analyzer settings
  (lsp-rust-analyzer-cargo-watch-command "clippy")  ; Use clippy for on-save checks
  (lsp-rust-analyzer-display-inlay-hints t)
  ;; Inlay hint settings
  (lsp-inlay-hint-enable t)

  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-closing-brace-hints t)
  :config
  ;; Disable features handled by other packages
  (setq lsp-headerline-breadcrumb-enable nil))

;; LSP UI enhancements
(use-package lsp-ui
  :after lsp-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions t))

;; Rust development with rustic
(use-package rustic
  :after (flycheck lsp-mode)
  :custom
  ;; Use lsp-mode for better Rust support
  (rustic-lsp-client 'lsp-mode)
  ;; Format on save
  (rustic-format-on-save t)
  ;; Store cargo arguments for reuse with C-u
  (rustic-cargo-use-last-stored-arguments t)
  :general
  ;; Shift-K for hover docs (like Vim)
  (:states 'normal :keymaps 'rustic-mode-map
   "K" #'lsp-describe-thing-at-point
   "gr" #'lsp-find-references)
  (my/leader-keys
    :keymaps 'rustic-mode-map
    "l" '(:ignore t :wk "LSP")
    "la" '(lsp-execute-code-action :wk "Code actions")
    "lr" '(lsp-rename :wk "Rename")
    "lf" '(rustic-format-buffer :wk "Format")
    "ld" '(lsp-find-definition :wk "Definition")
    "lD" '(lsp-find-references :wk "References")
    "lh" '(lsp-describe-thing-at-point :wk "Hover doc")
    "lm" '(lsp-rust-analyzer-expand-macro :wk "Expand macro")
    "li" '(lsp-rust-analyzer-inlay-hints-mode :wk "Toggle inlay hints")
    ;; Cargo commands
    "c" '(:ignore t :wk "Cargo")
    "cb" '(rustic-cargo-build :wk "Build")
    "cc" '(rustic-cargo-check :wk "Check")
    "cr" '(rustic-cargo-run :wk "Run")
    "ct" '(rustic-cargo-test :wk "Test")
    "cT" '(rustic-cargo-current-test :wk "Test at point")
    "cl" '(rustic-cargo-clippy :wk "Clippy")
    "cf" '(rustic-cargo-fmt :wk "Cargo fmt")
    "cd" '(rustic-cargo-doc :wk "Open docs")
    "ca" '(rustic-cargo-add :wk "Add crate")
    "co" '(rustic-cargo-outdated :wk "Outdated")
    "cp" '(rustic-popup :wk "Popup")))

;; LSP support via Eglot (built-in) for non-Rust languages
(use-package eglot
  :ensure nil
  :custom
  (eglot-events-buffer-size 0)  ; Disable event logging for performance
  (eglot-autoshutdown t)        ; Shutdown server when last buffer closes
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
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
  ;; Display Flymake diagnostics at bottom of frame (like Flycheck)
  (my/add-bottom-window-rule (rx bos "*Flymake diagnostics" (* any) "*" eos))
  ;; Toggle the Flymake diagnostics window
  (defun flymake-toggle-diagnostics ()
    "Toggle the Flymake diagnostics window."
    (interactive)
    (let ((window (seq-find (lambda (w)
                              (string-match-p "\\*Flymake diagnostics.*\\*"
                                              (buffer-name (window-buffer w))))
                            (window-list))))
      (if window
          (quit-window nil window)
        (call-interactively #'flymake-show-buffer-diagnostics))))
  :general
  (:states 'normal :keymaps '(python-mode-map python-ts-mode-map
                              js-mode-map js-ts-mode-map typescript-mode-map typescript-ts-mode-map tsx-ts-mode-map
                              sh-mode-map bash-ts-mode-map
                              html-mode-map css-mode-map
                              json-mode-map json-ts-mode-map
                              yaml-mode-map yaml-ts-mode-map)
   "gr" #'xref-find-references)
  (my/leader-keys
    :keymaps '(python-mode-map python-ts-mode-map
               js-mode-map js-ts-mode-map typescript-mode-map typescript-ts-mode-map tsx-ts-mode-map
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
    "lh" '(eldoc :wk "Hover doc")
    "le" '(flymake-toggle-diagnostics :wk "Toggle errors")))
