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

;; Fix CC for macOS: GNU GCC from Homebrew can't compile macOS SDK headers
;; that use Objective-C block syntax, which breaks rust-analyzer's cargo check
(when (eq system-type 'darwin)
  (setenv "CC" "/usr/bin/cc"))

;; Emacs built-ins configuration
(load-file (expand-file-name "emacs-config.el" user-emacs-directory))

;; Vim (evil) key mappings configuration
(load-file (expand-file-name "evil-config.el" user-emacs-directory))

;; Org-mode configuration
(load-file (expand-file-name "org-config.el" user-emacs-directory))

;; Autocomplete and Minibuffer configuration
(load-file (expand-file-name "completions-config.el" user-emacs-directory))

;; Window placement rules (replaces manual display-buffer-alist entries)
(use-package shackle
  :config
  (setq shackle-rules
        '(("*Flycheck errors*"                    :align below :size 0.25)
          ("\\*Flymake diagnostics.*\\*" :regexp t :align below :size 0.25)
          ("*Calendar*"                            :align below :size 0.25 :select t)))
  (shackle-mode 1))

;; Popup window management
(use-package popper
  :general
  ("C-`" #'popper-toggle)
  ("M-`" #'popper-cycle)
  ("C-M-`" #'popper-toggle-type)
  (my/leader-keys
    "'" '(popper-toggle :wk "Toggle popup"))
  :init
  (setq popper-display-control nil)  ; defer placement to shackle
  (setq popper-reference-buffers
        '("\\*Flycheck errors\\*"
          "\\*Flymake diagnostics.*\\*"
          "\\*Calendar\\*"
          "\\*Messages\\*"
          ("\\*Warnings\\*" . hide)
          help-mode
          helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

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

;; Flycheck - syntax checking
(use-package flycheck
  :general
  (my/leader-keys
    :keymaps 'flycheck-mode-map
    "le" '(flycheck-toggle-error-list :wk "Toggle errors"))
  :config
  ;; Toggle the Flycheck error list window
  (defun flycheck-toggle-error-list ()
    "Toggle the Flycheck error list window."
    (interactive)
    (let ((window (get-buffer-window "*Flycheck errors*")))
      (if window
          (quit-window nil window)
        (call-interactively #'flycheck-list-errors)))))

;; TRAMP - remote file editing via SSH
(use-package tramp
  :ensure nil
  :defer t
  :config
  (add-to-list 'tramp-remote-path "~/.toolbox/bin")
  (add-to-list 'tramp-remote-path "~/.cargo/bin")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Rust development with rustic
(use-package rustic
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t)
  (rustic-cargo-use-last-stored-arguments t)
  :config
  ;; Eglot uses flymake; prevent rustic from also setting up flycheck
  (remove-hook 'rustic-mode-hook #'rustic-flycheck-setup)
  ;; Disable format-on-save for remote (TRAMP) buffers
  (add-hook 'rustic-mode-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local rustic-format-on-save nil))))
  :general
  (my/leader-keys
    :keymaps 'rustic-mode-map
    "cc" '(:ignore t :wk "Cargo")
    "ccb" '(rustic-cargo-build :wk "Build")
    "ccc" '(rustic-cargo-check :wk "Check")
    "ccr" '(rustic-cargo-run :wk "Run")
    "cct" '(rustic-cargo-test :wk "Test")
    "ccT" '(rustic-cargo-current-test :wk "Test at point")
    "ccl" '(rustic-cargo-clippy :wk "Clippy")
    "ccf" '(rustic-cargo-fmt :wk "Cargo fmt")
    "ccd" '(rustic-cargo-doc :wk "Open docs")
    "cca" '(rustic-cargo-add :wk "Add crate")
    "cco" '(rustic-cargo-outdated :wk "Outdated")
    "ccp" '(rustic-popup :wk "Popup")))

;; Breadcrumbs in header line (replaces lsp-headerline-breadcrumb-mode)
(use-package breadcrumb
  :init (breadcrumb-mode 1))

;; LSP support via Eglot (built-in)
(use-package eglot
  :ensure nil
  :custom-face
  (eglot-highlight-symbol-face ((t (:inherit highlight))))
  :custom
  (eglot-events-buffer-size 0)
  (eglot-autoshutdown t)
  :hook ((rustic-mode . eglot-ensure)
         (python-mode . eglot-ensure)
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
  ;; Work around eglot caching empty inlay hint results before the
  ;; server is ready.  Clear the region cache and refontify multiple
  ;; times to ensure all jit-lock chunks get processed.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (let ((buf (current-buffer)))
                (dolist (secs '(3 6 9))
                  (run-with-timer secs nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (when (bound-and-true-p eglot-inlay-hints-mode)
                            (setq eglot--outstanding-inlay-hints-last-region nil)
                            (font-lock-flush))))))))))
  (setq-default eglot-workspace-configuration
    '(:rust-analyzer (:check (:command "clippy"))))
  ;; Language server configurations
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("ty" "server")))
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
  (:states 'normal :keymaps 'eglot-mode-map
   "K" #'eldoc-doc-buffer
   "gr" #'xref-find-references)
  (my/leader-keys
    :keymaps 'eglot-mode-map
    "c" '(:ignore t :wk "Code")
    "ca" '(eglot-code-actions :wk "Code actions")
    "cr" '(eglot-rename :wk "Rename")
    "cf" '(eglot-format :wk "Format")
    "cd" '(xref-find-definitions :wk "Definition")
    "cD" '(xref-find-references :wk "References")
    "ch" '(eldoc-doc-buffer :wk "Hover doc")
    "ce" '(flymake-toggle-diagnostics :wk "Toggle errors")
    "ci" '(eglot-inlay-hints-mode :wk "Toggle inlay hints")))
