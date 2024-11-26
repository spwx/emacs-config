;; Add homebrew to the PATH
(setenv "PATH" (concat "/opt/homebrew/bin:" (getenv "PATH"))) (add-to-list 'exec-path "/opt/homebrew/bin")

;; Hide startup screen
(setq inhibit-startup-screen t)

;; Highlight the current line
(global-hl-line-mode 1)

;; Convert the selected region from Org to GitHub Flavored Markdown and save it
;; to the clipboard
(defun spw-region-to-gfm-clipboard ()
  "Convert the selected region to GFM and copy to clipboard."
  (interactive)
  (if (use-region-p)
      (let* ((begin (region-beginning))
             (end (region-end))
             (buffer-content (buffer-substring-no-properties begin end))
             (temp-file (make-temp-file "region-to-gfm"))
             (escaped-temp-file (shell-quote-argument temp-file))
             (clipboard-command (if (eq system-type 'darwin) "pbcopy" "xclip -selection clipboard")))
        (with-temp-file temp-file
          (insert buffer-content))
        (condition-case err
            (progn
              (shell-command-to-string
               (format "/opt/homebrew/bin/pandoc -f org -t gfm --wrap=none %s | %s"
                       escaped-temp-file
                       clipboard-command))
              (delete-file temp-file)
              (message "Selected region converted to GFM and copied to clipboard."))
          (error
           (message "Error occurred: %s" (error-message-string err)))))
    (message "No region selected. Please select a region first.")))

;; Create backup directory if it doesn't exist
(unless (file-exists-p "~/.backups")
  (make-directory "~/.backups"))

;; Store all backup and auto-save files in the backup directory
(setq-default backup-directory-alist '(("." . "~/.backups"))
              auto-save-file-name-transforms '((".*" "~/.backups/" t))
              auto-save-list-file-prefix "~/.backups/")

;; Use versioned backups
(setq-default version-control t     ; Use version numbers for backups
              kept-new-versions 10  ; Number of newest versions to keep
              kept-old-versions 0   ; Number of oldest versions to keep
              delete-old-versions t ; Don't ask to delete excess backup versions
              backup-by-copying t)  ; Copy all files, don't rename them

;; Break lines at 80 characters.
(setq-default fill-column 80)  ; or any other number you prefer
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Delete trailing white space on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Show a menu on right click.
(when (display-graphic-p)
  (context-menu-mode))

;; Enable tabs
;; `previous-buffer' and `next-buffer' change tabs.
;; `bury-buffer' hides a tab.
(global-tab-line-mode)

;; Hide UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Use dired-x
(require 'dired-x)

;; Automatically switch to help windows
(setq help-window-select t)

;; Automatically close pairs (like parenthesis)
(electric-pair-mode)

;; Elpaca Setup
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.yasnippet")))
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet-capf
  :ensure t
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; Orderless searching for Consult and Corfu
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Display completion candidates in buffer
(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  :config
  (define-key corfu-map (kbd "S-SPC") 'corfu-insert-separator)
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "S-<return>") 'corfu-insert)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (global-corfu-mode))
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
;; Add backends to populate Corfu
(use-package cape
  :ensure t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-dict))

;; Enable vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Cycle through the annotations
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  (recentf-mode)

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-maris-dark t))
(use-package doom-modeline
  :ensure t
  :init
  ;; ;; Show the column number in the modeline
  (column-number-mode)
  ;; Setup date & time display
  (setopt display-time-default-load-average nil)
  (setopt display-time-24hr-format t)
  (setopt display-time-day-and-date t)
  (display-time)
  ;; Flash the mode line instead of ringing the system bell
  (defun flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))
  (setq visible-bell nil
        ring-bell-function 'flash-mode-line)
  ;; Make the modeline pretty
  (doom-modeline-mode 1))
(use-package anzu :ensure t :config (global-anzu-mode))

(defun is-first-line-heading ()
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (beginning-of-line)
    (looking-at org-heading-regexp)))

(defun spw-org-shift-right ()
  (interactive)
  (if (org-at-heading-p)
      (org-demote-subtree)
    (evil-shift-right-line 1)))

(defun spw-org-shift-right-visual ()
  (interactive)
  (if (is-first-line-heading)
      (org-do-demote)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-visual-restore)))

(defun spw-org-shift-left ()
  (interactive)
  (if (org-at-heading-p)
      (org-promote-subtree)
    (evil-shift-left-line 1)))

(defun spw-org-shift-left-visual ()
  (interactive)
  (if (is-first-line-heading)
      (org-do-promote)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-visual-restore)))

;; Evil keybinding
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  ;; global keys
  (evil-define-key '(normal visual) 'global (kbd "SPC u") 'universal-argument)
  (evil-define-key '(normal visual) 'global (kbd "g h") 'evil-first-non-blank)
  (evil-define-key '(normal visual) 'global (kbd "g l") 'evil-end-of-line)
  (evil-define-key '(normal visual) 'global (kbd "H") 'previous-buffer)
  (evil-define-key '(normal visual) 'global (kbd "L") 'next-buffer)
  ;; file opening keys
  (evil-define-key '(normal visual) 'global (kbd "SPC f s") 'save-buffer)
  (evil-define-key '(normal visual) 'global (kbd "SPC f f") 'consult-buffer)
  ;; org-mode keys
  ;; (evil-define-key 'visual org-mode-map (kbd ">>") 'org-do-demote)
  (evil-define-key 'visual org-mode-map (kbd ">>") 'spw-org-shift-right-visual)
  ;; (evil-define-key 'visual org-mode-map (kbd "<<") 'org-do-promote)
  (evil-define-key 'visual org-mode-map (kbd "<<") 'spw-org-shift-left-visual)
  (evil-define-key 'normal org-mode-map (kbd ">>") 'spw-org-shift-right)
  (evil-define-key 'normal org-mode-map (kbd "<<") 'spw-org-shift-left)
  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)
  ;; emacs-lisp-mode keys
  (evil-define-key 'normal emacs-lisp-mode-map (kbd "K")
    (lambda ()
      (interactive)
      (let ((symbol (thing-at-point 'symbol)))
      (when symbol
  	(describe-symbol (intern symbol))))))
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-commentary
  :after evil
  :ensure t
  :config (evil-commentary-mode))
(use-package evil-goggles
  :after evil
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))


;; Org-Mode stuff
(use-package org
  :ensure t
  :init
  (setq org-time-stamp-custom-formats
      '("<%Y-%m-%d %a %H:%M>" . "<%Y-%m-%d %a %H:%M>"))
  ;; Indent the content of a heading.
  ;; (setq org-indent-indentation-per-level 0)
  ;; (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  :config
  ;; Follow links to files in the same window.
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  (add-hook 'org-mode-hook (lambda ()
                             (electric-indent-local-mode -1))))
(use-package org-appear
  :ensure t
  :init
  (setq org-appear-autolinks t)
  (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  (defun open-and-focus-current-journal-file ()
  "Open the current day's journal file and focus on it."
  (interactive)
  (org-journal-open-current-journal-file)
  (let ((journal-buffer (org-journal-get-entry-path)))
    (when journal-buffer
      (switch-to-buffer (find-buffer-visiting journal-buffer)))))

  (add-hook 'after-init-hook 'open-and-focus-current-journal-file)
  :bind ("C-c j j" . org-journal-new-entry)
  :config
  (setq org-journal-dir "~/org/journal/"
	org-journal-find-file 'find-file
	org-journal-file-format "%Y/%m-%b/%d-%a.org"
	org-journal-date-format "%A, %d %B %Y"
        org-journal-file-type 'daily))


;; Setup spell checking
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :init
  (evil-define-key '(normal visual) 'global (kbd "z =") 'jinx-correct)
  (evil-define-key '(normal visual) 'global (kbd "] s") 'jinx-next)
  (evil-define-key '(normal visual) 'global (kbd "[ s") 'jinx-previous)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hint-enable t)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rustic-mode . lsp)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
(use-package dap-mode :ensure t)

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  (evil-define-key 'normal rustic-mode-map "K" 'lsp-describe-thing-at-point)
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
  :custom
  (rustic-cargo-use-last-stored-arguments t))

;; Magit needs a newer version of transient
(use-package transient :ensure t)
(use-package magit :ensure t :after transient)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "VictorMono Nerd Font" :foundry "nil" :slant normal :weight medium :height 220 :width normal))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("f019002925408f081e767c515e4fb4b1d7f1462228d6cd32ff66f06a43671527" "1fefcf9915617538b409d8aba3c6bbefddfcf2a80db09741aeef1457e1809c2b" "40352d95bc42c2e3acb7fc75afb3029d81a76897e14e9438857729cc87630980" "9fba87dbc0f14d5650006893ed53088be71f16d57b749394d9c485ef2326e85f" "79ab8329f4522beaa2285888d38f6204bb60f324912660d774a412a79e336d6c" "9ddb83c12595e789e9abd04a5c0705661748776223a794a6f64669352b956e79" "29a073e66535bad18e11e9bcaa17d7f2d17e4c79f01023e59e9841633915c232" "a087e01778a85f8381b2aa2b7b0832951aea078621b38844b6c8c8d638d73e3b" "97283a649cf1ffd7be84dde08b45a41faa2a77c34a4832d3884c7f7bba53f3f5" "aa04c854054e8d43245bd67ca619a7bede9171e2a2efb1b2c26caf1d031497eb" "d0dc7861b33d68caa92287d39cf8e8d9bc3764ec9c76bdb8072e87d90546c8a3" default))
 '(magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
 '(package-selected-packages
   '(magit consult lsp-mode ef-themes yasnippet which-key org-modern org-appear nerd-icons-dired jinx gruvbox-theme evil-surround evil-org evil-goggles evil-commentary evil-collection doom-themes doom-modeline corfu cape)))
(put 'dired-find-alternate-file 'disabled nil)
