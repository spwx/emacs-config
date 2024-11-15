(setq inhibit-startup-screen t)

(defun region-to-gfm-clipboard ()
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

;; Use a vertical picker
;; (fido-vertical-mode)

(setq-default fill-column 80)  ; or any other number you prefer
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Open recent files
;; Disabled, using Consult now.
;; (keymap-global-set "C-x C-r" 'recentf-open)

;; Flash the mode line instead of ringing the system bell
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

;; Enable tabs
;; `previous-buffer' and `next-buffer' change tabs.
;; `bury-buffer' hides a tab.
(global-tab-line-mode)

;; Use dired-x
(require 'dired-x)

;; Hide UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Show the column number in the modeline
(column-number-mode)

;; Automatically switch to help windows
(setq help-window-select t)

;; Automatically close pairs (like parenthesis)
(electric-pair-mode)

;; Enable the MELPA package repository
(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs '("~/.yasnippet")))
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :ensure t)
(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

;; Add extensions
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
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))
;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
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
  :init (doom-modeline-mode 1))

(defun spw-org-shift-right ()
  (interactive)
  (if (org-at-heading-p)
      (org-demote-subtree)
    (evil-shift-right-line 1)))

(defun spw-org-shift-left ()
  (interactive)
  (if (org-at-heading-p)
      (org-promote-subtree)
    (evil-shift-left-line 1)))

;; Evil keybinding
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (define-key evil-normal-state-map (kbd "g h") 'evil-first-non-blank)
  (define-key evil-normal-state-map (kbd "g l") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "H") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "L") 'next-buffer)
  (evil-define-key 'visual org-mode-map ">>" 'org-do-demote)
  (evil-define-key 'visual org-mode-map "<<" 'org-do-promote)
  (evil-define-key 'normal org-mode-map ">>" 'spw-org-shift-right)
  (evil-define-key 'normal org-mode-map "<<" 'spw-org-shift-left)
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  ;; (evil-define-key 'normal org-mode-map ">>" 'org-demote-subtree)
  ;; (evil-define-key 'normal org-mode-map "<<" 'org-promote-subtree)
  (evil-collection-init))
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-commentary
  :ensure t
  :config (evil-commentary-mode))
(use-package evil-goggles
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
  ;; Follow links to files in the same window.
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  ;; Indent the content of a heading.
  ;; (setq org-indent-indentation-per-level 0)
  ;; (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars t)
  (setq org-startup-indented t)
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (electric-indent-local-mode -1))))
;; (use-package org-modern
;;   :ensure t
;;   :init (setq org-modern-star 'replace)
;;   :hook (org-mode . org-modern-mode))
(use-package org-appear
  :ensure t
  :init
  (setq org-appear-autolinks t)
  (setq org-hide-emphasis-markers t)
  :hook (org-mode . org-appear-mode))

;; Setup spell checking
;; To compile the `dylib` for this package on MacOS:
;;    brew install enchant
;;    cd ~/.config/emacs/elpa/jinx*
;;    gcc -O2 -Wall -Wextra -fPIC -shared -o jinx-mod.dylib jinx-mod.c \
;;      -I/opt/homebrew/opt/enchant/include/enchant-2 \
;;      -L/opt/homebrew/opt/enchant/lib -lenchant-2
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hint-enable t)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rustic-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config (evil-define-key 'normal rustic-mode-map "K" 'lsp-describe-thing-at-point))
;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; optionally if you want to use debugger
(use-package dap-mode :ensure t)

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(use-package magit :ensure t)

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
 '(package-selected-packages
   '(magit consult lsp-mode ef-themes yasnippet which-key org-modern org-appear nerd-icons-dired jinx gruvbox-theme evil-surround evil-org evil-goggles evil-commentary evil-collection doom-themes doom-modeline corfu cape)))
(put 'dired-find-alternate-file 'disabled nil)
