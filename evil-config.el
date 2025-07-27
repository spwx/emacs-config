;; Evil mode
(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  (evil-respect-visual-line-mode t)
  (evil-undo-system 'undo-fu)
  (evil-want-fine-undo t)
  :config
  (evil-mode 1)

  ;; make "gl"/"gh" available everywhere
  (evil-define-key '(normal visual) 'global
    (kbd "gl") #'evil-last-non-blank
    (kbd "gh") #'evil-first-non-blank))

;; Evil everywhere
(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; Comments
(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-define-key 'normal 'global (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (evil-define-key 'visual 'global (kbd "gc") 'evilnc-comment-or-uncomment-lines))

;; Surround
(use-package evil-surround
  :config (global-evil-surround-mode 1))

;; Highlight yank & paste
(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

;; Setup key mappings
(use-package general
  :config
  (general-create-definer my/leader-keys
    :states '(normal visual motion)
    :prefix "SPC"
    :non-normal-prefix "C-SPC"
    :keymaps 'override)

  ;; Global Key Mappings
  (my/leader-keys
    "q" '(save-buffers-kill-terminal :wk "Quit Emacs")
    "x" '(execute-extended-command :wk "Execute command")
    "`" '(evil-buffer :wk "Last active buffer")
    ":" '(eval-expression :wk "Evaluate expression")
    "w" '(:keymap evil-window-map :wk "Windows")
    "b" '(:ignore t :wk "Buffers")
    "bb" '(switch-to-buffer :wk "Switch Buffer")
    "bd" '(evil-delete-buffer :which-key "Delete Buffer")
    "bn" '(next-buffer :which-key "next")
    "bp" '(previous-buffer :which-key "prev")
    "f" '(:ignore t :wk "Files")
    "ff" '(find-file :wk "Find file")
    "fr" '(consult-recent-file :wk "Recent files")
    "fs" '(save-buffer :wk "Save file")
    "fc" '((lambda () (interactive)
             (let ((default-directory user-emacs-directory))
               (call-interactively #'find-file)))
           :wk "Config files")
    "h" '(:ignore t :wk "Help")
    "ho" '(describe-symbol :wk "Help")
    "hk" '(describe-key :wk "Help")
    "hm" '(describe-mode :wk "Help")
    ))

;; Shift-K in Elisp mode
(defun my/describe-symbol-at-point ()
  "Show help for the symbol under the cursor"
  (interactive)
  (let ((sym (symbol-at-point)))
    (when sym
      (describe-symbol sym))))
(evil-define-key 'normal emacs-lisp-mode-map (kbd "K") #'my/describe-symbol-at-point)

;; Make <escape> abort like C-g in the minibuffer (Emacs 28+)
(defun my/minibuffer-escape ()
  "Bind <escape> to `keyboard-escape-quit' (same effect as C-g)."
  (local-set-key (kbd "<escape>") #'keyboard-escape-quit))
(add-hook 'minibuffer-setup-hook #'my/minibuffer-escape)
