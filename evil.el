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
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'org-agenda-mode 'normal))

;; Evil Collection
(use-package evil-collection
  :after evil
  :init
  ;; put this *before* (evil-collection-init)
  (setopt evil-collection-key-blacklist '("SPC"))
  :config
  (evil-collection-init))

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :config
  (evil-terminal-cursor-changer-activate)) ; or (etcc-on)

;; Setup key binds
(use-package evil-nerd-commenter
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines))

(use-package general
  :config

  (general-create-definer my/leader-keys
  :states '(normal visual motion)
  :prefix "SPC"
  :non-normal-prefix "C-SPC"
  :keymaps 'override)

  (general-define-key :states '(normal visual) "gl" 'evil-last-non-blank)
  (general-define-key :states '(normal visual) "gh" 'evil-first-non-blank)

  (my/leader-keys
    "q" '(save-buffers-kill-terminal :wk "Quit Emacs")
    "x" '(execute-extended-command :wk "Execute command")
    "`" '(evil-buffer :wk "Last active buffer")
    ":" '(eval-expression :wk "Evaluate expression")
    "w" '(:keymap evil-window-map :wk "Windows")
    "b" '(:ignore t :wk "Buffers")
    "bb" '(switch-to-buffer :wk "Switch Buffer")
    "SPC" '(switch-to-buffer :wk "Switch Buffer")
    "bd" '(evil-delete-buffer :which-key "Delete Buffer")
    "bn" '(next-buffer :which-key "next")
    "bp" '(previous-buffer :which-key "prev")
    "f" '(:ignore t :wk "Files")
    "ff" '(find-file :wk "Find file")
    "fr" '(recentf :wk "Recent files")
    "fs" '(save-buffer :wk "Save file")
    "fc" '((lambda () (interactive)
           (let ((default-directory user-emacs-directory))
             (call-interactively #'find-file)))
           :wk "Find config file")
    "o"  '(:ignore t :wk "Org")
    "oa" '(org-agenda :wk "Agenda")
    "oc" '(org-capture :wk "Capture"))
  (my/leader-keys
    :keymaps 'org-mode-map
    :states '(normal)
    "od" '(org-deadline :wk "Deadline")
    "oe" '(org-export-dispatch :wk "Export")
    "oi" '(org-insert-structure-template :wk "Insert template")
    "ol" '(org-store-link :wk "Store link")
    "ot" '(org-todo :wk "Todo")
    "os" '(org-schedule :wk "Schedule")
    "oT" '(org-show-todo-tree :wk "Todo tree")
    "or" '(org-refile :wk "Refile")
    "og" '(org-goto :wk "Goto")
    "ob" '(org-switchb :wk "Switch buffer")
    "oB" '(org-babel-tangle :wk "tangle"))

  ;; q quits the org-agenda while in normal mode
  (with-eval-after-load 'org-agenda
    (general-define-key
     :states 'normal
     :keymaps 'org-agenda-mode-map
     "q" #'org-agenda-quit))

    (general-define-key
     :states 'normal
     :keymaps 'global
     "z=" #'jinx-correct)

    ;; Org Keybindings
    (defun my/org-smart-demote ()
    "Demote heading if on one, otherwise call `evil-shift-right-line`."
	(interactive)
	(if (org-at-heading-p)
	    (org-do-demote)
	    (evil-shift-right-line 1)))

    (defun my/org-smart-demote-subtree ()
    "Demote subtree if on a heading, otherwise call `evil-shift-right-line`."
	(interactive)
	(if (org-at-heading-p)
	    (org-demote-subtree)
	    (evil-shift-right-line 1)))

    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     ">>" #'my/org-smart-demote)
    
    (general-define-key
     :states 'visual
     :keymaps 'org-mode-map
     ">" #'my/org-smart-demote)

    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     ">s" #'my/org-smart-demote-subtree)

    (defun my/org-smart-promote ()
    "Promote heading if on one, otherwise call `evil-shift-right-line`."
	(interactive)
	(if (org-at-heading-p)
	    (org-do-promote)
	    (evil-shift-left-line 1)))

    (defun my/org-smart-promote-subtree ()
    "Promote subtree if on a heading, otherwise call `evil-shift-right-line`."
	(interactive)
	(if (org-at-heading-p)
	    (org-promote-subtree)
	    (evil-shift-left-line 1)))

    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "<<" #'my/org-smart-promote)
    
    (general-define-key
     :states 'visual
     :keymaps 'org-mode-map
     "<" #'my/org-smart-promote)
    
    (general-define-key
     :states 'normal
     :keymaps 'org-mode-map
     "<s" #'my/org-smart-promote-subtree)

    (general-define-key
     :keymaps 'org-read-date-minibuffer-local-map
     "C-l" #'org-calendar-forward-day
     "C-h" #'org-calendar-backward-day
     "C-j" #'org-calendar-forward-week
     "C-k" #'org-calendar-backward-week
     "C-S-l" #'org-calendar-forward-month
     "C-S-h" #'org-calendar-backward-month
     "C-S-j" #'org-calendar-forward-year
     "C-S-k" #'org-calendar-backward-year)

    (general-define-key
     :keymaps 'org-agenda-mode-map
     :states '(normal motion)
     :prefix "SPC"
     "a" '(org-agenda :wk "Agenda") ;; Example
     "q" '(org-agenda-quit :wk "Quit agenda"))
)

(defun my/describe-symbol-at-point ()
  "Show help for the symbol under the cursor and switch to the help window."
  (interactive)
  (let ((sym (symbol-at-point)))
    (when sym
      (describe-symbol sym)
      (let ((help-window (get-buffer-window "*Help*")))
        (when help-window
          (select-window help-window))))))

;; Shift-K in text/org modes
(evil-define-key 'normal text-mode-map (kbd "K") #'dictionary-lookup-definition)
(evil-define-key 'normal outline-mode-map (kbd "K") #'dictionary-lookup-definition)

;; Shift-K in Elisp mode
(defun my/describe-symbol-at-point ()
  "Show help for the symbol under the cursor and switch to the help window."
  (interactive)
  (let ((sym (symbol-at-point)))
    (when sym
      (describe-symbol sym)
      (let ((help-window (get-buffer-window "*Help*")))
        (when help-window
          (select-window help-window))))))
(evil-define-key 'normal emacs-lisp-mode-map (kbd "K") #'my/describe-symbol-at-point)

;; Make <escape> abort like C-g in the minibuffer (Emacs 28+)
(defun my/minibuffer-escape ()
  "Bind <escape> to `keyboard-escape-quit' (same effect as C-g)."
  (local-set-key (kbd "<escape>") #'keyboard-escape-quit))
(add-hook 'minibuffer-setup-hook #'my/minibuffer-escape)

;; RET opens a link or checks a box
(defun my/org-activate-link-or-checkbox ()
  "In normal mode, check checkbox or follow link under point."
  (interactive)
  (cond
   ;; If on a checkbox, toggle it
   ((org-at-item-checkbox-p)
    (call-interactively #'org-toggle-checkbox))
   ;; If on a link, open it
   ((org-in-regexp org-link-any-re)
    (call-interactively #'org-open-at-point))
   ;; Otherwise, do nothing or beep sarcastically
   (t (message "Not on a link or checkbox, pal!"))))
(with-eval-after-load 'evil
  (evil-define-key 'normal org-mode-map (kbd "RET") #'my/org-activate-link-or-checkbox))
