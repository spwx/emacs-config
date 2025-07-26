(use-package org
  :custom
  (org-link-frame-setup '((file . find-file))) ;; open links in the current window
  (org-agenda-files '("~/org/logs/2025/07-July/00-july_tasks.org"))
  (org-startup-folded 'overview)
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "STOP(s)" "DONE(d)")))
  (org-todo-keyword-faces '(("WAIT" . "orange")
          ("STOP" . (:foreground "dimgray" :strike-through t))))
  :config
  ;; evil-collection's org-agenda bindings are set for motion mode
  (evil-set-initial-state 'org-agenda-mode 'motion)
  ;; Make Org Pretty
  (add-hook 'org-mode-hook #'org-indent-mode))

;; Pretty bullets in Org
(use-package org-bullets
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . org-bullets-mode))

(use-package org-appear
  :after org
  :custom
  (org-appear-autolinks t)
  (org-appear-autokeywords t)
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  :hook
  (org-mode . org-appear-mode))

(use-package org-download
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-journal
  :general
  (my/leader-keys
    "oj" '(:ignore t :wk "Journal")
    "ojt" '(org-journal-open-current-journal-file :wk "Create entry")
    "ojj" '(org-journal-new-entry :wk "Create entry"))
  (my/leader-keys
    :keymaps 'org-journal-mode-map
    "ojp" '(org-journal-previous-entry :wk "Previous entry")
    "ojn" '(org-journal-next-entry :wk "Next entry"))
  :custom
  ;; Fullscreen journal window
  (org-journal-find-file-fn 'find-file)
  (org-journal-enable-agenda-integration t)
  (org-journal-dir "~/org/logs/")
  ;; the name of the actual file must include %Y%m%d !!!
  (org-journal-file-format "%Y/%m-%B/%Y%m%d.org"))

(use-package org
  :preface

  ;; Convert Org to MD and copy to Clipboard
  (defun org-to-gfm-clipboard ()
    "Convert the selected region from org format to GitHub Flavored Markdown and copy to clipboard."
    (interactive)
    (if (use-region-p)
	(let ((start (region-beginning))
	      (end (region-end)))
	  (shell-command-on-region
	   start end
	   "/opt/homebrew/bin/pandoc -f org -t gfm --wrap=none | pbcopy")
	  (message "Region converted to GFM and copied to clipboard"))
      (message "No region selected")))

  ;; Better org-do-demote
  (defun my/org-smart-demote ()
    "Demote heading if on one, otherwise call `evil-shift-right-line`."
    (interactive)
    (if (org-at-heading-p)
	(org-do-demote)
      (evil-shift-right-line 1)))

  ;; Better org-demote-subtree
  (defun my/org-smart-demote-subtree ()
    "Demote subtree if on a heading, otherwise call `evil-shift-right-line`."
    (interactive)
    (if (org-at-heading-p)
	(org-demote-subtree)
      (evil-shift-right-line 1)))

  ;; Better org-do-promote
  (defun my/org-smart-promote ()
    "Promote heading if on one, otherwise call `evil-shift-right-line`."
    (interactive)
    (if (org-at-heading-p)
	(org-do-promote)
      (evil-shift-left-line 1)))

  ;; Better org-demote-subtree
  (defun my/org-smart-promote-subtree ()
    "Promote subtree if on a heading, otherwise call `evil-shift-right-line`."
    (interactive)
    (if (org-at-heading-p)
	(org-promote-subtree)
      (evil-shift-left-line 1)))

  :general
  ;; Org mode key mappings
  (my/leader-keys
    :keymaps 'global
    :states '(normal visual)
    "o"  '(:ignore t :wk "Org")
    "oa" '(org-agenda :wk "Agenda")
    "oc" '(org-capture :wk "Capture"))
  (my/leader-keys
    :keymaps 'org-mode-map
    :states '(normal)
    "oe" '(org-export-dispatch :wk "Export")
    "ot" '(org-todo :wk "Todo")
    "oT" '(org-show-todo-tree :wk "Todo tree")
    "or" '(org-refile :wk "Refile")
    "oB" '(org-babel-tangle :wk "tangle")
    "oi"  '(:ignore t :wk "Insert")
    "oit" '(org-insert-structure-template :wk "Insert template")
    "oi." '(org-timestamp :wk "Timestamp")
    "oii" '(org-download-clipboard :wk "Image from Clipboard")
    "oid" '(org-deadline :wk "Deadline")
    "ois" '(org-schedule :wk "Schedule")
    "oil" '(org-insert-link :wk "Insert link")
    "oiy" '(org-store-link :wk "Store link"))
  (my/leader-keys
    :keymaps 'org-mode-map
    :states '(visual)
    "om" '(org-to-gfm-clipboard :wk "Convert to MD"))

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
)
;; Shift-K in text/org modes
(evil-define-key 'normal text-mode-map (kbd "K") #'dictionary-lookup-definition)
(evil-define-key 'normal outline-mode-map (kbd "K") #'dictionary-lookup-definition)

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
