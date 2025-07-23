;;
;; General Setting
;;

(when (display-graphic-p)
  (add-to-list 'default-frame-alist
               '(font . "VictorMono Nerd Font-18")))

(setopt which-key-add-column-padding 4) ; or any larger number

(which-key-mode)
;; Jump to a help window when one is opened
(setopt help-window-select t)

(setopt ring-bell-function 'ignore)

;; Store recently opened files
(recentf-mode 1)
(setopt recentf-max-menu-items 25)

;; Don't create annoying files
(setopt make-backup-files nil) ;; Disable backup~ files
(setopt auto-save-default nil) ;; Disable #autosave# files
(setopt create-lockfiles nil) ;; Disable .#lockfile

;; Make the window separator prettier in the terminal
(unless window-system  ; Terminal only
  (set-display-table-slot standard-display-table
                          'vertical-border
                          (make-glyph-code ?│)))


;;
;; Package Manager Configuration
;;

;; Enable package.el and add MELPA
(require 'package)
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Ensure package list is up to date
(unless package-archive-contents
  (package-refresh-contents))

;; Use use-package by default
(eval-when-compile
  (require 'use-package))

;; Auto install packages
(setopt use-package-always-ensure t)


;;
;; Additional packages
;;

;; Enable evil
(load-file (expand-file-name "evil.el" user-emacs-directory))

;; Theme
(use-package ef-themes
  :config
  (load-theme 'ef-maris-dark t))

(use-package doom-modeline
  :custom (column-number-mode t)
  :init (doom-modeline-mode 1))

(use-package org
  :custom
  (org-link-frame-setup '((file . find-file))) ;; open links in the current window
  ;; (org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
  (org-agenda-files '("~/org/logs/25/07-Jul/00-jul_misc_tasks.org"))
  (org-startup-folded 'overview)
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "STOP(s)" "DONE(d)")))
  (org-todo-keyword-faces '(("WAIT" . "orange")
          ("STOP" . (:foreground "dimgray" :strike-through t))))
  :config
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


;;
;; Minibuffer config
;;

;; Fido mode
(fido-mode 1)
(fido-vertical-mode 1)

;; Annotations
;; NOTE: Marginalia must be activated in the :init section
(use-package marginalia :init (marginalia-mode))


;; Icons
(use-package nerd-icons)
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Auto-completion
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  :config
  (corfu-popupinfo-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package emacs
  :custom
  ;; Enable indentation+completion using the TAB key.
  (tab-always-indent 'complete)
  ;; Disable Ispell completion function.
  (text-mode-ispell-word-completion nil)
  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))
;; Enable Corfu completion UI
;; See the Corfu README for more configuration tips.
(use-package corfu
  :init
  (global-corfu-mode))

;; Add extensions
(use-package cape
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
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
  ;; (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)
(use-package jinx
  :hook (emacs-startup . global-jinx-mode))

(use-package tempel
  :bind (("M-*" . tempel-complete)
       ("M-+" . tempel-insert)
       (:map tempel-map
             ("<tab>" . tempel-next)
             ("TAB" . tempel-next)
             ("<backtab>" . tempel-previous)))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  :config
  (evil-define-key 'visual 'global (kbd "gt") #'tempel-insert))

(use-package tempel-collection :after tempel)


;;
;; Custom Variable Configuration
;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
