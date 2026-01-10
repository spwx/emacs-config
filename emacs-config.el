(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)

;; Most of this is from: https://github.com/LionyxML/emacs-kick/
(use-package emacs
  :ensure nil
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (help-window-select t)                          ;; Jump to a help window when one is opened
  (require-final-newline t)                       ;; Always include a newline at EOF
  (sentence-end-double-space nil)                 ;; A sentence doesn't need two spaces after the period
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (recentf-max-menu-items 25)                     ;; Set number of files to remember
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
  (backup-directory-alist '(("." . "~/.emacs_backups/")))
  (backup-by-copying t)
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (inhibit-startup-message t)                     ;; Disable the startup message when Emacs launches.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.

  :config

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  (setopt custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.

  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  (server-start)

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.

  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.

  (global-hl-line-mode 1)      ;; Enable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)

  ;; Delete trailing whitespace when saving a file
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  ;; Add a hook to run code after Emacs has fully initialized.
  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs has fully loaded. This code runs after startup.")

              ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format
                         ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                         (emacs-init-time)
                         (number-to-string (length package-activated-list))))))))
