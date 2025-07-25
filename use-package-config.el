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
