;;; -*- lexical-binding: t -*-
;;
;; Package Manager Configuration
;;

;; Enable package.el and add MELPA
(require 'package)
(setopt package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("gnu"   . "https://elpa.gnu.org/packages/")
                           ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Use use-package by default
(eval-when-compile
  (require 'use-package))

;; Auto install packages
(setopt use-package-always-ensure t)

;; Keep package-selected-packages in sync for package-autoremove
(defun my/use-package-ensure-advice (name &rest _)
  "Add NAME to `package-selected-packages' when use-package ensures it."
  (when (and (not (package-built-in-p name))
             (not (memq name package-selected-packages)))
    (customize-save-variable 'package-selected-packages
                             (cons name package-selected-packages))))
(advice-add 'use-package-ensure-elpa :after #'my/use-package-ensure-advice)
