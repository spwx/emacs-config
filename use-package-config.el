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

;; Track installed packages for package-autoremove (without disk I/O on each load)
(defun my/use-package-ensure-add-to-selected (name &rest _)
  "Add NAME to `package-selected-packages' in memory when use-package ensures it."
  (when (and (not (package-built-in-p name))
             (not (memq name package-selected-packages)))
    (add-to-list 'package-selected-packages name)))
(when (fboundp 'use-package-ensure-elpa)
  (advice-add 'use-package-ensure-elpa :after #'my/use-package-ensure-add-to-selected))
