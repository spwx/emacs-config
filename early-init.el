;;; -*- lexical-binding: t -*-
(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font-18"))

;; Startup speed, annoyance suppression
(setq gc-cons-threshold most-positive-fixnum)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
