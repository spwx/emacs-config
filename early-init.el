;;; -*- lexical-binding: t -*-
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
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

;; Temp: Explicitly set PATH environment variable and update exec-path to match it.
;; (the string here should be copied from the PATH in Emacs.app/Contents/Info.plist)
(setenv "PATH" "/Users/spw/.local/bin:/Users/spw/.toolbox/bin:/Users/spw/go/bin:/Users/spw/.cargo/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Applications/iTerm.app/Contents/Resources/utilities")

(setq exec-path (split-string (getenv "PATH") path-separator))
