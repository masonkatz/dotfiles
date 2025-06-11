;; early-emacs.el --- loads before GUI is initialized
;;; Commentary:
;;
;; Portable across MacOS and Linux, but assumes a fairly recent
;; version of EMACS.
;;
;;; Code:

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setopt inhibit-startup-screen t)
(setq site-run-file nil) ; don't include system level files (looking at you debian)
