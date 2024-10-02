;; emacs.el --- top level config -*- eval: (outline-minor-mode) -*-
;;; Commentary:
;;
;; Portable across MacOS and Linux, but assumes a fairly recent
;; version of EMACS.
;;
;;; Code:

;;;; Bootstrap straight package manager

;; Trying to stick with simpler configs, but copilot needs use-package.
;; Hopefully it goes into melpa soon.

;; https://github.com/raxod502/straight.el#getting-started

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         nil 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;;;; Functions

(defun mjk/install (package)
  "Installs package if not already installed"
  (unless (package-installed-p package)
    (package-install package)))

(defun mjk/macos-graphic-p ()
  "Return true if running as an Application on MacOS"
  (eq window-system 'ns))

(defun mjk/font-size ()
  "Return font size to use based on resolution."
  (let* ((geometry (cdr (assoc 'geometry (car (display-monitor-attributes-list)))))
	 (resolution (cddr geometry)))
    (* 10 (cdr (assoc (cddr geometry) mjk/resolution-font-size-alist)))))

(defun mjk/window-config ()
  "Set the frame defaults, font, font size, ..."
  (when (display-graphic-p)
    (window-divider-mode)
    (when (mjk/macos-graphic-p)
      (when (find-font (font-spec :name "JetBrains Mono"))
	(set-face-attribute 'default nil :family "JetBrains Mono")
	(global-ligature-mode t))
      (set-face-attribute 'default nil :height (mjk/font-size))
      (set-fontset-font "fontset-default" 'hebrew
			(font-spec :family "Arial Hebrew" :size (* .12 (mjk/font-size)))))))


;;;; Startup

(require 'package)
(require 'server)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
					;(package-refresh-contents)

(unless (server-running-p)
  (server-start))

(when (display-graphic-p)
  (mjk/install 'exec-path-from-shell)
  (setq default-directory "~/"))

(when (mjk/macos-graphic-p)
  (unbind-key "s-t")
  (unbind-key "s-,"))

;;;; Dashboard

(mjk/install 'dashboard)

(dashboard-setup-startup-hook)


;;;; Treemacs, Font, Icons & Ligatures

(when (display-graphic-p)
  (mjk/install 'treemacs)
  (mjk/install 'ligature)
  (mjk/install 'nerd-icons) ; (nerd-icons-install-fonts)
  (mjk/install 'nerd-icons-dired)
  (mjk/install 'treemacs-nerd-icons)
  ;;  (treemacs-load-theme "nerd-icons")
  (global-set-key (kbd "C-c t") 'treemacs-select-window)
  (ligature-set-ligatures 'prog-mode '("++" "--"
				       ">=" "<="
				       "+=" "-=" "/=" "*=" "|=" "~=" "^="
				       ":=" "!=" "==" "==="
				       "/*" "*/" "//"
				       "::" "<<" ">>"
				       "<-" "->"
				       "||" "&&"
				       "...")))


(add-hook 'dired-mode-hook 'nerd-icons-dired-mode)

(add-hook 'window-setup-hook 'mjk/window-config)

(defvar mjk/resolution-font-size-alist '(((1280 800)  . 14)
					 ((1440 900)  . 14)
					 ((1470 956)  . 14)
					 ((1512 982)  . 14)
					 ((1680 1050) . 14)
					 ((1920 1080) . 14)
					 ((2560 1440) . 16)
					 ((3008 1692) . 16)
					 ((3360 1890) . 16)
					 ((3440 1440) . 16)
					 ((3840 2160) . 20))
  "Font sizes for different monitors.")


;;;; Modeline

(mjk/install 'doom-modeline)

(doom-modeline-mode 1)
(size-indication-mode)

;;;; Zenburn

(mjk/install 'zenburn-theme)
(load-theme 'zenburn t)

;;;; Misc

(mjk/install 'rainbow-mode)		; highlights color strings
(mjk/install 'hl-todo)

;;;; Git

(mjk/install 'git-gutter-fringe)
(mjk/install 'magit)

;;;; Company

(mjk/install 'company)
(global-company-mode t)

;;;; Yasnippet

(mjk/install 'yasnippet)
(mjk/install 'yasnippet-snippets)

;;;; Prettier

(mjk/install 'prettier-js)				; add prettier-js-mode to mode hooks


;;;; Treesitter

(require 'treesit)

(defun mjk/install-treesitter-grammars ()
  "Download and install treesitter grammar files."
  (interactive)
  (dolist (grammar
	   '(
	     (c   . ("https://github.com/tree-sitter/tree-sitter-c"))
	     (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	     (go     . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.1"))
	     (gomod  . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
	     (gowork . ("https://github.com/omertuc/tree-sitter-go-work"))
	     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	     (json . ("https://github.com/tree-sitter/tree-sitter-json"))
	     (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
	     ))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(mjk/install-treesitter-grammars)

;;;; LSP (eglot) / Copilot

(require 'eglot)
(require 'flymake)

(setq-default eglot-workspace-configuration
	      '((:gopls .
                        ((local . "git.softiron.com,github.com/endobit")
                         (staticcheck . t)))))


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t)

;;;; prog-mode


(add-hook 'prog-mode-hook
	  (lambda ()
	    (flyspell-prog-mode)
	    (electric-pair-mode)
	    (hl-todo-mode)
	    (yas-minor-mode)
	    (when (display-graphic-p)
	      (git-gutter-mode)
	      (hl-line-mode)
	      (display-fill-column-indicator-mode)
	      (display-line-numbers-mode))))


;;;; Lisp

; Lisp indents with both tabs and spaces. If we redefine tabs the file will look
; bad outside of our editor.

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
    	    (setq tab-width 8)))

;;;; C/C++

(require 'c-ts-mode)

(add-hook 'c-ts-base-mode-hook
	  (lambda ()
	    (copilot-mode)
	    (eglot-ensure)
	    (flymake-mode)
	    (c-ts-mode-toggle-comment-style -1) ; c++ style comments
	    (add-hook 'before-save-hook
		      (lambda ()
			(eglot-format-buffer)))))

;;;; Go

(add-to-list 'auto-mode-alist '("\\.go\\'" .  go-ts-mode))

(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (copilot-mode)
	    (eglot-ensure)
	    (add-hook 'before-save-hook
		      (lambda ()
			(eglot-format-buffer)))))

;;;; GNU Makefile

(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))

;;;; Just

(mjk/install 'just-mode)

(add-hook 'just-mode-hook
	  (lambda ()
	    (setq just-indent-offset 4)))

;;;; Markdown

(mjk/install 'markdown-mode)

;;;; JSON / YAML

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(add-hook 'json-ts-mode-hook
	  (lambda ()
	    (prettier-js-mode)))

(add-hook 'yaml-ts-mode-hook
	  (lambda ()
	    (prettier-js-mode)))

;;;; Custom setting into own file

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
					; (customize-save-customized)

