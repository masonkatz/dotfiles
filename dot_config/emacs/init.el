;; emacs.el --- top level config
;;; Commentary:
;;
;; Portable across MacOS and Linux, but assumes a fairly recent
;; version of EMACS.
;;
;;; Code:

;;;; Functions

(defun mjk/eldoc-doc-buffer ()
  "Show and prettify the Eldoc documentation buffer in a new window,
without selecting it."
  (interactive)
  (let ((buf (eldoc-doc-buffer)))
    (with-current-buffer buf
      (visual-line-mode 1)
      (setq-local truncate-lines nil)
      (setq-local word-wrap t)
      ;; Reconstruct paragraphs where gopls collapsed newlines
      (goto-char (point-min))
      (while (re-search-forward "\\([^\n]\\)\n\\([^ \n]\\)" nil t)
        (replace-match "\\1 \\2"))
      (goto-char (point-min)))
    ;; Display buffer without selecting it
    (display-buffer buf)))

(defun mjk/install (package)
  "Installs package if not already installed"
  (unless (package-installed-p package)
    (package-install package)))

(defun mjk/graphic-p ()
  "Return true if running as a graphical application"
  (or (eq window-system 'ns) (eq system-type 'darwin)))

(defun mjk/macos-graphic-p ()
  "Return true if running as a graphical application on MacOS"
  (and (eq system-type 'darwin) (mjk/graphic-p)))

(defun mjk/font-size ()
  "Return font size to use based on resolution."
  (let* ((geometry (cdr (assoc 'geometry (car (display-monitor-attributes-list)))))
	 (resolution (cddr geometry)))
    (* 10 (cdr (assoc (cddr geometry) mjk/resolution-font-size-alist)))))

(defun mjk/window-config ()
  "Set the frame defaults, font, font size, ..."
  (when (display-graphic-p)
    (window-divider-mode)
    (when (mjk/graphic-p)
      (set-face-attribute 'aw-leading-char-face nil :height 4.0)
      (when (find-font (font-spec :name "JetBrains Mono"))
	(set-face-attribute 'default nil :family "JetBrains Mono")
	(global-ligature-mode t))
      (if (eq window-system 'x)
	  (set-face-attribute 'default nil :height 135) ; close to what 160 means on MacOS
	(set-face-attribute 'default nil :height (mjk/font-size))
	(set-fontset-font "fontset-default" 'hebrew
			  (font-spec :family "Arial Hebrew" :size (* .12 (mjk/font-size))))))))

(defun mjk/find-executable-in-paths (executable paths)
  "Search for EXECUTABLE in the given list of PATHS. Returns the
full path to the executable if found, or nil otherwise."
  (let ((found nil))
    (dolist (path paths found)
      (let ((candidate (expand-file-name executable path)))
        (when (and (file-exists-p candidate) (file-executable-p candidate))
          (setq found candidate))))))

(defun mjk/add-to-list-multiple (list &rest items)
  "Add multiple ITEMS to LIST."
  (dolist (item items)
    (add-to-list list item)))



;;;; Startup

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'chezmoi)

(require 'package)
(require 'server)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
					;(package-refresh-contents)

(unless (server-running-p)
  (setq server-use-tcp t)
  (server-start))

(mjk/install 'exec-path-from-shell)

(when (or (daemonp) (mjk/macos-graphic-p))
  (setq default-directory "~/")
  (exec-path-from-shell-initialize))

(when (mjk/macos-graphic-p)
  (unbind-key "s-t")
  (unbind-key "s-,"))

(when (not (display-graphic-p))
  (xterm-mouse-mode 1))			; mouse click moves cursor

(let ((ispell (mjk/find-executable-in-paths
	       "ispell"
	       '("/usr/local/bin" "/opt/homebrew/bin" "/usr/bin"))))
  (when ispell
    (setq ispell-program-name ispell)))

;;;; Tramp

(require 'tramp)
(add-to-list 'tramp-remote-path "/opt/homebrew/bin")
(add-to-list 'tramp-remote-path "/opt/homebrew/sbin")
(add-to-list 'tramp-remote-path 'tramp-own-remote-path) ; preserves remote's PATH 


;;;; Dashboard

(mjk/install 'dashboard)

(dashboard-setup-startup-hook)

;;;; Ace Windows

(mjk/install 'ace-window)
(require 'ace-window)			; so we can modify the faces

(global-set-key [remap other-window] 'ace-window)
(global-set-key [remap delete-window] 'ace-delete-window)


;;;; Which Key

(which-key-mode)

;;;; Treemacs, Font, Icons & Ligatures

(global-set-key (kbd "C-c t") 'treemacs-select-window)

(when (display-graphic-p)
  (mjk/install 'treemacs)
  (mjk/install 'ligature)
  (mjk/install 'nerd-icons) ; (nerd-icons-install-fonts)
  (mjk/install 'nerd-icons-dired)
  (mjk/install 'treemacs-nerd-icons)
  ;;  (treemacs-load-theme "nerd-icons")
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

;;;; EditorConfig
(require 'editorconfig)
(editorconfig-mode)

;;;; Terminal

(mjk/install 'eat)

(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;;;; SSH

(mjk/install 'ssh-config-mode)

;;;; Git

(mjk/install 'git-gutter-fringe)
(mjk/install 'magit)
(mjk/install 'git-modes)

;;;; Company

(mjk/install 'company)
(mjk/install 'company-box)
(global-company-mode t)
(add-hook 'company-mode-hook 'company-box-mode)

;;;; Yasnippet

(mjk/install 'yasnippet)
(mjk/install 'yasnippet-snippets)

;;;; Prettier

(mjk/install 'prettier-js)		; add prettier-js-mode to mode hooks

;;;; Page Breaks

(mjk/install 'page-break-lines)


;;;; Treesitter

(require 'treesit)

(defun mjk/install-treesitter-grammars ()
  "Download and install treesitter grammar files."
  (interactive)
  (dolist (grammar
	   '((c          . ("https://github.com/tree-sitter/tree-sitter-c"))
	     (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
	     (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
	     (go         . ("https://github.com/tree-sitter/tree-sitter-go" "v0.23.1"))
	     (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.2"))
	     (gowork     . ("https://github.com/omertuc/tree-sitter-go-work"))
	     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
	     (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
	     (proto      . ("https://github.com/mitchellh/tree-sitter-proto"))
	     (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
	     (toml       . ("https://github.com/ikatyang/tree-sitter-toml"))
	     (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(mjk/install-treesitter-grammars)

;;;; Org Mode

(require 'org)
(mjk/install 'org-superstar)

(add-hook 'org-mode-hook
	  (lambda ()
	    (visual-line-mode t)	; wrap lines (needed for copilot-chat)
	    (org-superstar-mode 1)
	    (push '("[ ]" . "☐") prettify-symbols-alist)
	    (push '("[X]" . "☑") prettify-symbols-alist)
	    (push '("[-]" . "❍") prettify-symbols-alist)
	    (prettify-symbols-mode)))

;;;; Eglot / Copilot

(require 'eglot)
(require 'flymake)

(mjk/install 'eldoc-box)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-h .") 'mjk/eldoc-doc-buffer)

(setq-default eglot-workspace-configuration
	      `((:gopls .
			((local . ,chezmoi-golocal)
			 (hoverKind . "FullDocumentation")
			 (staticcheck . t)))))


(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(mjk/install 'copilot-chat)

(mjk/add-to-list-multiple 'copilot-indentation-alist
			  '((protobuf-ts-mode 2)
			    (sql-mode 8)))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-<return>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
(define-key copilot-completion-map (kbd "C-p n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-p p") 'copilot-previous-completion)


;;;; Utilities
;;;;; Docker

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))

;;;; Programming Modes


(add-hook 'prog-mode-hook
	  (lambda ()
	    (winner-mode)
	    (flyspell-prog-mode)
	    (electric-pair-mode)
	    (hl-todo-mode)
	    (yas-minor-mode)
	    (page-break-lines-mode)
	    (when (display-graphic-p)
	      (git-gutter-mode)
	      (hl-line-mode)
	      (display-fill-column-indicator-mode)
	      (display-line-numbers-mode))))


;;;;; C/C++

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

;;;;; GNU Makefile

(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))

;;;;; Go

(add-to-list 'auto-mode-alist '("\\.go\\'" .  go-ts-mode))

(add-hook 'go-ts-mode-hook
	  (lambda ()
	    (copilot-mode)
	    (eglot-ensure)
	    (setq-local page-delimiter "\/\/\/\/") ; use //// instead of ^L (syntax error in go)
	    (add-hook 'before-save-hook
		      (lambda ()
			(eglot-format-buffer)))))

;;;;; Just

(mjk/install 'just-mode)

(add-hook 'just-mode-hook
	  (lambda ()
	    (setq just-indent-offset 4)))

;;;;; JSON / YAML

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(add-hook 'json-ts-mode-hook
	  (lambda ()
	    (prettier-js-mode)))

(add-hook 'yaml-ts-mode-hook
	  (lambda ()
	    (prettier-js-mode)))

;;;;; Lisp

; Lisp indents with both tabs and spaces. If we redefine tabs the file will look
; bad outside of our editor.

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (outline-minor-mode)
	    (setq-local outline-regexp ";;;\\{1,\\} ")
	    (setq-local outline-minor-mode-cycle t)
	    (outline-hide-body)
    	    (setq tab-width 8)))

;;;;; Markdown

(mjk/install 'markdown-mode)

;;;;; Python

(add-hook 'python-ts-mode
	  (lambda ()
	    (copilot-mode)
	    (eglot-ensure)
	    (flymake-mode)))

;;;;; Protobuf

(mjk/install 'protobuf-ts-mode)

(add-to-list 'auto-mode-alist '("\\.proto\\'" .  protobuf-ts-mode))

(add-hook 'protobuf-ts-mode-hook
	  (lambda ()
	    (copilot-mode)))


;;;;; SQL

(add-hook 'sql-mode-hook
	  (lambda ()
	    (setq tab-width 8)
	    (copilot-mode)))

;;;;; Web

(mjk/install 'web-mode)

(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))


;;;; Custom setting into own file

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
					; (customize-save-customized)

