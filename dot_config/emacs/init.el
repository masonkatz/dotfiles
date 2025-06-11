;; emacs.el --- top level config -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Portable across MacOS and Linux, but assumes a fairly recent
;; version of EMACS.
;;
;;; Code:

;;;; Functions

(defun my--install (package)
  "Installs package if not already installed"
  (unless (package-installed-p package)
    (package-install package)))

(defun my--font-size ()
  "Return font size to use based on resolution."
  (let* ((geometry
          (cdr (assoc 'geometry (car (display-monitor-attributes-list)))))
         (resolution (cddr geometry)))
    (* 10 (cdr (assoc (cddr geometry) my--resolution-font-size-alist)))))

(defun my--find-executable-in-paths (executable paths)
  "Search for EXECUTABLE in the given list of PATHS. Returns the
full path to the executable if found, or nil otherwise."
  (let ((found nil))
    (dolist (path paths found)
      (let ((candidate (expand-file-name executable path)))
        (when (and (file-exists-p candidate) (file-executable-p candidate))
          (setq found candidate))))))

(defun my--add-to-list-multiple (list &rest items)
  "Add multiple ITEMS to LIST."
  (dolist (item items)
    (add-to-list list item)))

(defun my--graphic-setup ()
  "Common graphic mode hook."
  (git-gutter-mode)
  (hl-line-mode)
  (display-fill-column-indicator-mode)
  (display-line-numbers-mode))

;;;; Startup

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'chezmoi-vars)

(require 'package)
(require 'server)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (package-refresh-contents)

(unless (server-running-p)
  (server-start))

(my--install 'exec-path-from-shell)
(setopt exec-path-from-shell-variables '("PATH" "MANPATH"))

(when (or (daemonp) (display-graphic-p))
  (setq default-directory "~/")
  (exec-path-from-shell-initialize))

(cond ((eq window-system 'ns)
       (unbind-key "C-z") ; disable suspending the frame
       (unbind-key "s-t") ; disable font menu
       (unbind-key "s-,")) ; disable preferences menu
      ((eq window-system 'x)
       (setq
	x-alt-keysym 'meta ; replicate MacOS behavior
	x-meta-keysym 'super)
       (global-set-key (kbd "s-v") 'yank))
      ((not window-system)
       (xterm-mouse-mode 1))) ; mouse click moves cursor

(let ((ispell
       (my--find-executable-in-paths
        "ispell" '("/usr/local/bin" "/opt/homebrew/bin" "/usr/bin"))))
  (when ispell
    (setq ispell-program-name ispell)))

;;;; Spacing

(setopt tab-width 4
	sentence-end-double-space nil
	fill-column 80
	display-fill-column-indicator-column 100
	x-stretch-cursor t)

;;;; Graphic Settings

(defvar my--graphic-initialized nil
  "Flag to indicate if the graphic mode has been initialized.")

(defvar my--resolution-font-size-alist
  '(((1280 800) . 14)
    ((1440 900) . 14)
    ((1470 956) . 14)
    ((1512 982) . 14)
    ((1680 1050) . 14)
    ((1920 1080) . 14)
    ((2560 1440) . 16)
    ((3008 1692) . 16)
    ((3360 1890) . 16)
    ((3440 1440) . 16)
    ((3840 2160) . 20))
  "Font sizes for different monitors.")

(defun my--window-config ()
  "Set the frame defaults, font, font size, ...
Will only change settings once, so it is safe to run as a frame creation
hook"
  (when (and (display-graphic-p) (not my--graphic-initialized))
    (window-divider-mode)
    (setopt treemacs-is-never-other-window t)
    (my--install 'ligature)
    (my--install 'nerd-icons) ; (nerd-icons-install-fonts)
    (my--install 'nerd-icons-dired)
    (my--install 'treemacs-nerd-icons)
    (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
    (ligature-set-ligatures 'prog-mode
			    '("++" "--"
			      ">=" "<="
			      "+=" "-=" "/=" "*="
			      "|=" "~=" "^="
			      ":=" "!=" "==" "==="
			      "/*" "*/" "//"
			      "::"
			      "<<" ">>"
			      "<<<" ">>>"
			      "<-" "->"
			      "||" "&&"
			      "..."))
    (cond ((eq system-type 'darwin) ; MacOS
	   (set-face-attribute 'aw-leading-char-face nil :height 4.0)
	   (set-face-attribute 'default nil :height (my--font-size))
	   (when (find-font (font-spec :name "JetBrains Mono"))
             (set-face-attribute 'default nil :family "JetBrains Mono")
             (global-ligature-mode t))
	   (when (find-font (font-spec :name "Arial Hebrew"))
             (set-fontset-font
              "fontset-default" 'hebrew
              (font-spec :family "Arial Hebrew" :size (* .12 (my--font-size))))))
	  ((eq window-system 'x) ; X11
	   (set-face-attribute 'default nil :height 135)))) ; close to what 160 means on MacOS
  (setq my--graphic-initialized t))

(if (daemonp)
    (add-hook 'server-after-make-frame-hook 'my--window-config)
  (add-hook 'window-setup-hook 'my--window-config))


;;;; Tramp

(require 'tramp)

(setopt tramp-default-method "ssh")

(when (eq system-type 'darwin)
  (add-to-list 'tramp-remote-path "/opt/homebrew/bin")
  (add-to-list 'tramp-remote-path "/opt/homebrew/sbin"))
(add-to-list 'tramp-remote-path 'tramp-own-remote-path) ; preserves remote's PATH


;;;; Time & Calendar

(require 'calendar)

(setopt calendar-hebrew-all-holidays-flag t
	calendar-latitude 32.7157
	calendar-longitude -117.1611
	calendar-mark-holidays-flag t
	calendar-location-name "San Diego"
	calendar-week-start-day 1
	calendar-holidays (append holiday-general-holidays
				  holiday-local-holidays
				  holiday-other-holidays
				  holiday-christian-holidays
				  holiday-hebrew-holidays
				  holiday-solar-holidays))

(setopt world-clock-list
	'(("America/Los_Angeles" "San Diego")
	  ("America/Phoenix" "Tucson")
	  ("America/Denver" "Santa Fe")
	  ("Etc/UTC" "UTC")
	  ("Asia/Jerusalem" "Jerusalem")
	  ("Asia/Tokyo" "Kobe"))
	world-clock-time-format "%a %b %d%t%I:%M %p%t%Z")

;;;; Dashboard

(my--install 'dashboard)
(dashboard-setup-startup-hook)

(setopt dashboard-banner-logo-title ""
	dashboard-items '((projects . 5) (recents . 5) (bookmarks . 5) (registers . 5))
	dashboard-projects-backend 'project-el
	dashboard-set-file-icons t
	dashboard-set-footer nil
	dashboard-set-heading-icons t
	dashboard-set-init-info nil
	dashboard-set-navigator t)

;;;; Ag (Silver Searcher)
(my--install 'ag)

;;;; Ace Windows
(my--install 'ace-window)
(require 'ace-window) ; so we can modify the faces

(global-set-key [remap other-window] 'ace-window)
(global-set-key [remap delete-window] 'ace-delete-window)


;;;; Which Key

(which-key-mode)

;;;; Treemacs

(my--install 'treemacs)

(global-set-key (kbd "C-c t") 'treemacs-select-window)

;;;; Modeline

(my--install 'doom-modeline)

(doom-modeline-mode 1)
(size-indication-mode)
(column-number-mode)

(setopt doom-modeline-column-zero-based nil
	doom-modeline-buffer-encoding t
	doom-modeline-hud t
	doom-modeline-indent-info t
	doom-modeline-project-name t
	doom-modeline-vcs-max-length 20
	display-time-load-average-threshold 10
	display-time-mail-string ""
	project-mode-line t)

;;;; Ibuffer

(my--install 'nerd-icons-ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setopt ibuffer-show-empty-filter-groups nil
	ibuffer-saved-filter-groups '(("default"
				       ("Lisp" (or (mode . emacs-lisp-mode) (mode . lisp-mode)))
				       ("Dired" (mode . dired-mode))
				       ("Org" (mode . org-mode))
				       ("Python" (mode . python-mode))
				       ("Go" (mode . go-ts-mode))
				       ("C/C++" (or (mode . c-ts-mode) (mode . c++-ts-mode)))
				       ("Shell" (or (mode . sh-mode) (mode . shell-script-mode)))
				       ("Web" (or (mode . web-mode) (mode . js-ts-mode)))
				       ("Markdown" (mode . markdown-mode))
				       ("Text" (mode . text-mode))
				       ("Other" (name . "^\\*")))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (when (display-graphic-p)
	      (hl-line-mode))
	    (nerd-icons-ibuffer-mode)
	    (ibuffer-switch-to-saved-filter-groups "default")))

;;;; Zenburn

(my--install 'zenburn-theme)
(load-theme 'zenburn t)

(setq zenburn-scale-org-headlines t
      zenburn-scale-outline-headlines t
      zenburn-use-variable-pitch t)

;;;; Misc

(my--install 'rainbow-mode) ; highlights color strings

(my--install 'hl-todo)
(setopt hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("DEPRECATED" font-lock-doc-face bold)
	  ("FIXME" error bold)
	  ("NOTE" success bold)
	  ("REVIEW" font-lock-keyword-face bold)
	  ("TODO" warning bold)
	  ("WARN" font-lock-constant-face bold)
	  ("WARNING" font-lock-constant-face bold)
	  ("XXX" error bold)))

;;;; EditorConfig

(require 'editorconfig)
(editorconfig-mode)

;;;; Terminal

(my--install 'eat)

(add-hook 'eshell-load-hook #'eat-eshell-mode)
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

;;;; SSH

(my--install 'ssh-config-mode)

;;;; Git

(my--install 'git-gutter-fringe)
(my--install 'magit)
(my--install 'git-modes)

;;;; Company

(my--install 'company)
(my--install 'company-box)

(setopt company-idle-delay 0.5)

(global-company-mode t)
(add-hook 'company-mode-hook 'company-box-mode)

;;;; Yasnippet

(my--install 'yasnippet)
(my--install 'yasnippet-snippets)

;;;; Prettier

(my--install 'prettier-js) ; add prettier-js-mode to mode hooks

;;;; Page Breaks

(my--install 'page-break-lines)


;;;; Treesitter

(require 'treesit)

(setopt treesit-font-lock-level 3)

(defun my--install-treesitter-grammars ()
  "Download and install treesitter grammar files."
  (interactive)
  (dolist (grammar
           '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
             (c . ("https://github.com/tree-sitter/tree-sitter-c"))
             (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
             (css . ("https://github.com/tree-sitter/tree-sitter-css"))
             (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
             (go . ("https://github.com/tree-sitter/tree-sitter-go"))
             (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))
             (gowork . ("https://github.com/omertuc/tree-sitter-go-work"))
             (html . ("https://github.com/tree-sitter/tree-sitter-html"))
             (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
             (json . ("https://github.com/tree-sitter/tree-sitter-json"))
             (proto . ("https://github.com/mitchellh/tree-sitter-proto"))
             (python . ("https://github.com/tree-sitter/tree-sitter-python"))
             (toml . ("https://github.com/ikatyang/tree-sitter-toml"))
             (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
    (add-to-list 'treesit-language-source-alist grammar)
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

(my--install-treesitter-grammars)

;;;; Org Mode

(require 'org)
(my--install 'org-superstar)

(add-hook 'org-mode-hook
	  (lambda ()
	    (visual-line-mode t) ; wrap lines (needed for copilot-chat)
	    (org-superstar-mode 1)
	    (push '("[ ]" . "☐") prettify-symbols-alist)
	    (push '("[X]" . "☑") prettify-symbols-alist)
	    (push '("[-]" . "❍") prettify-symbols-alist)
	    (prettify-symbols-mode)))

;;;; Eglot

(require 'eglot)
(require 'flymake) ; eglot will enable flymake-mode

(when (eq system-type 'darwin)
  (add-to-list
   'eglot-server-programs '(swift-mode . ("xcrun" "sourcekit-lsp"))))


(my--install 'eldoc-box)
(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)

(setopt eldoc-box-only-multi-line t
	eldoc-box-max-pixel-height (* 8 (frame-char-height)))

(define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c o") 'eglot-code-action-organize-imports)

(setq-default eglot-workspace-configuration
	      `((:clangd . ((inlayHints . ((parameterName . t)
					   (defaultArguments . t)))))
		(:gopls . ((local . ,chezmoi-vars-golocal)
			   (hoverKind . "FullDocumentation")
			   (staticcheck . t)
			   (hints . ((compositeLiteralFields . t)
				     (compositeLiteralTypes . t)
				     (constantValues . t)
				     (functionTypeParameters . t)
				     (parameterNames . t)
				     (rangeVariableTypes . t)))))))


;;;; Copilot / GPTel

(use-package copilot
  :vc
  (:url
   "https://github.com/masonkatz/copilot.el.git"
   :rev
   :newest
   :branch "main"))

;; using gptel instead
;;(my--install 'copilot-chat)

(my--add-to-list-multiple 'copilot-indentation-alist
                          '((protobuf-ts-mode 2) (sql-mode 8)))

(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "<backtab>") 'copilot-accept-completion-by-line)
(define-key copilot-completion-map (kbd "C-RET") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-g") 'copilot-clear-overlay)
(define-key copilot-completion-map (kbd "C-n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "C-p") 'copilot-previous-completion)

(my--install 'gptel)

(gptel-make-gh-copilot "Copilot")

;;;; Text Modes

(defun my--text-mode-hook ()
  "Common text mode hook."
  (flyspell-mode)
  (when (display-graphic-p)
    (my--graphic-setup)))

(add-hook 'text-mode-hook 'my--text-mode-hook)

;;;;; Markdown

(my--install 'markdown-mode)
(setopt markdown-header-scaling t)

(defun my--markdown-mode-hook ()
  "My markdown mode hook."
  (eglot-ensure)		; brew install marksman
  (prettier-js-mode))

(add-hook 'markdown-mode-hook 'my--markdown-mode-hook)

;;;; Programming Modes

(setopt compilation-always-kill t
	compilation-ask-about-save nil
	compilation-auto-jump-to-first-error t
	compilation-scroll-output t
	compilation-window-height 12)


(define-key prog-mode-map (kbd "C-c n") 'next-error)
(define-key prog-mode-map (kbd "C-c p") 'previous-error)

(setopt major-mode-remap-alist '((c++-mode . c++-ts-mode)
				 (c-mode . c-ts-mode)
				 (c-or-c++-mode . c-or-c++-ts-mode)
				 (js-json-mode . json-ts-mode)
				 (python-mode . python-ts-mode)
				 (yaml-mode . yaml-ts-mode)))

(defun my--prog-mode-hook ()
  "Common programming mode hook."
  (flyspell-prog-mode)
  (winner-mode)
  (electric-pair-mode)
  (hl-todo-mode)
  (yas-minor-mode)
  (page-break-lines-mode)
  (when (display-graphic-p)
    (my--graphic-setup)))

(add-hook 'prog-mode-hook 'my--prog-mode-hook)

;;;;; C/C++

(require 'c-ts-mode)

(defun my--c-base-mode-hook ()
  "Shared c/c++ mode hook."
  (copilot-mode)
  (eglot-ensure)			; clangd
  (add-hook 'before-save-hook (lambda () (eglot-format-buffer))))

(defun my--c-mode-hook ()
  "C mode hook"
  (setq-local c-ts-mode-indent-style 'k&r)
  (c-ts-mode-toggle-comment-style -1)) ; c++ style comments

(defun my--c++-mode-hook ()
  "C++ mode hook"
  (setq-local c-ts-mode-indent-style 'stroustrup))


(add-hook 'c-ts-base-mode-hook 'my--c-base-mode-hook)
(add-hook 'c-ts-mode-hook      'my--c-mode-hook)
(add-hook 'c++-ts-mode-hook    'my--c++-mode-hook)

;;;;; Docker

(my--install 'docker)
(require 'dockerfile-ts-mode)

;;;;; Go

(require 'go-ts-mode)

(defun my--go-mode-hook ()
  "Go mode hook"
  (copilot-mode)
  (eglot-ensure)			 ; gopls
  (setq-local page-delimiter "\/\*\*\/") ; use /**/ instead of ^L (syntax error in go)
  (add-hook 'before-save-hook (lambda () (eglot-format-buffer))))


(add-hook 'go-ts-mode-hook 'my-go-mode-hook)

;;;;; JSON / YAML

(require 'yaml-ts-mode)

(add-hook 'json-ts-mode-hook (lambda () (prettier-js-mode)))
(add-hook 'yaml-ts-mode-hook (lambda () (prettier-js-mode)))

;;;;; Lisp

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (indent-tabs-mode) ; This is the factory default, but
	    (setq tab-width 8) ; explicitly enforce it anyway.
	    (outline-minor-mode)
	    (setq-local	copilot-indent-offset-warning-disable t
			outline-regexp ";;;\\{1,\\} "
			outline-minor-mode-cycle t)))


;;;;; Make / Just

(my--install 'just-mode)

(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'"       . makefile-gmake-mode))

;;;;; Python

(my--install 'python-black)
(my--install 'pyvenv)

(add-hook 'python-ts-mode-hook
	  (lambda () 
	    (copilot-mode)
	    (eglot-ensure)	    ; pylsp (needs to be installed in venv)	    
	    (python-black-on-save-mode) 
	    (pyvenv-mode) 
	    (pyvenv-tracking-mode)))


;;;;; Protobuf

(my--install 'protobuf-ts-mode)
(require 'protobuf-ts-mode)

;;;;; Shell

(add-hook 'sh-mode-hook
	  (lambda ()
	    (copilot-mode)
	    (eglot-ensure)))		; brew install bash-language-server

;;;;; Swift

(my--install 'swift-mode)

;;;;; Web

(my--install 'web-mode)

(add-hook 'web-mode-hook
	  (lambda ()
	    (electric-pair-mode -1)))	; web-mode has its own electric pair functionality

(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))


;;;; Custom setting into own file

(setopt custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t)
;; (customize-save-customized)
