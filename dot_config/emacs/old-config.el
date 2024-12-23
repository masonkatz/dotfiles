;; emacs.el --- top level config -*- eval: (outshine-cycle-buffer 2) -*-
;;; Commentary:
;;
;; Portable across MacOS and Linux, but assumes a fairly recent version of EMACS.
;;
;;; Code:
;;;; Bootstrap straight.el and use-package


(when (< emacs-major-version 27)
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; bootstrap for straight package manager
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
(straight-use-package 'org)

;;;; Startup


(use-package server
  :config
  (when (not (server-running-p))
    (server-start)))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "ORGANIZATION"))
  :config
  (when (display-graphic-p)
    (exec-path-from-shell-initialize)))


(when (display-graphic-p)
  (setq default-directory "~/"))

(when (memq window-system '(mac ns))
  (unbind-key "s-t")
  (unbind-key "s-,"))

;;;; Dashboard

(use-package dashboard
  :straight t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-init-info nil
	dashboard-set-footer nil
	dashboard-banner-logo-title nil
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	dashboard-set-navigator t
	dashboard-items '((projects . 5)
			  (recents  . 5)
                          (bookmarks . 5)
                          (registers . 5))))


;;;; Font, Icons & Ligatures

(use-package nerd-icons
  :straight t)

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

(defun mjk/font-size ()
  "Return font size to use based on resolution."
  (let* ((geometry (cdr (assoc 'geometry (car (display-monitor-attributes-list)))))
	 (resolution (cddr geometry)))
    (* 10 (cdr (assoc (cddr geometry) mjk/resolution-font-size-alist)))))

(defun mjk/window-config ()
  "Set the frame defaults, font, font size, ..."
  (when (display-graphic-p)
    (window-divider-mode)
    (when (string= system-type 'darwin)
      (when (find-font (font-spec :name "JetBrains Mono"))
	(set-face-attribute 'default nil :family "JetBrains Mono")
	(global-ligature-mode t))
      (set-face-attribute 'default nil :height (mjk/font-size))
      (set-fontset-font "fontset-default" 'hebrew
			(font-spec :family "Arial Hebrew" :size (* .12 (mjk/font-size)))))))

(use-package ligature
  :straight
  (ligatures :type git :host github :repo "mickeynp/ligature.el")  
  :config
  (ligature-set-ligatures 'prog-mode '("++" "--"
				       ">=" "<="
				       "+=" "-=" "/=" "*=" "|=" "~=" "^="
				       ":=" "!=" "==" "==="
				       "/*" "*/" "//"
				       "::" "<<" ">>"
				       "<-" "->"
				       "||" "&&"
				       "...")))


(add-hook 'window-setup-hook
	  (lambda ()
	    (mjk/window-config)))


;;;; Modeline

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-project-detection 'projectile
	doom-modeline-indent-info t
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 20
        doom-modeline-hud t)
  (size-indication-mode))

;;;; Dark/Light Modes

(defvar mjk/dark-mode-p nil
  "Are we dark or light.")

(defun mjk/dark-mode ()
  "Toggle dark/light mode."
  (interactive)
  (if (string= mjk/dark-mode-p t)
      (mjk/force-light-mode)
    (mjk/force-dark-mode)))

(defun mjk/force-dark-mode ()
  "Force dark mode."
  (when (not (string= mjk/dark-mode-p t))
    (disable-theme 'whiteboard)
    (load-theme 'zenburn t)
    (zenburn-with-color-variables
      (custom-theme-set-faces
       'zenburn
       `(aw-leading-char-face ((t (:foreground ,zenburn-blue+1 :height 4.0))))
       `(fill-column-indicator ((t :foreground ,zenburn-bg+1)))
       `(font-lock-keyword-face ((t (:foreground ,zenburn-yellow :slant italic))))
       `(highlight ((t (:background ,zenburn-bg+1))))
       `(region ((,class (:background ,zenburn-bg+3 :extend t))
		 (t :inverse-video t)))))
    (setq mjk/dark-mode-p t)))

(defun mjk/force-light-mode ()
  "Force light mode."
  (when (string= mjk/dark-mode-p t)
    (disable-theme 'zenburn)
    (load-theme 'whiteboard t)
    (setq mjk/dark-mode-p nil)))

(global-set-key (kbd "C-c d")  'mjk/dark-mode)
(setq custom--inhibit-theme-enable nil)

(use-package zenburn-theme
  :straight t
  :config
  (setq zenburn-use-variable-pitch t
	zenburn-scale-org-headlines t
	zenburn-scale-outline-headlines t))


(mjk/dark-mode)

;;;; Printing

(defun mjk/print-landscape ()
  "Landscape print current buffer."
  (interactive)
  (let ((ps-font-size '(8 . 10))
	(ps-line-number t)
	(ps-landscape-mode t)
	(ps-number-of-columns 1)
	(pretty prettify-symbols-mode)
	(dark mjk/dark-mode-p))
    (when pretty (prettify-symbols-mode))
    (mjk/force-light-mode)
    (ps-print-buffer-with-faces)
    (when dark (mjk/force-dark-mode))
    (when pretty (prettify-symbols-mode))))

;;;; Org

(defun mjk/org-capture-cond (s)
  "Used by org-capture-templates to only include a line for S if non nil."
  (if (= 0 (length s))
      nil
    "  %s\n" s))

(use-package outshine
  :straight t
  :bind (:map outshine-mode-map ("<tab>" . outshine-kbd-TAB))
  :hook
  (prog-mode . outshine-mode))

(use-package ob-go
  :straight t)

(use-package org
  :bind
  (:map global-map
	("C-c a" . org-agenda)
	("C-c c" . org-capture))
  :custom
  (org-return-follows-link t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((go . t)
     (python . t)
     (shell . t)))
  (when (string= system-type 'darwin)	; has icloud
    (setq org-directory "~/Documents/Org"))
  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "Tasks.org" "Backlog")
           "* TODO %?\n%(mjk/org-capture-cond \"%i\")  %u"))))

(use-package org-superstar
  :straight t
  :after (org)
  :hook
  (org-mode . (lambda ()
		(org-superstar-mode 1)
		(push '("[ ]" . "☐") prettify-symbols-alist)
		(push '("[X]" . "☑") prettify-symbols-alist)
		(push '("[-]" . "❍") prettify-symbols-alist)
		(prettify-symbols-mode))))

(use-package org-projectile
  :straight t
  :after (org)
  :config
  (if (string= system-type 'darwin)	; has icloud
      (setq org-projectile-projects-file "~/Documents/Org/Projects.org")
    (org-projectile-per-project))
  (setq org-projectile-capture-template "* TODO %?\n%(mjk/org-capture-cond \"%i\")  %u\n  %a"
	org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates))

(use-package org-roam
  :straight t
  :after (org)
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Roam")
  (org-roam-dailies-directory "Journal")
  :bind
  (("C-c r l" . org-roam-buffer-toggle)
   ("C-c r f" . org-roam-node-find)
   ("C-c r i" . org-roam-node-insert)
   ("C-c r c" . org-roam-capture))
  :bind-keymap
  ("C-c d" . org-roam-dailies-map)  
  :config
  (add-to-list 'load-path "~/.emacs.d/straight/repos/org-roam/extensions")
  (require 'org-roam-dailies) 
  (org-roam-db-autosync-mode))



;;;; Which Key

(use-package which-key
  :straight t
  :init
  (which-key-mode))

;;;; Vertico

(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode))

(use-package vertico-prescient
  :straight t
  :after vertico
  :config
  (vertico-prescient-mode))

(use-package marginalia
  :straight t
  :after vertico
  :init
  (when (> emacs-major-version 26)
    (marginalia-mode)))

(use-package consult
  :straight t
  :config
  (global-set-key (kbd "C-c f") 'consult-line))

;;;; God

(use-package god-mode
  :straight t)

(define-key esc-map (kbd "<escape>") 'god-local-mode)

;;;; Projectile

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c P" . projectile-command-map))

(defun mjk/compile (arg)
  (interactive "P")
  (if (projectile-project-root)
      (projectile-compile-project arg)
    (compile arg)))

(defun mjk/grep (arg)
  (interactive "P")
  (if (projectile-project-root)
      (projectile-grep arg)
    (grep arg)))

(winner-mode)
(global-set-key (kbd "C-c u")  'winner-undo)
;; (global-set-key (kbd "C-c c")  'mjk/compile)
;; (global-set-key (kbd "C-c g")  'mjk/grep)



;;;; Company

(use-package company
  :straight t
  :bind (:map company-active-map
              ("C-n" . company-select-next-or-abort)
              ("C-p" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0.5)
  (global-company-mode t))

(use-package company-quickhelp
  :straight t
  :after (company)
  :config
  (setq company-quickhelp-delay 2))

(use-package company-box
  :straight t
  :hook
  (company-mode . company-box-mode))

;;;; Treemacs

(use-package treemacs
  :straight t
  :bind
  (:map global-map
        ("C-c t" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package treemacs-nerd-icons
  :straight t
  :after (treemacs)
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-dired
  :straight t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; (use-package treemacs-icons-dired
;;   :straight t
;;   :after (treemacs dired)
;;   :config
;;   (treemacs-icons-dired-mode))

(use-package treemacs-projectile
  :straight t
  :after (treemacs projectile))

(use-package treemacs-magit
  :straight t
  :after (treemacs magit))

;;;; Fix Tab

(setq tab-always-indent 't)

;;;; System Administration
;;;;; Debian

(use-package debian-el
  :straight t)


;;;;; Docker

(use-package docker
  :straight t)

(use-package dockerfile-mode
  :straight t)

;;;;; Terminal

(use-package tramp-term
  :straight t)

(use-package term
  :demand t				; force load to get the keymap
  :bind (:map term-raw-map
	      ("C-c" . nil)
	      ("M-x" . nil)
	      ("C-y" . term-paste)))

(defun mjk/ansi-term ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (ansi-term (getenv "SHELL"))
    (switch-to-buffer "*ansi-term*")))

(global-set-key (kbd "C-c s")  'mjk/ansi-term)

;;;; Writing

;;;;; Markdown

(use-package markdown-mode
  :straight t
  :custom
  (markdown-header-scaling t))

;;;; Programming
;;;;; Copilot

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)

;; (with-eval-after-load 'company
;;   ;; disable inline previews
;;   (delq 'company-preview-if-just-one-frontend company-frontends))

(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; (with-eval-after-load 'company
;;   (add-to-list 'company-backends 'copilot-company-backend))

;;;;; Git

(use-package git-gutter-fringe
  :straight t)

(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)))

(use-package magit-gitflow
  :straight t
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow))


;;;;; LSP / Flycheck

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :straight t
  :custom
  (lsp-enable-links nil))


(use-package lsp-ui
  :straight t
  :after (lsp-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-delay 0.5
	lsp-ui-doc-delay 0.5
	lsp-ui-doc-enable t
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-header nil
	lsp-ui-doc-use-webkit nil
	lsp-ui-doc-include-signature nil
	lsp-ui-doc-position 'top))

(when (string= system-type 'darwin)
  (use-package lsp-sourcekit
    :straight t
    :after (lsp-mode)
    :config
    (setq lsp-sourcekit-executable "/Library/Developer/CommandLineTools/usr/bin/sourcekit-lsp")))

;;;;; Eglot / Treesitter

(when (>= emacs-major-version 29)
  (add-to-list 'major-mode-remap-alist
	       '((c++-mode . c++-ts-mode)
		 (c-mode . c-ts-mode)
		 (c-or-c++-mode . c-or-c++-ts-mode)
		 (go-dot-mod-mode . go-mod-ts-mode)
		 (go-mode . go-ts-mode)
		 (python-mode . python-ts-mode)
		 (sh-mode . bash-ts-mode)
		 (yaml-mode . yaml-ts-mode)))
  (setq treesit-language-source-alist	; https://tree-sitter.github.io/tree-sitter/
	'((bash "https://github.com/tree-sitter/tree-sitter-bash")
	  (c "https://github.com/tree-sitter/tree-sitter-c")
	  (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	  (css "https://github.com/tree-sitter/tree-sitter-css")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	  (dot "https://github.com/rydesun/tree-sitter-dot")
	  (elisp "https://github.com/Wilfred/tree-sitter-elisp")
	  (gitcommit "https://github.com/gbprod/tree-sitter-gitcommit")
	  (gitignore "https://github.com/shunsambongi/tree-sitter-gitignore")
	  (go "https://github.com/tree-sitter/tree-sitter-go")
	  (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
	  (go-work "https://github.com/omertuc/tree-sitter-go-work")
	  (hcl "https://github.com/MichaHoffmann/tree-sitter-hcl")
	  (html "https://github.com/tree-sitter/tree-sitter-html")
	  (java "https://github.com/tree-sitter/tree-sitter-java")
	  (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
	  (json "https://github.com/tree-sitter/tree-sitter-json")
	  (markdown "https://github.com/ikatyang/tree-sitter-markdown")
	  (org "https://github.com/milisims/tree-sitter-org")
	  (protobuf "https://github.com/mitchellh/tree-sitter-proto")
	  (python "https://github.com/tree-sitter/tree-sitter-python")
	  (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	  (rust "https://github.com/tree-sitter/tree-sitter-rust")
	  (sql . ("https://github.com/derekstride/tree-sitter-sql" "gh-pages"))
	  (sqlite "https://github.com/dhcmrlchtdj/tree-sitter-sqlite")
	  (swift "https://github.com/tree-sitter/tree-sitter-swift")
	  (toml "https://github.com/tree-sitter/tree-sitter-toml")
	  (typescript "https://github.com/tree-sitter/tree-sitter-typescript")
	  (yaml "https://github.com/ikatyang/tree-sitter-yaml")
	  (zig "https://github.com/maxxnino/tree-sitter-zig")))
  (setq-default eglot-workspace-configuration
		'((:gopls .
                          ((local . "git.softiron.com,github.com/endobit")
                           (staticcheck . t))))))  

;;;;; Modes
;;;;;; Common

(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (when (display-graphic-p)
	      (setq linum-format "%5d "))
	    (smartparens-mode)
	    (show-smartparens-mode)
	    (company-quickhelp-mode)
	    (font-lock-mode)
	    (flyspell-prog-mode)
	    (copilot-mode)))

;;;;;; APL

(use-package gnu-apl-mode
  :straight t
  :hook
  ((gnu-apl-interactive-mode-hook . mjk/gnu-apl-init)
   (gnu-apl-mode-hook . mjk/gnu-apl-init)))

(defun mjk/gnu-apl-init ()
  (setq buffer-face-mode-face 'gnu-apl-default)
  (buffer-face-mode))


;;;;;; ELisp

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (prettify-symbols-mode)
	    (rainbow-mode)))

;;;;;; C/C++

(use-package cc-mode
  :config
  (setq c-default-style "linux")
  :hook
  (c-mode . (lambda ()
	      (setq c-basic-offset 4
		    tab-width 4)
	      (lsp-deferred)
	      (flycheck-add-next-checker 'lsp '(warning . c/c++-cppcheck)))))

(when (>= emacs-major-version 29)
  (require 'c-ts-mode)
  (add-hook 'c-ts-base-mode-hook
	    (lambda ()
	      (setq c-ts-mode-indent-style 'linux
		    c-ts-mode-indent-offset 8)
	      (c-ts-mode-toggle-comment-style -1)
	      (eglot-ensure))))



;;;;;; Go

(defvar mjk/organization (getenv "ORGANIZATION"))

(defconst mjk/goimports-alist '(("softiron" . "git.softiron.com")
				("endobit"  . "github.com/endobit"))
  "Mapping of ORGANIZATION to git repository base path.")

(defun mjk/goimports-local ()
  "Return the string used to set lsp-go-goimports-local."
  (interactive)
  (cdr (assoc mjk/organization mjk/goimports-alist)))

(use-package go-mode
  :straight t
  :after (lsp-mode)
  :custom
  (lsp-go-hover-kind "FullDocumentation")
  (web-mode-enable-auto-pairing nil)
  :hook
  (go-mode . (lambda ()
	       (setq lsp-go-goimports-local (mjk/goimports-local)
		     indent-tabs-mode 't
		     tab-width 4)
	       (lsp-deferred)
	       (flycheck-add-next-checker 'lsp '(warning . go-golint))
	       (add-hook 'before-save-hook
                         (lambda ()
                             (lsp-organize-imports)
                             (lsp-format-buffer))
                         nil
                         'local))))

(when (>= emacs-major-version 29)
  (add-hook 'go-ts-mode-hook
	    (lambda ()
	      (mjk/go-setup)
	      (setq go-ts-mode-indent-offset 4)
	      (eglot-ensure)
	      (add-hook 'before-save-hook
			(lambda ()
			  (eglot-format-buffer))))))


;;;;;; HCL

(use-package hcl-mode
  :straight t)

;;;;;; Javascript

(use-package web-mode
  :straight t
  :hook
  (web-mode . (lambda ()
		(turn-off-smartparens-mode)
		(when (equal web-mode-content-type "javascript")
		  (web-mode-set-content-type "jsx")) ;; react
		(setq web-mode-css-indent-offset 2)))
  :custom
  (web-mode-enable-auto-indentation nil)
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-engines-alist '(("go" . "\\.tmpl\\'"))))

(use-package prettier-js
  :straight t
  :hook
  ((js-mode   . prettier-js-mode)
   (json-mode . prettier-js-mode)
   (yaml-mode . prettier-js-mode)))

;;;;;; Make

(add-to-list 'auto-mode-alist '("[Mm]akefile\\'" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-gmake-mode))

;;;;;; Python

(use-package jedi
  :straight t)

(use-package python-black
  :straight t
  :after python)

(use-package python-mode
  :straight t
  :custom
  (py-empty-line-closes-p t)
  (py-use-font-lock-doc-face-p t)
  (py-auto-complete-p t)
  (py-tab-shifts-region-p t)
  (py-python-command "python3")
  :hook ((python-mode . (lambda ()
			  (python-black-on-save-mode)))))

;;;;;; Protobuf

(use-package protobuf-mode
  :straight t
  :hook
  (protobuf-mode . (lambda ()
		     (setq c-basic-offset 2
			   indent-tabs-mode nil))))
;;;;;; Ruby

(add-hook 'ruby-mode-hook
	  (lambda ()
	    (lsp-deferred)))

;;;;;; Salt

(use-package salt-mode
  :straight t
  :config
  (add-hook 'salt-mode-hook
            (lambda ()
              (flyspell-mode 1))))

;;;;;; Shell

(add-hook 'sh-mode-hook
	  (lambda ()
	    (setq sh-basic-offset 8
		  sh-indentation  8
		  sh-indent-for-case-label 0
		  sh-indent-for-case-alt '+)))

;;;;;; Tcl

(add-hook 'tcl-mode-hook
          (lambda ()
            (setq tab-width 4)))

;;;;;; Toml

(add-hook 'conf-toml-mode-hook
	  (lambda ()
	    (setq tab-width 4)))

;;;;;; Yaml

(use-package yaml-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  :hook (yaml-mode . (lambda ()
		       (lsp-deferred))))

;;;; Misc

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
	  ("XXX"        error bold)
          ("FIXME"      error bold)
          ("WARNING"    font-lock-constant-face bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(use-package mwim
  :straight t
  :config
  (global-set-key (kbd "C-a") 'mwim-beginning)
  (global-set-key (kbd "C-e") 'mwim-end))

(global-set-key (kbd "C-c l")  'sort-lines)
(global-set-key (kbd "C-c n")  'next-error)
(global-set-key (kbd "C-c p")  'previous-error)

(use-package dimmer
  :straight t
  :custom
  (dimmer-fraction 0.1)
  :config
  (dimmer-mode)
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-company-box))

(use-package ace-window
  :straight t
  :config
  (global-set-key [remap other-window] 'ace-window)
  (global-set-key [remap delete-window] 'ace-delete-window)
  (global-set-key (kbd "C-c o") 'ace-swap-window))


(use-package pomidor
  :straight t
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil))


(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(put 'downcase-region 'disabled nil)

;;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(calendar-hebrew-all-holidays-flag t)
 '(calendar-mark-holidays-flag t)
 '(calendar-week-start-day 1)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output t)
 '(compilation-window-height 12)
 '(display-time-load-average-threshold 10)
 '(display-time-mail-string "")
 '(global-auto-revert-mode t)
 '(hi-lock-file-patterns-range 1000000)
 '(inhibit-startup-screen t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (describe . 1)
      (it . 1)
      (org-element-map . defun)
      (org-roam-with-temp-buffer . 1)
      (org-with-point-at . 1)
      (magit-insert-section . defun)
      (magit-section-case . 0)
      (->> . 1)
      (org-roam-with-file . 2))
     (elisp-lint-ignored-validators "byte-compile" "package-lint")
     (hl-sexp-mode)
     (rainbow-mode . t)
     (eval when
	   (require 'rainbow-mode nil t)
	   (rainbow-mode 1))
     (eval font-lock-add-keywords nil
	   `((,(concat "("
		       (regexp-opt
			'("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
			t)
		       "\\_>")
	      1 'font-lock-variable-name-face)))
     (checkdoc-minor-mode . t)
     (checkdoc-package-keywords-flag)
     (eval outshine-cycle-buffer 2)))
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((use-package) (org)))
 '(world-clock-list
   '(("America/Los_Angeles" "San Diego")
     ("America/Phoenix" "Tucson")
     ("America/Denver" "Santa Fe")
     ("America/Chicago" "Dallas")
     ("America/New_York" "New York")
     ("Etc/UTC" "UTC")
     ("Europe/Berlin" "Berlin")
     ("Europe/Moscow" "Moscow")
     ("Asia/Jerusalem" "Jerusalem")
     ("Asia/Calcutta" "Bangalore")
     ("Australia/Perth" "Perth")
     ("Asia/Tokyo" "Tokyo")))
 '(world-clock-time-format "%a %b %d%t%I:%M %p%t%Z"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:height 0.75))))
 '(linum ((t (:height 0.75))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :underline t :weight bold)))))
