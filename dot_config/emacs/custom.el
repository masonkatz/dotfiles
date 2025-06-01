(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-ts-mode-indent-style 'k&r)
 '(calendar-hebrew-all-holidays-flag t)
 '(calendar-latitude 32.7157)
 '(calendar-longitude -117.1611)
 '(calendar-mark-holidays-flag t)
 '(calendar-week-start-day 1)
 '(company-idle-delay 0.5)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output t)
 '(compilation-window-height 12)
 '(dashboard-banner-logo-title nil)
 '(dashboard-items
   '((projects . 5) (recents . 5) (bookmarks . 5) (registers . 5)))
 '(dashboard-projects-backend 'project-el)
 '(dashboard-set-file-icons t)
 '(dashboard-set-footer nil)
 '(dashboard-set-heading-icons t)
 '(dashboard-set-init-info nil)
 '(dashboard-set-navigator t)
 '(display-fill-column-indicator-column 100)
 '(display-time-load-average-threshold 10)
 '(display-time-mail-string "")
 '(doom-modeline-buffer-encoding t)
 '(doom-modeline-hud t)
 '(doom-modeline-indent-info t)
 '(doom-modeline-project-name t)
 '(doom-modeline-vcs-max-length 20)
 '(eldoc-box-only-multi-line t)
 '(exec-path-from-shell-variables '("PATH" "MANPATH"))
 '(fill-column 80)
 '(global-auto-revert-mode t)
 '(hl-todo-highlight-punctuation ":")
 '(hl-todo-keyword-faces
   `(("TODO" warning bold) ("XXX" error bold) ("FIXME" error bold)
     ("WARNING" font-lock-constant-face bold)
     ("HACK" font-lock-constant-face bold)
     ("REVIEW" font-lock-keyword-face bold) ("NOTE" success bold)
     ("DEPRECATED" font-lock-doc-face bold)))
 '(inhibit-startup-screen t)
 '(major-mode-remap-alist
   '((js-json-mode . json-ts-mode) (c++-mode . c++-ts-mode) (c-mode . c-ts-mode)
     (c-or-c++-mode . c-or-c++-ts-mode) (python-mode . python-ts-mode)))
 '(make-backup-files nil)
 '(markdown-header-scaling t)
 '(menu-bar-mode nil)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(company-box copilot copilot-chat dashboard doom-modeline eat eldoc-box
		 exec-path-from-shell git-gutter-fringe git-modes hl-todo
		 just-mode ligature magit nerd-icons-dired org-superstar
		 page-break-lines prettier-js protobuf-ts-mode rainbow-mode
		 salt-mode ssh-config-mode treemacs-nerd-icons web-mode
		 yasnippet-snippets zenburn-theme))
 '(package-vc-selected-packages
   '((copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(project-mode-line t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh")
 '(tramp-syntax 'simplified nil (tramp))
 '(treemacs-is-never-other-window t)
 '(treesit-font-lock-level 4)
 '(uniquify-buffer-name-style 'post-forward nil (uniquify))
 '(world-clock-list
   '(("America/Los_Angeles" "San Diego") ("America/Phoenix" "Tucson")
     ("America/Denver" "Santa Fe") ("Etc/UTC" "UTC")
     ("Asia/Jerusalem" "Jerusalem") ("Asia/Tokyo" "Kobe")))
 '(world-clock-time-format "%a %b %d%t%I:%M %p%t%Z")
 '(zenburn-scale-org-headlines t t)
 '(zenburn-scale-outline-headlines t t)
 '(zenburn-use-variable-pitch t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:weight normal :slant italic))))
 '(line-number ((t (:height 0.75))))
 '(treemacs-root-face ((t (:inherit font-lock-constant-face :underline t :weight bold)))))
