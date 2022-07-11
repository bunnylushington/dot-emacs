(require 'org)
(org-babel-load-file
 (expand-file-name "dot-emacs.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe-mode '(1 . 1) nil (fringe))
 '(ido-vertical-disable-if-short t)
 '(ido-vertical-pad-list nil)
 '(indicate-buffer-boundaries 'right)
 '(magit-no-confirm '(stage-all-changes set-and-push))
 '(package-selected-packages
   '(ghub+ dap-mode go-imenu lsp-mode go-snippets ido-vertical-mode dilbert fancy-narrow git-link ido-grid-mode noaa osx-dictionary side-hustle smart-comment ssh ssh-config-mode uuidgen flx flx-ido flx-isearch vertico corfu so-long auto-complete-c-headers exec-path-from-shell reveal-in-osx-finder ztree yasnippet-snippets wttrin window-number which-key web-mode vterm-toggle use-package undo-tree treemacs-projectile term-projectile svg-tag-mode smartparens slack scss-mode salt-mode restclient-test quick-buffer-switch python-mode php-mode pg persp-projectile outline-magic org-modern org-journal open-junk-file nov multi-vterm mu4e-views messages-are-flowing md4rd markdown-preview-mode markdown-mode+ magit-todos lua-mode lsp-ui lsp-treemacs lsp-origami lsp-ivy lsp-docker keycast jinja2-mode impatient-mode imenu-list ido-hacks ido-completing-read+ hyperbole hiwin hide-lines helm-lsp haskell-mode grip-mode go-mode gitlab git-timemachine gh-md gh ggtags fontawesome flycheck filladapt emamux elm-mode elfeed-web elfeed-score elfeed-protocol elfeed-org elfeed-goodies eglot-java edts edbi-minor-mode dumb-jump dockerfile-mode docker-compose-mode docker-cli docker-api docker docean distel-completion-lib dired-rainbow dired-imenu diff-hl dash-at-point dad-joke cyberpunk-theme csv-mode csv command-log-mode com-css-sort column-enforce-mode colormaps calfw-org calfw-ical calfw bufler buffer-move buffer-flip boxquote apache-mode amx alchemist))
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :extend nil :stipple nil :background "#2E3440" :foreground "#ECEFF4" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco"))))
;;  '(bookmark-face ((t (:weight bold))))
;;  '(cfw:face-header ((t (:weight bold :foreground "unspecified"))))
;;  '(cfw:face-saturday ((t (:foreground "#888888" :weight bold))))
;;  '(cfw:face-sunday ((t (:foreground "#888888" :weight bold))))
;;  '(cfw:face-title ((t (:weight bold :height 1.8))))
;;  '(cfw:face-toolbar ((t (:background "unspecified" :inherit nil))))
;;  '(cfw:face-toolbar-button-on ((t (:weight bold :foreground "#ff8c00" :background "unspecified"))))
;;  '(erlang-font-lock-exported-function-name-face ((t (:inherit font-lock-function-name-face :slant italic))))
;;  '(fill-column-indicator ((t (:stipple nil :foreground "#555555" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight thin))))
;;  '(font-lock-variable-name-face ((t (:inherit nano-strong :foreground "SlateGray2"))))
;;  '(markdown-header-face-1 ((t nil)))
;;  '(nano-strong ((t (:foreground "SeaGreen1" :weight bold))) t)
;;  '(show-paren-match ((t (:foreground "spring green" :inherit nano-strong))))
;;  '(show-paren-mismatch ((t (:foreground "IndianRed2" :inherit nano-critical))))
;;  '(slack-message-output-header ((t (:foreground "#FFA000" :underline nil :weight normal :height 1.0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flx-highlight-face ((t (:weight bold :foreground "turquoise" :inherit font-lock-variable-name-face))))
 '(lsp-headerline-breadcrumb-path-error-face ((t (:background "red4" :inherit lsp-headerline-breadcrumb-path-face))))
 '(lsp-headerline-breadcrumb-path-face ((t (:inherit nano-face-header-default))))
 '(lsp-headerline-breadcrumb-project-prefix-face ((t (:weight bold :inherit nano-face-header-default))))
 '(lsp-headerline-breadcrumb-separator-face ((t (:height 0.8 :inherit nano-face-header-default))))
 '(lsp-headerline-breadcrumb-symbols-error-face ((t (:inherit lsp-headerline-breadcrumb-symbols-face :background "red4"))))
 '(lsp-headerline-breadcrumb-symbols-face ((t (:inherit nano-face-header-default :weight bold))))
 '(lsp-headerline-breadcrumb-unknown-project-prefix-face ((t (:weight bold :inherit nano-face-header-default)))))
