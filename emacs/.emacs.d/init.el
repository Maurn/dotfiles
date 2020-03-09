;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(winner-mode 1)
(put 'narrow-to-region 'disabled nil)
(defvar show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq vc-follow-symlinks t)
(setq fill-column 100)
(setq initial-scratch-message nil)
(global-display-line-numbers-mode 1)
(save-place-mode 1)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq-default truncate-lines t)
(setq use-dialog-box nil)

(setq help-window-select 't)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore
      visible-bell nil)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; no fucking lockfiles either

(eval-when-compile
  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa"  . "https://elpa.gnu.org/packages/")
                           ("org"   . "https://orgmode.org/elpa/"))
        package-quickstart t
        load-prefer-newer t)

  (package-initialize)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (setq-default use-package-always-ensure t
                use-package-verbose t
                use-package-compute-statistics t)
  (require 'use-package))

(use-package which-key
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0))

(use-package general
  :after which-key
  :config
  (general-override-mode 1)
  (general-evil-setup)

  (defun find-user-init-file ()
    "Edit the `user-init-file', in same window."
    (interactive)
    (find-file user-init-file))
  (defun load-user-init-file ()
    "Load the `user-init-file', in same window."
    (interactive)
    (load-file user-init-file))

  (defun last-buffer (&optional buffer visible-ok frame)
    "Return the last buffer in FRAME's buffer list.
    If BUFFER is the last buffer, return the preceding buffer
    instead.  Buffers not visible in windows are preferred to visible
    buffers, unless optional argument VISIBLE-OK is non-nil.
    Optional third argument FRAME nil or omitted means use the
    selected frame's buffer list.  If no such buffer exists, return
    the buffer `*scratch*', creating it if necessary."
    (setq frame (or frame (selected-frame)))
    (or (get-next-valid-buffer (nreverse (buffer-list frame))
                               buffer visible-ok frame)
        (get-buffer "*scratch*")
        (let ((scratch (get-buffer-create "*scratch*")))
          (set-buffer-major-mode scratch)
          scratch)))

  ;;Taken from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
  (defun rename-file-and-buffer ()
    "Rename the current buffer and file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (message "Buffer is not visiting a file!")
        (let ((new-name (read-file-name "New name: " filename)))
          (cond
           ((vc-backend filename) (vc-rename-file filename new-name))
           (t
            (rename-file filename new-name t)
            (set-visited-file-name new-name t t)))))))

  (defun alternate-buffer (&optional window)
    "Switch back and forth between current and last buffer in the current WINDOW."
    (interactive)
    (let ((current-buffer (window-buffer window)))
      ;; if no window is found in the windows history, `switch-to-buffer' will
      ;; default to calling `other-buffer'.
      (switch-to-buffer
       (cl-find-if (lambda (buffer)
                     (not (eq buffer current-buffer)))
                   (mapcar #'car (window-prev-buffers window)))
       nil t)))

  (defun alternate-window ()
    "Switch back and forth between current and last window in the current frame."
    (interactive)
    (let (;; switch to first window previously shown in this frame
          (prev-window (get-mru-window nil t t)))
      ;; Check window was not found successfully
      (unless prev-window (user-error "Last window not found"))
      (select-window prev-window)))

  (general-create-definer leader-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer major-def
    :states '(normal visual insert motion emacs)
    :prefix "SPC m"
    :non-normal-prefix "C-SPC m")

  (general-def '(normal visual) '(prog-mode-map restclient-mode-map)
    "RET" (general-simulate-key "SPC m"))

  (leader-def
    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "u"   (general-simulate-key "C-u")
    "x"   (general-simulate-key "C-x")
    "TAB" 'alternate-buffer

    "m" '(:ignore t :which-key "major mode")

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit")
    "qq"  'kill-emacs
    "qz"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffers")
    "bd"  'kill-this-buffer
    "bn"  'next-buffer
    "bp"  'previous-buffer
    "bq"  'kill-buffer-and-window
    "bR"  'rename-file-and-buffer
    "br"  'revert-buffer

    ;; Window operations
    "w"   '(:ignore t :which-key "windows")
    "w TAB" 'alternate-window
    "wv"  'split-window-horizontally
    "ws"  'split-window-vertically
    "wV"  (lambda () (interactive)(split-window-horizontally) (other-window 1))
    "wS"  (lambda () (interactive)(split-window-vertically) (other-window 1))
    "wm"  'maximize-window
    "w="  'balance-windows
    "wu"  'winner-undo
    "wr"  'winner-redo
    "ww"  'other-window
    "wd"  'delete-window
    "wD"  'delete-other-windows

    ;; File operations
    "f"   '(:ignore t :which-key "files")
    "fc"  'write-file
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer
    "fe"  '(:ignore t :which-key "emacs")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fep" 'list-packages

    ;; applications
    "a"   '(:ignore t :which-key "applications")
    "ad"  'dired
    ":"   'shell-command
    ";"   'eval-expression
    "ac"  'calendar
    "aq"  'quick-calc
    "oa"  'org-agenda)

  (general-def 'normal doc-view-mode-map
    "j"   'doc-view-next-line-or-next-page
    "k"   'doc-view-previous-line-or-previous-page
    "gg"  'doc-view-first-page
    "G"   'doc-view-last-page
    "C-d" 'doc-view-scroll-up-or-next-page
    "C-f" 'doc-view-scroll-up-or-next-page
    "C-b" 'doc-view-scroll-down-or-previous-page)

  (general-def '(normal visual) outline-minor-mode-map
    "zn"  'outline-next-visible-heading
    "zp"  'outline-previous-visible-heading
    "zf"  'outline-forward-same-level
    "zB"  'outline-backward-same-level)

  (general-def 'normal package-menu-mode-map
    "i"   'package-menu-mark-install
    "U"   'package-menu-mark-upgrades
    "d"   'package-menu-mark-delete
    "u"   'package-menu-mark-unmark
    "x"   'package-menu-execute
    "q"   'quit-window))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (set-face-foreground 'highlight nil)
  (set-face-background 'highlight nil)
  (set-face-attribute 'highlight nil :underline t))

(use-package doom-modeline
  ;; :hook (after-init . doom-modeline-mode)
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root
        doom-modeline-icon nil))

(use-package rainbow-delimiters
  ;; :config (rainbow-delimiters-mode 1))
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :ensure nil
  ;; :hook (after-init . recentf-mode)
  :config
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup 600))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-minibuffer t
        evil-want-Y-yank-to-eol t)
  (evil-mode 1)
  :config
  (add-hook 'window-configuration-change-hook #'evil-normalize-keymaps)
  (evil-set-initial-state 'shell-mode 'normal)
  (evil-set-initial-state 'doc-view-mode 'normal)
  (evil-set-initial-state 'package-menu-mode 'normal)
  (evil-set-initial-state 'biblio-selection-mode 'motion)
  (defvar doc-view-continuous t)
  :general
  (leader-def
    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new
    "fd"  'evil-save-and-close))

(use-package evil-terminal-cursor-changer
  :unless window-system
  :after evil
  :init
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  :config
  (evil-terminal-cursor-changer-activate))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1)
  :general
  ('normal override-global-map
    "gc"  'evil-commentary
    "gC" 'evil-commentary-line))

(use-package evil-visualstar
  :after evil
  :config
  (defvar evilmi-always-simple-jump t)
  (global-evil-visualstar-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package avy
  :after evil
  :general
  (general-nmap
    "'" 'avy-goto-char))

(use-package company
  :config
  (global-company-mode 1)
  (setq
    company-idle-delay 0.0
    company-minimum-prefix-length 1
    company-tooltip-align-annotations t
    company-frontends '(company-echo-metadata-frontend
                         company-pseudo-tooltip-unless-just-one-frontend
                         company-preview-frontend)
    company-backends '(company-capf company-files))
    ;; company-frontends '(company-echo-metadata-frontend
    ;;                      company-pseudo-tooltip-unless-just-one-frontend
    ;;                      company-preview-frontend)
    ;; company-backends '((company-capf company-files)
    ;;                     (company-dabbrev-code company-keywords)
    ;;                     company-dabbrev company-yasnippet))
  :general
  ('company-active-map
    "C-j" 'company-select-next-or-abort
    "C-k" 'company-select-previous-or-abort))

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq
    ivy-use-virtual-buffers t
    ivy-count-format "(%d/%d) "
    ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (set-face-attribute 'ivy-current-match nil
                      :underline t
                      :background nil
                      :weight 'semi-bold)
  :general
  (general-def
    '(normal insert visual emacs)
    '(ivy-minibuffer-map projectile-mode-map counsel-mode-map)
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line)
  (leader-def "bb"  'ivy-switch-buffer))

(use-package smex
  :after ivy)

(use-package counsel
  :after ivy
  :config
  (setq ivy-initial-inputs-alist nil)
  :general
  (leader-def
    "SPC" 'counsel-M-x
    "/"   'counsel-rg
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf
    "fL"  'counsel-locate))

(use-package projectile
  ;; :hook (emacs-startup . projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-other-file-alist '("component.ts" "component.html"))
  (add-to-list 'projectile-other-file-alist '("component.html" "component.ts")))

(use-package counsel-projectile
  :after (projectile ivy)
  :custom
  (counsel-projectile-find-file-matcher 'counsel-projectile-find-file-matcher-basename)
  :general
  (leader-def
   "p"   '(:ignore t :which-key "projects")
   "pd"  'counsel-projectile-dired-find-dir
   "po"  'projectile-find-other-file
   "pO"  'projectile-find-other-file-other-window
   "pf"  'counsel-projectile-find-file
   "pp"  'counsel-projectile-switch-project
   "pb"  'counsel-projectile-switch-to-buffer))

(use-package treemacs
  :commands (treemacs)
  :general
  (leader-def
    "pt" 'treemacs))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :general
  (leader-def
   "e"   '(:ignore t :which-key "errors")
   "en"  'flycheck-next-error
   "ep"  'flycheck-previous-error))


(use-package magit
  :commands (magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  :general
  (leader-def
   "g"   '(:ignore t :which-key "git")
   "gs"  'magit-status
   "gf"  'magit-log-buffer-file
   "gb"  'magit-blame-addition)
  (normal
   magit-blame-mode-map
   "q" 'magit-blame-quit))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package git-timemachine
  :general
  (leader-def
    "gt" 'git-timemachine)
  (general-def
    '(normal visual)
    git-timemachine-mode-map
    "C-k" 'git-timemachine-show-previous-revision
    "C-j" 'git-timemachine-show-next-revision
    "q"    'git-timemachine-quit))

(use-package git-gutter
  :hook (prog-mode . global-git-gutter-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)))

;; (use-package tide
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode))
;;   :config
;;   (setq tide-tsserver-executable "/usr/bin/tsserver")
;;   (setq tide-completion-detailed t)
;;   :general
;;   (general-nmap typescript-mode-map
;;     "gd" 'tide-jump-to-definition
;;     "K"  'tide-documentation-at-point)
;;   (general-nmap 'tide-references-mode-map
;;     "gj" 'tide-find-next-reference
;;     "gk" 'tide-find-previous-reference
;;     (kbd "C-j") 'tide-find-next-reference
;;     (kbd "C-k") 'tide-find-previous-reference
;;     (kbd "RET") 'tide-goto-reference
;;     "q" 'quit-window)
;;   (major-def
;;     :keymaps 'typescript-mode-map
;;     "=" 'tide-format
;;     "r" '(:ignore t :which-key "refactor")
;;     "rf" 'tide-fix
;;     "rr" 'tide-rename-symbol
;;     "ro" 'tide-organize-imports))

(use-package web-mode
  :init (setq web-mode-enable-auto-pairing 'nil)
  :mode "\\.html\\'"
  :general
  (major-def
    :keymaps 'web-mode-map
    "rr" 'web-mode-element-rename))

(use-package emmet-mode
  :hook web-mode
  :general
  (general-def
    'insert
    '(emmet-mode-map company-active-map web-mode-map)
    "TAB" 'emmet-expand-line))

(use-package lsp-mode
  :init
  (defvar lsp-prefer-flymake nil)
  (setq lsp-prefer-capf t)
  (setq lsp-completion-styles '(basic flex))
  (setq lsp-clients-angular-language-server-command
    '("node"
       "/home/maurn/.npm-global/lib/node_modules/@angular/language-server"
       "--ngProbeLocations"
       "/home/maurn/.npm-global/lib/node_modules"
       "--tsProbeLocations"
       "/home/maurn/.npm-global/lib/node_modules"
       "--stdio"))
  :commands lsp
  :hook (typescript-mode . lsp)
  :general
  (general-def 'normal lsp-mode-map
    ;; [remap evil-goto-definition] 'lsp-find-definition)
    "gd" 'lsp-find-definition)
  (major-def
    :keymaps '(c-mode-map c++-mode-map rust-mode-map)
    "r" '(:ignore t :which-key "refactor")
    "rr" 'lsp-rename
    "=" 'lsp-format-buffer))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(use-package nim-mode
  :mode ("\\.nim\\'" . nim-mode)
  :hook (nim-mode . lsp))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :general
  (major-def
    :keymaps 'restclient-mode-map
    "s" 'restclient-http-send-current
    "j" 'restclient-jump-next
    "k" 'restclient-jump-prev))

(use-package latex
  :mode ("\\.tex\\'" . latex-mode)
  :hook (latex-mode . auto-fill-mode)
  :ensure auctex
  :config
  (TeX-PDF-mode 1))

(use-package company-auctex
  :after (auctex company)
  :config (company-auctex-init))

(use-package auctex-latexmk
  :after (auctex company)
  :config (auctex-latexmk-setup))

(use-package glsl-mode
  :mode (("\\.frag\\'" . glsl-mode)))

(use-package gcmh
  ;; :hook (after-init . gcmh-mode)
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 10))
  ;; (gcmh-high-cons-threshold (* 16 1024 1024)))

(provide 'init)
;;; init.el ends here
