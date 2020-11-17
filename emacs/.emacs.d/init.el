;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:


(setq-default
  read-process-output-max (* 1024 1024) ;; 1mb
  indent-tabs-mode nil
  tab-width 4
  show-paren-delay 0
  vc-follow-symlinks t
  help-window-select 't
  fill-column 80
  initial-scratch-message nil

  ;; scroll one line at a time (less "jumpy" than defaults)
  mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
  mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
  mouse-wheel-follow-mouse 't                  ;; scroll window under mouse
  scroll-step 1                                ;; keyboard scroll one line at a time

  use-dialog-box nil
  ring-bell-function 'ignore
  visible-bell nil

  comment-auto-fill-only-comments t

  make-backup-files nil ; stop creating backup~ files
  auto-save-default nil ; stop creating #autosave# files
  create-lockfiles nil) ; no fucking lockfiles either


(defalias 'yes-or-no-p 'y-or-n-p)

(set-default-coding-systems 'utf-8)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(winner-mode 1)
(save-place-mode 1)
(put 'narrow-to-region 'disabled nil)
(show-paren-mode 1)
(electric-pair-mode 1)

(defun my-prog-mode-hook ()
  "Prog hook!"
  (display-line-numbers-mode)
  (setq truncate-lines t
        auto-hscroll-mode 'current-line))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)


(eval-when-compile
  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file))

  (setq-default package-archives '(("melpa" . "https://melpa.org/packages/")
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

(use-package general
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

  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  (general-create-definer major-def
    :states '(normal visual insert motion emacs)
    :prefix "RET"
    :non-normal-prefix "C-RET")

  (leader-def
    ""     nil
    "c"   (general-simulate-key "C-c")
    "h"   (general-simulate-key "C-h")
    "TAB" 'alternate-buffer

    ;; Quit operations
    "q"	  '(:ignore t :which-key "quit")
    "qq"  'kill-emacs
    "qz"  'delete-frame

    ;; Buffer operations
    "b"   '(:ignore t :which-key "buffers")
    "bd"  'kill-this-buffer
    "bD"  'kill-other-buffers
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

    ;; text
    "t"  '(:ignore t :which-key "text")
    "ta" 'align
    "tA" 'align-regexp
    "t+" 'text-scale-adjust

    ;; applications
    "a"  '(:ignore t :which-key "applications")
    "ad" 'dired
    "ac" 'calendar
    "ap" 'list-packages
    "aq" 'quick-calc)

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


(use-package which-key
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))
  ;; (set-face-foreground 'highlight nil)
  ;; (set-face-background 'highlight nil)
  ;; (set-face-attribute 'highlight nil :underline t))
  ;; :custom-face
  ;; ('highlight ))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root
        doom-modeline-icon nil))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode)
  :hook (kill-emacs . recentf-save-list)
  :config
  (setq recentf-max-saved-items 300
    recentf-auto-cleanup 600
    recentf-exclude '("/tmp/"
                       "/ssh:"
                       "/sudo:"
                       "recentf$"
                       "/elpa/"
                       "/snippets/"))
  (defun recentf-save-list/silent ()
    (let ((save-silently t)) (recentf-save-list)))
  (run-at-time nil (* 5 60) 'recentf-save-list/silent))

(use-package evil
  :init
  (global-undo-tree-mode 1)
  (setq evil-want-C-u-scroll t
        evil-want-minibuffer t
        evil-undo-system 'undo-tree
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

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1)
  :general
  (general-nmap
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

(use-package evil-terminal-cursor-changer
  :unless window-system
  :after evil
  :init
  (setq evil-motion-state-cursor 'box   ; █
        evil-visual-state-cursor 'box   ; █
        evil-normal-state-cursor 'box   ; █
        evil-insert-state-cursor 'bar   ; ⎸
        evil-emacs-state-cursor  'hbar) ; _
  :config
  (evil-terminal-cursor-changer-activate))

(use-package xclip
  :unless window-system
  :config
  (xclip-mode 1))

(use-package avy
  :after evil
  :general
  ('(normal visual motion)
    "'" 'avy-goto-char))

(use-package company
  :init
  (global-company-mode 1)
  :config
  (setq
    company-idle-delay 0.0
    company-minimum-prefix-length 1
    company-tooltip-align-annotations t
    completion-ignore-case t
    ;; company-frontends '(company-echo-metadata-frontend
    ;;                      company-pseudo-tooltip-unless-just-one-frontend
    ;;                      company-preview-frontend)
    company-backends '(company-files (company-capf :with company-yasnippet)))
    ;; company-frontends '(company-echo-metadata-frontend
    ;;                      company-pseudo-tooltip-unless-just-one-frontend
    ;;                      company-preview-frontend)
    ;; company-backends '((company-capf company-files)
    ;;                     (company-dabbrev-code company-keywords)
    ;;                     company-dabbrev company-yasnippet))
  (add-hook 'evil-normal-state-entry-hook #'company-abort)

  :general
  ('company-active-map
    "C-j" 'company-select-next-or-abort
    "C-k" 'company-select-previous-or-abort))

(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq enable-recursive-minibuffers t
        ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((t . ivy--regex-ignore-order)))

  (add-hook 'server-after-make-frame-hook
    '(lambda () (set-face-attribute 'ivy-current-match nil
                  :underline t
                  :background nil
                  :weight 'semi-bold)))

  :general
  (general-iemap
    ivy-minibuffer-map
    "C-'" 'ivy-avy
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line)
  (general-nvmap
    ivy-minibuffer-map
    "'" 'ivy-avy
    "j" 'ivy-next-line
    "k" 'ivy-previous-line)
  (leader-def "bb"  'ivy-switch-buffer))

(use-package ivy-xref
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

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
  :init
  (setq treemacs-follow-mode t)
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
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
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
    "q"   'git-timemachine-quit))

(use-package git-gutter
  :hook (prog-mode . global-git-gutter-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'snippet-mode-hook
    '(lambda ()
       (setq-local require-final-newline nil))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package gdscript-mode
  :mode "\\.gd\\'")

(use-package yaml-mode)

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
  (defvar lsp-clients-angular-language-server-command
    '("node"
       "/home/maurn/.npm-global/lib/node_modules/@angular/language-server"
       "--ngProbeLocations"
       "/home/maurn/.npm-global/lib/node_modules"
       "--tsProbeLocations"
       "/home/maurn/.npm-global/lib/node_modules"
       "--stdio"))
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode
          web-mode
          c++-mode
          c-mode
          rust-mode
          python-mode
          nim-mode) . lsp-deferred)
  :general
  (general-def 'normal lsp-mode-map
    "gd" 'lsp-find-definition
    "gt" 'lsp-find-type-definition
    "gr" 'lsp-find-references
    "K" 'lsp-describe-thing-at-point)
  (major-def 'lsp-mode-map
    "r" '(:ignore t :which-key "refactor")
    "rr" 'lsp-rename
    "rf" 'lsp-execute-code-action
    "ro" 'lsp-organize-imports
    "=" 'lsp-format-buffer))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(use-package nim-mode
  :mode "\\.nim\\'")

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :general
  (major-def 'restclient-mode-map
    "s" 'restclient-http-send-current
    "j" 'restclient-jump-next
    "k" 'restclient-jump-prev))

(use-package latex
  :mode ("\\.tex\\'" . latex-mode)
  :hook (text-mode . auto-fill-mode)
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
  :config
  (gcmh-mode 1)
  :custom
  (gcmh-idle-delay 10))
  ;; (gcmh-high-cons-threshold (* 16 1024 1024)))

(provide 'init)
;;; init.el ends here
