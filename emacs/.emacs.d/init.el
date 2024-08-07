;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:

(setopt
 read-process-output-max (* 1024 1024) ;; 1mb
 indent-tabs-mode nil
 tab-width 2
 evil-shift-width 2

 use-package-always-ensure t

 show-paren-delay 0
 vc-follow-symlinks t
 help-window-select 't
 fill-column 80
 initial-scratch-message nil
 sentence-end-double-space nil

 ;; scroll one line at a time (less "jumpy" than defaults)
 mouse-wheel-scroll-amount '(1 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil            ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't                  ;; scroll window under mouse
 scroll-step 1                                ;; keyboard scroll one line at a time

 use-dialog-box nil
 ring-bell-function 'ignore
 visible-bell nil
 use-dialog-box nil

 comment-auto-fill-only-comments t

 make-backup-files nil ; stop creating backup~ files
 auto-save-default nil ; stop creating #autosave# files
 create-lockfiles nil) ; no fucking lockfiles either

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(set-default-coding-systems 'utf-8)
(global-hl-line-mode 1)
(blink-cursor-mode 0)
(winner-mode 1)
(save-place-mode 1)
(put 'narrow-to-region 'disabled nil)
(show-paren-mode 1)
(electric-pair-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun my-prog-mode-hook ()
  "Prog hook!"
  (display-line-numbers-mode)
  (setq truncate-lines t
        auto-hscroll-mode 'current-line))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(require 'use-package)

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LSP_USE_PLISTS")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

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
    "aq" 'quick-calc))


(use-package which-key
  :config
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :config
  (doom-modeline-mode 1)
  (setopt doom-modeline-buffer-file-name-style 'truncate-upto-root
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
  (setopt recentf-max-saved-items 300
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

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil))

(use-package evil
  :init
  (global-undo-tree-mode 1)
  (setopt evil-want-C-u-scroll t
          evil-want-minibuffer t
          evil-want-keybinding nil
          evil-undo-system 'undo-tree
          evil-want-Y-yank-to-eol t)
  (evil-mode 1)
  :config
  (add-hook 'window-configuration-change-hook #'evil-normalize-keymaps)
  ;; (evil-set-initial-state 'shell-mode 'normal)
  ;; (evil-set-initial-state 'doc-view-mode 'normal)
  ;; (evil-set-initial-state 'package-menu-mode 'normal)
  ;; (evil-set-initial-state 'biblio-selection-mode 'motion)
  (defvar doc-view-continuous t)
  :general
  (leader-def
    "wh"  'evil-window-left
    "wl"  'evil-window-right
    "wj"  'evil-window-down
    "wk"  'evil-window-up
    "bN"  'evil-buffer-new
    "fd"  'evil-save-and-close))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config (evil-commentary-mode 1)
  :general
  (general-nmap
    "gc" 'evil-commentary
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
  (setopt evil-motion-state-cursor 'box   ; █
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

(use-package savehist
  :config
  (savehist-mode 1))

(use-package vertico
  :init
  (vertico-mode)

  (setopt enable-recursive-minibuffers t)

  :general
  (general-iemap
    minibuffer-local-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous)
  (general-nvmap
    minibuffer-local-map
    "j" 'vertico-next
    "k" 'vertico-previous)
  (leader-def "bb" 'consult-buffer)
  (leader-def "bB" 'consult-buffer-other-window))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-initialism))
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :init
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
  :general
  (leader-def
    "SPC" (general-simulate-key "M-x")
    "ff"  'find-file
    "fr"  'consult-recent-file
    "/"   'consult-ripgrep))
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (consult embark)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 't)
  :config
  (delete 'yaml treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package yaml-mode
  :custom
  (yaml-ts-mode-hook yaml-mode-hook))

(use-package projectile
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-other-file-alist '("component.ts" "component.html"))
  (add-to-list 'projectile-other-file-alist '("component.html" "component.ts"))
  :general
  (leader-def
    "p"   '(:ignore t :which-key "projects")
    "po"  'projectile-find-other-file
    "pO"  'projectile-find-other-file-other-window
    "pf"  'projectile-find-file
    "pF"  'projectile-find-file-other-window
    "pp"  'projectile-switch-project
    "pb"  'projectile-switch-to-buffer
    "pB"  'projectile-switch-to-buffer-other-window))

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
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  :general
  (leader-def
    "g"   '(:ignore t :which-key "git")
    "gs"  'magit-status
    "gf"  'magit-log-buffer-file
    "gb"  'magit-blame-addition)
  (normal
   magit-blame-mode-map
   "q" 'magit-blame-quit))

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

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package corfu
  :custom
  (corfu-auto t)
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  (corfu-auto-delay  0.01) ;; TOO SMALL - NOT RECOMMENDED
  (corfu-auto-prefix 1) ;; TOO SMALL - NOT RECOMMENDED
  :bind
  ;; Another key binding can be used, such as S-SPC.
  ;; (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :config
  (setq corfu-min-width 30)
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-hook 'snippet-mode-hook
            '(lambda ()
               (setq-local require-final-newline nil))))

(use-package web-mode
  :mode (
         ("\\.svelte\\'" . svelte-mode))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing nil)
  :hook
  (web-mode . (lambda ()
                (setq-local
                 electric-pair-pairs
                 (append electric-pair-pairs '((?' . ?'))))))
  :config
  (define-derived-mode svelte-mode web-mode "Svelte")
  (setq web-mode-engines-alist
        '(("svelte" . "\\.svelte\\'"))))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-eldoc-render-all t)
  :config
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :commands (lsp lsp-deferred)
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (((typescript-ts-mode
     svelte-mode
     c++-mode
     c-mode
     rust-ts-mode
     python-mode) . lsp-deferred))
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

(use-package lsp-java)

(setq gc-cons-threshold 100000000) ; 100 Mb

(provide 'init)
;;; init.el ends here
