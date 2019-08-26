;;; init.el --- Initialization file for Emacs  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs Startup File --- initialization for Emacs
;;; Package --- Summary
;;; Code:
(eval-and-compile
  (setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6))

(with-eval-after-load 'display-line-numbers
  (defvar display-line-numbers-type 'relative))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode 0)
(winner-mode 1)
(put 'narrow-to-region 'disabled nil)
(defvar show-paren-delay 0)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq vc-follow-symlinks t)
(setq fill-column 100)

;; for fixing 'package unavailable' issues
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

(setq help-window-select 't)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore
      visible-bell nil)

;; Disable backup files
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; no fucking lockfiles either

(add-to-list 'default-frame-alist '(font . "SourceCodeProSemibold-11"))

(defun my-prog-mode-hook ()
  "Prog hook!"
  (flycheck-mode)
  (display-line-numbers-mode)
  (setq truncate-lines t))

(add-hook 'prog-mode-hook 'my-prog-mode-hook)

;;; We’re going to set the load-path ourselves and avoid calling
;;; (package-initilize) (for performance reasons) so we need to set
;;; package--init-file-ensured to true to tell package.el to not
;;; automatically call it on our behalf. Additionally we’re setting
;;; package-enable-at-startup to nil so that packages will not
;;; automatically be loaded for us since use-package will be handling
;;; that.
(eval-and-compile
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil)

  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))

  (setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))


(eval-when-compile
  (require 'package)
  ;; tells emacs not to load any packages before starting up
  ;; the following lines tell emacs where on the internet to look up
  ;; for new packages.
  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("elpa"  . "https://elpa.gnu.org/packages/")
                           ("org"   . "https://orgmode.org/elpa/")))
  ;; (package-initialize)
  (unless package--initialized (package-initialize t))

  ;; Bootstrap `use-package'
  (unless (package-installed-p 'use-package) ; unless it is already installed
    (package-refresh-contents) ; update packages archive
    (package-install 'use-package)) ; and install the most recent version of use-package

  (require 'use-package)
  (setq use-package-always-ensure t))

(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0))

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
    "fe"  '(:ignore t :which-key "emacs")
    "fed" 'find-user-init-file
    "feR" 'load-user-init-file
    "fj"  'dired-jump
    "fl"  'find-file-literally
    "fR"  'rename-file-and-buffer
    "fs"  'save-buffer

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
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
  (setq doom-modeline-icon nil))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-minibuffer t)
  (defvar evil-want-Y-yank-to-eol t)
  :hook (after-init . evil-mode)
  :config
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

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "'"))

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0
    company-minimum-prefix-length 1
    company-tooltip-align-annotations t
    company-frontends '(company-echo-metadata-frontend
                         company-pseudo-tooltip-unless-just-one-frontend
                         company-preview-frontend)
    company-backends '((company-capf company-files)
                        (company-dabbrev-code company-keywords)
                        company-dabbrev company-yasnippet))
  :general
  ('company-active-map
    "C-j" 'company-select-next-or-abort
    "C-k" 'company-select-previous-or-abort))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config (setq ivy-use-virtual-buffers t
            ivy-count-format "(%d/%d) "
            ivy-initial-inputs-alist nil
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

(use-package smex)

(use-package counsel
  :after (ivy)
  :general
  (leader-def
    "SPC" 'counsel-M-x
    "/"   'counsel-rg
    "ff"  'counsel-find-file
    "fr"  'counsel-recentf
    "fL"  'counsel-locate))

(use-package projectile
  :config (projectile-mode +1))

(use-package counsel-projectile
  :after (projectile ivy)
  :general
  (leader-def
   "p"   '(:ignore t :which-key "projects")
   "pd"  'counsel-projectile-dired-find-dir
   "po"  'counsel-projectile-find-other-file
   "pf"  'counsel-projectile-find-file
   "pp"  'counsel-projectile-switch-project
   "pb"  'counsel-projectile-switch-to-buffer))

(use-package treemacs
  :general
  (leader-def
    "pt" 'treemacs))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package flycheck
  :commands (flycheck-mode)
  :general
  (leader-def
   "e"   '(:ignore t :which-key "errors")
   "en"  'flycheck-next-error
   "ep"  'flycheck-previous-error))


(use-package eldoc-box
  :init
  (setq eldoc-idle-delay 0)
  (setq eldoc-echo-area-use-multiline-p t)

  (defun eldoc-message-now ()
    (interactive))

  (defun eldoc--message-command-p (command)
    ;; Should be using advice, but I'm lazy
    ;; One can also loop through `eldoc-message-commands' and empty it out
    (eq command 'eldoc-message-now))

  (eldoc-add-command 'eldoc-message-now)
  (leader-def "t" 'eldoc-message-now)
  :hook
  (eldoc-mode . eldoc-box-hover-mode)
  (eldoc-box-hover-mode . eldoc-box-hover-at-point-mode)
  :config
  (setq eldoc-box-cleanup-interval 0)
  (define-advice eldoc-box--default-at-point-position-function
    (:override (width height) display-above-point)
    "Set `eldoc-box-position-function' to this function to have
     childframe appear above point.  Position is calculated base on WIDTH
     and HEIGHT of childframe text window"
    (let* ((point-pos (eldoc-box--point-position-relative-to-native-frame))
            ;; calculate point coordinate relative to native frame
            ;; because childframe coordinate is relative to native frame
            (x (car point-pos))
            (y (cdr point-pos))
            ;; (en (frame-char-width))
            (em (frame-char-height))
            (frame-geometry (frame-geometry)))
      (cons (if (< (- (frame-inner-width) width) x)
              ;; space on the right of the pos is not enough
              ;; put to left
              (max 0 (- x width))
              ;; normal, just return x
              x)
        (if (let ((pos (point)))
              (goto-char (line-beginning-position))
              (prog1 (bobp)
                (goto-char pos)))
          ;; space under the pos is not enough
          ;; put above
          (+ y em)
          ;; normal, just return y + em
          (max 0 (- y height)))))))

(use-package magit
  :commands (magit-status)
  :general
  (leader-def
   "g"   '(:ignore t :which-key "git")
   "gs"  'magit-status
   "gf"  'magit-log-buffer-file
   "gb"  'magit-blame-addition))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init))

(use-package git-timemachine
  :commands (git-timemachine)
  :config
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "\C-k" 'git-timemachine-show-previous-revision
    "\C-j" 'git-timemachine-show-next-revision
    "q"    'git-timemachine-quit)
    ;; "gtg"  'git-timemachine-show-nth-revision
    ;; "gtt"  'git-timemachine-show-revision-fuzzy
    ;; "gty"  'git-timemachine-kill-abbreviated-revision
    ;; "gtY"  'git-timemachine-kill-revision
    ;; "gtb" 'git-timemachine-blame)
  :general
  ;; (general-def
  ;;   :definer 'minor-mode
  ;;   :states 'normal
  ;;   :keymaps 'git-timemachine-mode
  ;;   "\C-k" 'git-timemachine-show-previous-revision
  ;;   "\C-j" 'git-timemachine-show-next-revision
  ;;   "q"    'git-timemachine-quit)
  (leader-def
    "gt" 'git-timemachine))

(use-package git-gutter
  :hook (prog-mode . global-git-gutter-mode))

(use-package shell-pop
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (evil-define-key 'insert term-raw-map
    (kbd "C-k") 'term-send-up
    (kbd "C-j") 'term-send-down)
  :general
  (leader-def
    "'" 'shell-pop))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)))

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :config
  (setq tide-tsserver-executable "/usr/bin/tsserver")
  :general
  (general-nmap typescript-mode-map
    "gd" 'tide-jump-to-definition
    "K"  'tide-documentation-at-point)
  (major-def
    :keymaps 'typescript-mode-map
    "=" 'tide-format
    "r" '(:ignore t :which-key "refactor")
    "rf" 'tide-fix
    "rr" 'tide-rename-symbol
    "ro" 'tide-organize-imports))

(use-package web-mode
  :mode "\\.html\\'"
  :general
  (major-def
    :keymaps 'web-mode-map
    "z" 'web-mode-fold-or-unfold))

(use-package emmet-mode
  :hook web-mode
  :general
  (general-def
    'insert
    '(emmet-mode-map company-active-map web-mode-map)
    "TAB" 'emmet-expand-line))

(use-package anaconda-mode
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :general
  (general-nmap python-mode-map
    "gd" 'anaconda-mode-find-definitions
    "K"  'anaconda-mode-show-doc)
  (major-def
    :keymaps 'python-mode-map
    "ss" 'run-python
    "sb" 'python-shell-send-buffer
    "sf" 'python-shell-send-defun))

(use-package company-anaconda
  :after anaconda-mode
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package lsp-mode
  :init (setq lsp-prefer-flymake nil)
  :commands lsp
  :general
  (general-nmap (c-mode-map c++-mode-map rust-mode-map)
    "gd" 'lsp-find-definition)
  (major-def
    :keymaps '(c-mode-map c++-mode-map rust-mode-map)
    "r" '(:ignore t :which-key "refactor")
    "rr" 'lsp-rename
    "=" 'lsp-format-buffer))

(use-package company-lsp :commands company-lsp)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :hook (rust-mode . lsp))

(use-package flycheck-rust
  :hook (rust-mode . flycheck-rust-setup))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :general
  (major-def
    :keymaps 'restclient-mode-map
    "s" 'restclient-http-send-current
    "j" 'restclient-jump-next
    "k" 'restclient-jump-prev))

;; (use-package latex
;;   ;; :mode ("\\.tex\\'" . latex-mode)
;;   :ensure auctex
;;   :config
;;     (add-to-list 'TeX-view-program-list
;;                  '("Zathura"
;;                    ("zathura "
;;                     (mode-io-correlate " --synctex-forward %n:0:%b   -x \"emacsclient +%{line} %{input}\" ")
;;                     " %o")
;;                    "zathura"))
;;     (add-to-list 'TeX-view-program-selection
;;                  '(output-pdf "Zathura"))
;;     (TeX-PDF-mode 1))

;; (use-package company-auctex
;;   ;; :after (auctex company)
;;   :config (company-auctex-init))

;; (use-package auctex-latexmk
;;   ;; :after (auctex company)
;;   :config (auctex-latexmk-setup))

(eval-when-compile
  (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(eval-and-compile
  (add-hook 'emacs-startup-hook '(lambda ()
                                   (setq gc-cons-threshold 16777216
                                         gc-cons-percentage 0.1))))
(provide 'init)
;;; init.el ends here
