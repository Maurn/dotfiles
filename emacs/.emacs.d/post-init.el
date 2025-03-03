;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setopt
 read-process-output-max (* 1024 1024) ;; 1mb
 tab-width 2
 evil-shift-width 2

 use-package-always-ensure t

 eldoc-idle-delay 0.03

 show-paren-delay 0
 help-window-select 't
 fill-column 80
 initial-scratch-message nil
 sentence-end-double-space nil
 auto-hscroll-mode 'current-line

 comment-auto-fill-only-comments t)

(electric-pair-mode)
(winner-mode)
(pixel-scroll-precision-mode)
(auto-revert-mode)

(setq tab-always-indent 'complete)

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

(defun sudo ()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
             buffer-file-name))))

(use-package general
  :config
  (general-override-mode 1)
  (general-evil-setup)
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
    "w TAB" 'other-window
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

    "p"   '(:ignore t :which-key "projects")
    "pf"  'project-find-file
    "pF"  'project-find-file-other-window
    "pp"  'project-switch-project
    "pb"  'project-switch-to-buffer

    ;; errors
    "e"  '(:ignore t :which-key "errors")
    "en" 'flymake-goto-next-error
    "ep" 'flymake-goto-prev-error
    "es" 'consult-flymake

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

;; evil-want-keybinding must be declared before Evil and Evil Collection
(setq evil-want-keybinding nil)

(use-package evil
  :init
  (setopt evil-want-C-u-scroll t
          evil-want-minibuffer t
          evil-want-integration t
          evil-want-keybinding nil
          evil-undo-system 'undo-fu
          evil-want-Y-yank-to-eol t)
  (evil-mode 1)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (add-hook 'window-configuration-change-hook #'evil-normalize-keymaps)
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

(use-package undo-fu
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :config
  (undo-fu-session-global-mode))

(use-package evil-visualstar
  :after evil
  :commands global-evil-visualstar-mode
  :hook (after-init . global-evil-visualstar-mode))

(use-package evil-surround
  :after evil
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (after-init . global-evil-surround-mode))

(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

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
    "k" 'vertico-previous))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode))

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (setq consult-narrow-key "<")

  :general
  (leader-def
    "SPC" (general-simulate-key "M-x")
    "ff"  'find-file
    "fr"  'consult-recent-file
    "/"   'consult-ripgrep
    "bb" 'consult-buffer
    "bB" 'consult-buffer-other-window))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package forge)

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
  :config
  (global-git-gutter-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package apheleia
  :config
  (apheleia-global-mode +1))

(use-package corfu
  :custom
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space

  (corfu-min-width 30)

  (corfu-auto nil)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)

  (corfu-echo-delay 0)
  :config
  (corfu-echo-mode)
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter))

(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode)
  :config
  (setq-local
   truncate-lines nil
   display-line-numbers-mode nil))

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

(use-package yaml-mode)

(use-package typescript-mode)

(use-package rust-mode)

(use-package markdown-mode)

(use-package tempel
  :custom
  (tempel-path "~/dotfiles/emacs/.emacs.d/templates")
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'eglot-managed-mode-hook 'tempel-setup-capf)
  :general
  ('(normal visual insert) tempel-map
   "C-l" 'tempel-next
   "C-h" 'tempel-previous)
  ('(normal visual) tempel-map
   "TAB" 'tempel-next
   "<backtab>" 'tempel-previous
   "<escape>" 'tempel-done))

(use-package eglot-tempel
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

(use-package eglot
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  (eglot-report-progress nil)
  :config
  (add-to-list 'eglot-server-programs
               '(svelte-mode . ("svelteserver" "--stdio")))
  :hook
  (((typescript-mode
     svelte-mode
     c++-mode
     c-mode
     rust-mode
     python-mode) . eglot-ensure))
  (prog-mode . display-line-numbers-mode)
  :general
  (general-def 'normal eglot-mode-map
    "gd" 'xref-find-definitions
    "gt" 'eglot-find-typeDefinition
    "gr" 'xref-find-references)
  (major-def 'eglot-mode-map
    "r" '(:ignore t :which-key "refactor")
    "rr" 'eglot-rename
    "rf" 'eglot-code-actions
    "ro" 'eglot-code-action-organize-imports
    "=" 'aphelia-format-buffer))

(use-package eglot-booster
  :after eglot
  :config	(eglot-booster-mode))

;;; post-init.el ends here
