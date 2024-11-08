;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setopt
 use-package-always-ensure t)

(electric-pair-mode)
(winner-mode)
(global-display-line-numbers-mode)
(pixel-scroll-precision-mode)
(auto-revert-mode)

(setq tab-always-indent 'complete)

(load "~/.emacs.d/functions.el")

(load "~/.emacs.d/editor.el")

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
  (corfu-auto nil)
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
  (add-hook 'evil-insert-state-exit-hook 'tempel-done)
  :general
  (general-def '(normal visual insert) tempel-map
    "C-l" 'tempel-next
    "C-h" 'tempel-previous
    "C-k" 'tempel-done
    ))

(use-package eglot-tempel
  :preface (eglot-tempel-mode)
  :init
  (eglot-tempel-mode t))

(use-package eglot
  :custom
  (eglot-confirm-server-initiated-edits nil)
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
