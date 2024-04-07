;;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package diminish :ensure t)

(use-package no-littering
  :ensure t)

(use-package smex
  :ensure t)

(use-package browse-kill-ring
  :ensure t
  :commands browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item t)
  (setq browse-kill-ring-maximum-display-length 99)
  (setq browse-kill-ring-show-preview t))
(global-set-key (kbd "M-y") 'browse-kill-ring)

(use-package anzu
  :ensure t
  :commands (anzu-query-replace-regexp anzu-query-replace)
  :init
  (global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

(use-package multiple-cursors
  :ensure t
  :commands (mc/mark-next-like-this mc/mark-previous-like-this)
  :init
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :init
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package move-text
  :ensure t
  :commands (move-text-up move-text-down)
  :init
  (global-set-key (kbd "M-P") 'move-text-up)
  (global-set-key (kbd "M-N") 'move-text-down))

(use-package symbol-overlay
  :ensure t
  :diminish (symbol-overlay-mode)
  :config
  (dolist (hook '(prog-mode-hook
                  html-mode-hook
                  go-ts-mode
                  python-ts-mode
                  conf-mode-hook
                  text-mode-hook
                  protobuf-mode-hook
                  yaml-mode-hook))
    (add-hook hook 'symbol-overlay-mode))
  (define-key symbol-overlay-mode-map (kbd "M-i") 'symbol-overlay-put)
  (define-key symbol-overlay-mode-map (kbd "M-I") 'symbol-overlay-remove-all)
  (define-key symbol-overlay-mode-map (kbd "M-n") 'symbol-overlay-jump-next)
  (define-key symbol-overlay-mode-map (kbd "M-p") 'symbol-overlay-jump-prev))

(use-package project
  :config
  (define-key ctl-x-map "p" 'nil)
  (define-key mode-specific-map "p" project-prefix-map))

(use-package company
  :ensure t
  :custom
  (company-idle-delay 0.2)
  (company-tooltip-idle-delay 0.5)
  (company-minimum-prefix-length 1)
  (company-require-match nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-show-numbers t)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  (
   :map company-active-map
   ("M-/" . company-other-backend)
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("M-v" . company-previous-page)
   ("C-v" . company-next-page)
   ))

(use-package dired-subtree
  :defer t
  :ensure t
  :commands (dired-subtree-toggle)
  :custom
  (dired-subtree-use-backgrounds nil)
  (dired-subtree-line-prefix "   "))

(use-package dired
  :config
  (define-key dired-mode-map (kbd "C-<return>") 'dired-subtree-toggle))

(use-package string-inflection
  :ensure t
  :commands (string-inflection-all-cycle)
  :init
  (global-set-key (kbd "C-c C-u") 'string-inflection-all-cycle))

(use-package citre
  :defer t
  :ensure t
  :init
  (require 'citre-config)
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   citre-default-create-tags-file-location 'global-cache
   citre-use-project-root-when-creating-tags t
   citre-prompt-language-for-ctags-command t
   citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package buffer-move
  :ensure t
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :config
  (global-set-key (kbd "<C-S-up>")     'buf-move-up)
  (global-set-key (kbd "<C-S-down>")   'buf-move-down)
  (global-set-key (kbd "<C-S-left>")   'buf-move-left)
  (global-set-key (kbd "<C-S-right>")  'buf-move-right))

(use-package leetcode
  :ensure t
  :defer t
  :commands (leetcode))

(use-package go-playground
  :ensure t
  :commands (go-playground)
  :init
  (setq go-playground-init-command "go mod init go-playground")
  (setq go-playground-basedir "~/go-playground"))


;;;;;;;;;;;;;
;; copilot ;;
;;;;;;;;;;;;;
(use-package copilot
  :load-path "site-lisp/copilot.el"
  :diminish copilot-mode
  :hook
  (prog-mode . copilot-mode)
  (yaml-mode . copilot-mode)
  (go-mode . copilot-mode)

  :custom
  (copilot-idle-delay 0.5)
  (copilot-indent-offset-warning-disable t)

  :bind
  (
   :map copilot-completion-map
   ("<right>" . copilot-accept-completion)
   ("<tab>" . copilot-accept-completion)
   ("M-f" . copilot-accept-completion-by-word)
   ("M-n" . copilot-next-completion)
   ("M-p" . copilot-previous-completion)
   ))


;;;;;;;;;;;;
;; format ;;
;;;;;;;;;;;;
(use-package reformatter
  :ensure t
  :config
  (reformatter-define goimports
    :program "goimports")
  (with-eval-after-load 'go-mode
    (add-hook 'go-mode-hook #'goimports-on-save-mode))

  (reformatter-define black-format
    :program "black"
    :args '("-t" "py310" "-"))

  (reformatter-define isort-format
    :program "isort"
    :args '("-")))

(use-package format-all
  :ensure t
  :commands (format-all-buffer))

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;
(use-package magit
  :ensure t
  :commands (magit-status magit-dispatch magit-file-dispatch)
  :bind
  (("C-x g" . magit-status)
   :map magit-hunk-section-map
   ("RET" . magit-diff-visit-file-other-window)
   :map magit-file-section-map
   ("RET" . magit-diff-visit-file-other-window)))

(use-package yasnippet :ensure t :defer t)

;;;;;;;;;
;; lsp ;;
;;;;;;;;;
(use-package lspce
  :load-path "site-lisp/lspce"
  :defer t
  :commands (lspce-mode)
  :init
  (setq lspce-send-changes-idle-time 1))

;;;;;;;;;
;; pdf ;;
;;;;;;;;;
(use-package pdf-tools
  :ensure t
  :defer t)


(use-package htmlize
  :ensure t
  :defer t)


(provide 'init-useful)
