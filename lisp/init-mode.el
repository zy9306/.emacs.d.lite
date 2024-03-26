;;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode
  :defer t
  :ensure t
  :mode (("\\.go\\'" . go-mode)))

(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package yaml-mode
  :defer t
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package protobuf-mode
  :defer t
  :mode (("\\.proto\\'" . protobuf-mode))
  :config
  (defconst my-protobuf-style
    '((c-basic-offset . 4)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-protobuf-style" my-protobuf-style t))))

(use-package json-mode
  :defer t
  :ensure t
  :mode (("\\.json\\'" . json-mode))
  :init
  ;; https://github.com/joshwnj/json-mode/issues/72
  (setq-default json-mode-syntax-table
                (let ((st (make-syntax-table)))
                  ;; Objects
                  (modify-syntax-entry ?\{ "(}" st)
                  (modify-syntax-entry ?\} "){" st)
                  ;; Arrays
                  (modify-syntax-entry ?\[ "(]" st)
                  (modify-syntax-entry ?\] ")[" st)
                  ;; Strings
                  (modify-syntax-entry ?\" "\"" st)
                  ;; Comments
                  (modify-syntax-entry ?\n ">" st)
                  st)))


(provide 'init-mode)
