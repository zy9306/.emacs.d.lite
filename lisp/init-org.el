;;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package ox-hugo
  :ensure t
  :defer t)

(use-package org-download
  :ensure t
  :commands (org-download-clipboard)
  :init
  (setq-default org-download-heading-lvl nil)
  (put 'org-download-image-dir 'safe-local-variable #'stringp))

(use-package org
  :hook ((org-mode . org-indent-mode))
  :custom
  ;; (org-image-actual-width 600)
  (org-startup-numerated t)
  (org-fontify-quote-and-verse-blocks t)
  (org-startup-folded t)
  (org-src-fontify-natively t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-edit-src-content-indentation 0)
  (org-src-tab-acts-natively t)
  (org-hide-emphasis-markers nil)
  (org-hide-leading-stars nil)
  (org-indent-mode-turns-on-hiding-stars nil)
  (org-ctrl-k-protect-subtree t)
  (org-link-descriptive t)
  (system-time-locale "C")
  :config
  (define-key org-mode-map (kbd "<RET>") 'org-return-indent))

(defun local/org-hugo-export-to-md ()
  (interactive)
  (save-excursion
    (let ((cur_point (point)))
      (beginning-of-buffer)
      (org-hugo-export-to-md)
      (goto-char cur_point))))


(provide 'init-org)
