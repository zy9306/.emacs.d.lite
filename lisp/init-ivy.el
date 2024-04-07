;;; -*- coding: utf-8; lexical-binding: t; -*-

(require-package 'ivy)
(require-package 'swiper)
(require-package 'counsel)
(require-package 'ivy-hydra)
(require-package 'wgrep)

(use-package ivy
  :hook (after-init . ivy-mode)
  :diminish (ivy-mode)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (ivy-height 10)
  (ivy-virtual-abbreviate 'abbreviate)
  (ivy-initial-inputs-alist nil)
  :bind
  (:map ivy-minibuffer-map
        ("C-t" . insert-time-string))
  :config
  (define-key ivy-switch-buffer-map (kbd "C-<tab>") 'next-line)
  (define-key ivy-switch-buffer-map (kbd "C-S-<tab>") 'previous-line)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-c C-o") 'ivy-occur))

(use-package counsel
  :hook (after-init . counsel-mode)
  :diminish (counsel-mode)
  :custom
  (counsel-search-engine 'google)
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c g") 'counsel-git)
  ;; press M-n to insert symbol at point
  (global-set-key (kbd "C-S-s") 'counsel-rg))

(use-package swiper
  :commands (swiper)
  :config
  (add-to-list 'ivy-height-alist '(swiper . 15))
  (global-set-key (kbd "C-s") 'swiper)
  (define-key swiper-map (kbd "C-r") 'previous-line))

;;;;;;;;;;;
;; utils ;;
;;;;;;;;;;;
(defun local/ivy-switch-buffer (regex-list)
  (let ((ivy-ignore-buffers regex-list))
    (ivy-switch-buffer)))

(defun local/ivy-switch-buffer-ignore-star-buffers ()
  (interactive)
  (local/ivy-switch-buffer (append ivy-ignore-buffers `("^\*"))))

(global-set-key (kbd "C-<tab>") 'local/ivy-switch-buffer-ignore-star-buffers)


(provide 'init-ivy)
