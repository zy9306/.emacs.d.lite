;;; -*- coding: utf-8; lexical-binding: t; -*-

;;;;;;;;;;;;;;;
;; constants ;;
;;;;;;;;;;;;;;;
(defconst *mac* (eq system-type 'darwin))
(defconst *win* (eq system-type 'windows-nt))
(defconst *cygwin* (eq system-type 'cygwin))
(defconst *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)))
(defconst *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)))

;;;;;;;;;;;;;;
;; encoding ;;
;;;;;;;;;;;;;;
                                        ; M-x revert-buffer-with-coding-system
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;
;; font size ;;
;;;;;;;;;;;;;;;
                                        ; https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/RobotoMono.zip
(setq font-size 14)
(setq my-font (format "RobotoMono Nerd Font %d" font-size))
(if (daemonp)
    (add-to-list 'default-frame-alist '(font . my-font)))
(when *linux*
  (set-face-attribute 'default nil :font (format "RobotoMono Nerd Font %d" font-size)))
(when *win*
  (set-face-attribute 'default nil :font (format "RobotoMono Nerd Font %d" font-size))
  (set-default 'process-coding-system-alist
               '(("[pP][lL][iI][nN][kK]" gbk-dos . gbk-dos)
                 ("[cC][mM][dD][pP][rR][oO][xX][yY]" gbk-dos . gbk-dos))))
(when *mac*
  (set-face-attribute 'default nil :font my-font))

;;;;;;;;;;;;;;
;; CJK Font ;;
;;;;;;;;;;;;;;
;; (when (display-graphic-p)
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font (frame-parameter nil 'font)
;;                       charset (font-spec :family "FZKai-Z03")))
;;   ;; C-u C-x = 查看当前选中的字符所用的字体
;;   (setq face-font-rescale-alist '(("FZKai-Z03" . 1.0))))

;;;;;;;;;
;; tab ;;
;;;;;;;;;
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(when (version<= "26.0.50" emacs-version)
  (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode))

(delete-selection-mode t)
(electric-pair-mode t)
(push '(?\' . ?\') electric-pair-pairs)
(push '(?\" . ?\") electric-pair-pairs)
(push '(?` . ?`) electric-pair-pairs)

(show-paren-mode t)
(column-number-mode t)

;;;;;;;;;
;; mac ;;
;;;;;;;;;
(when *mac*
  (let ((gls (executable-find "/usr/local/bin/gls")))
    (when gls (setq insert-directory-program "/usr/local/bin/gls")))
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

;;;;;;;;;;;;;
;; windows ;;
;;;;;;;;;;;;;
(when *win*
  ;; need git bash mingw environment
  ;; https://git-scm.com/download/win
  (setq ls-lisp-use-insert-directory-program t)
  (setq insert-directory-program "C:/Program Files/Git/usr/bin/ls.exe"))

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;
(setq dired-listing-switches "-al -h --group-directories-first --color=auto")

(set-default 'truncate-lines t)
(set-default 'message-truncate-lines t)

(setq-default confirm-kill-processes nil)

;;;;;;;;;;;;;;;
;; long line ;;
;;;;;;;;;;;;;;;
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t)
(setq long-line-threshold 1000)
(setq large-hscroll-threshold 1000)
(setq syntax-wholeline-max 1000)

(setq warning-suppress-types '((comp)))

;;;;;;;;;;;;
;; y-or-n ;;
;;;;;;;;;;;;
(setq original-y-or-n-p 'y-or-n-p)
(defalias 'original-y-or-n-p (symbol-function 'y-or-n-p))
(defun default-yes-sometimes (prompt)
  (if (or
       (string-match "has a running process" prompt)
       (string-match "does not exist; create" prompt)
       (string-match "modified; kill anyway" prompt)
       (string-match "Delete buffer using" prompt)
       (string-match "Kill buffer of" prompt)
       (string-match "still connected.  Kill it?" prompt)
       (string-match "Shutdown the client's kernel" prompt)
       (string-match "kill them and exit anyway" prompt)
       (string-match "Revert buffer from file" prompt)
       (string-match "Kill Dired buffer of" prompt)
       (string-match "delete buffer using" prompt)
       (string-match "Kill all pass entry" prompt))
      t
    (original-y-or-n-p prompt)))
(defalias 'yes-or-no-p 'default-yes-sometimes)
(defalias 'y-or-n-p 'default-yes-sometimes)

(defun local/modify-syntax-entry ()
  (modify-syntax-entry ?_ "w"))
(add-hook 'prog-mode-hook 'local/modify-syntax-entry)


(global-set-key (kbd "C-;") 'set-mark-command)


;;;;;;;;;;;;
;; backup ;;
;;;;;;;;;;;;
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq backup-directory-alist `((".*" . "~/.emacs.d/backups/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosaves/" t)))
(setq version-control t)
(setq vc-make-backup-files t)
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq delete-by-moving-to-trash t)
(setq kept-old-versions 0)
(setq kept-new-versions 20)
(setq create-lockfiles nil)

(defun local/backup-on-save ()
  (let ((buffer-backed-up nil))
    (if (<= (buffer-size) (* 1 1024 1024))  ;; 1 MB
        (progn
          (message "Made per save backup of %s." (buffer-name))
          (backup-buffer))
      (message "WARNING: File %s too large to backup." (buffer-name)))))

(add-hook 'before-save-hook 'local/backup-on-save)


;;;;;;;;;;;;;;;
;; custom.el ;;
;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;
(use-package nano-theme
  :load-path "site-lisp/nano-theme"
  :config
  (load-theme 'nano t)
  (nano-mode)
  ;; https://github.com/rougier/nano-theme/blob/master/nano-theme-support.el#L412
  (setq default-frame-alist
        (append (list
                 '(left . 300)
                 '(top . 100)
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 150)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 0)
                 '(right-fringe . 0)
                 '(undecorated-round . t)
                 '(scroll-bar-mode . -1)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0)))))


(provide 'init-config)
