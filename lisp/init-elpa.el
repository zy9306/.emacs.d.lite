;;; -*- coding: utf-8; lexical-binding: t; -*-

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)

;; (setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
;;                          ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(defun require-package (package &optional min-version no-refresh)
  (cond
   ((package-installed-p package min-version)
    t)
   ((or (assoc package package-archive-contents) no-refresh)
    (package-install package))
   (t
    (package-refresh-contents)
    (require-package package min-version t))))


(provide 'init-elpa)
