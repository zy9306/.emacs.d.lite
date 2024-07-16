;;; -*- coding: utf-8; lexical-binding: t; -*-

(push (expand-file-name "lisp" user-emacs-directory) load-path)

(setq warning-minimum-level :emergency)

(require 'init-elpa)
(require 'init-config)
(require 'init-useful)
(require 'init-ivy)
(require 'init-utils)
(require 'init-mode)
(require 'init-org)
(require 'init-platform)
(require 'init-llm)

(server-start)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fs"
                     (float-time (time-subtract (current-time) before-init-time)))))
