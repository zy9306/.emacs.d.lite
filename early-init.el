;;; -*- coding: utf-8; lexical-binding: t; -*-

(setq native-comp-jit-compilation nil)
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Suppress GUI features ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq indicate-empty-lines t)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
