;;; anki-editor-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from anki-editor.el

(autoload 'anki-editor-mode "anki-editor" "\
A minor mode for making Anki cards with Org.

This is a minor mode.  If called interactively, toggle the
`anki-editor mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `anki-editor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "anki-editor" '("anki-editor-"))


;;; Generated autoloads from anki-editor-ui.el

(register-definition-prefixes "anki-editor-ui" '("anki-editor-ui-"))

;;; End of scraped data

(provide 'anki-editor-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; anki-editor-autoloads.el ends here
