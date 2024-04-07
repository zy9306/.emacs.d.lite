;;; -*- coding: utf-8; lexical-binding: t; -*-

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun query-replace-no-case ()
  (interactive)
  (let ((case-replace nil)
        (case-fold-search nil))
    (call-interactively 'anzu-query-replace)))

(defun rename-current-buffer-file ()
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;;;;;;;;;;;;;;;;;;;
;; revert-buffer ;;
;;;;;;;;;;;;;;;;;;;
(defun revert-buffer-no-confirm ()
  (interactive) (revert-buffer t t))

(defun revert-all-buffers ()
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t) )))
  (message "Refreshed open files.") )
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
(global-set-key (kbd "<C-f5>") 'revert-all-buffers)

;;;;;;;;;;;;;;
;; windmove ;;
;;;;;;;;;;;;;;
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)


;;;;;;;;;;;;;;;;
;; fit window ;;
;;;;;;;;;;;;;;;;
(defun local/fit-window-to-buffer-horizontally ()
  (interactive)
  (setq fit-window-to-buffer-horizontally 'only)
  (fit-window-to-buffer)
  (setq fit-window-to-buffer-horizontally nil))

(global-set-key (kbd "C-x w h") 'local/fit-window-to-buffer-horizontally)
(global-set-key (kbd "C-x w v") 'fit-window-to-buffer)


;;;;;;;;;;;;
;; vscode ;;
;;;;;;;;;;;;
(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.
URL `http://xahlee.info/emacs/emacs/emacs_open_in_vscode.html'

Version: 2020-02-13 2021-01-18 2022-08-04 2023-06-26"
  (interactive)
  (let ((xpath (if buffer-file-name buffer-file-name (expand-file-name default-directory))))
    (message "path is %s" xpath)
    (cond
     ((eq system-type 'darwin)
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument xpath))))
     ((eq system-type 'windows-nt)
      (shell-command (format "code.cmd %s" (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (shell-command (format "code %s" (shell-quote-argument xpath)))))))


;;;;;;;;;;;;;;;;;;;;;
;; show in desktop ;;
;;;;;;;;;;;;;;;;;;;;;
(defun xah-show-in-desktop ()
  "Show current file in desktop.
 (Mac Finder, Microsoft Windows File Explorer, Linux file manager)
This command can be called when in a file buffer or in `dired'.

URL `http://xahlee.info/emacs/emacs/emacs_show_in_desktop.html'
Version: 2020-11-20 2022-08-19 2023-06-26 2023-09-09"
  (interactive)
  (let ((xpath (if (eq major-mode 'dired-mode)
                   (if (eq nil (dired-get-marked-files))
                       default-directory
                     (car (dired-get-marked-files)))
                 (if buffer-file-name buffer-file-name default-directory))))
    (cond
     ((eq system-type 'windows-nt)
      (shell-command (format "PowerShell -Command invoke-item '%s'" (expand-file-name default-directory )))
      )
     ((eq system-type 'darwin)
      (shell-command
       (concat "open -R " (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (call-process shell-file-name nil 0 nil
                    shell-command-switch
                    (format "%s %s"
                            "xdg-open"
                            (file-name-directory xpath)))
      ))))

;;;;;;;;;;;;;;;
;; timestamp ;;
;;;;;;;;;;;;;;;
(defun insert-timestamp ()
  (interactive)
  (insert (format "%d" (truncate (float-time)))))

(defun insert-time-string ()
  (interactive)
  (insert (format-time-string "%y%m%d%H%M%S")))


(provide 'init-utils)
