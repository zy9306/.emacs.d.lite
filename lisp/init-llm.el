;;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package gptel
  :commands (gptel gptel-send gptel-send-menu)
  :custom
  (gptel-model "gpt-4o")
  (gptel-stream nil)
  (gptel-crowdsourced-prompts-file (expand-file-name "llm_prompt/gptel-prompts.csv" user-emacs-directory))
  (gptel-api-key '(lambda () (getenv "OPENAI_API_KEY"))))


(defun local/gptel-programming ()
  (interactive)
  (let ((buffer-name "*ChatGPT: Programming*"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (gptel buffer-name gptel-api-key)
      )
    (with-current-buffer buffer-name
      (transient:gptel-system-prompt:Programming))
    ))

(defun local/gptel-chat ()
  (interactive)
  (let ((buffer-name "*ChatGPT: Chat*"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (gptel buffer-name gptel-api-key)
      )
    (with-current-buffer buffer-name
      (transient:gptel-system-prompt:Chat))
    ))


(provide 'init-llm)
