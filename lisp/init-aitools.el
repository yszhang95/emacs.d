;;; init-aitools.el --- Configure personal ai tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; chatgpt
(require-package 'gptel)
(use-package gptel
  :ensure t
  :hook
  ((gptel-mode . (lambda ()
                   (setq-local truncate-lines nil)
                   (visual-line-mode 1)
                   (display-line-numbers-mode 1))))
  :init
  (setq gptel-default-mode #'org-mode)
  (setq gptel-model 'gpt-4.1)
  (setq gptel-backend (gptel-make-gh-copilot "Copilot"))
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(gpt-oss:20b,
              llama3.1:8b
              codegemma:7b-instruct
              mistral:7b-instruct
              gemma3:latest
              gemma3:4b
              gemma3:12b
              gemma3n:e4b-it-q4_K_M
              nous-hermes2:10.7b-solar-q4_0)))

;;; aidermacs
;; (require-package 'aidermacs)
;; (setq aidermacs-default-chat-mode "ollama_chat/gemma3:4b")
;; (setq aidermacs-default-model "ollama_chat/gemma3:12b")
;; (setq aidermacs-architect-model "ollama_chat/gemma3:12b")

;;; https://github.com/Aider-AI/aider/issues/2227
;;; This solution works by upgrading litellm to 1.74.
;;; https://github.com/MatthewZMD/aidermacs/issues/127
;;; not working as of 2025-07-30
;; (setq aidermacs-default-chat-mode "github_copilot/gpt-4o")
;; (setq aidermacs-default-model "github_copilot/gpt-4.1")
;; (setq aidermacs-architect-model "github_copilot/gpt-4o")
;; (setq aidermacs-default-chat-mode 'architect)
;;; (global-set-key (kbd "C-c C-a") #'aidermacs-transient-menu)

;; Copilot
(require-package 'copilot)
(add-hook 'prog-mode-hook #'copilot-mode)
(add-hook 'cbase-mode-hook #'copilot-mode)
(add-hook 'python-base-mode-hook #'copilot-mode)
;; ;; (setq copilot-network-proxy '(:host "127.0.0.1" :port 11435 :rejectUnauthorized :json-false))
;; ;; (setq copilot-lsp-settings
;; ;;       '(:http (:proxy "http://127.0.0.1:11435" :proxyStrictSSL :json-false)))
(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "C-<tab>" )
              #'copilot-accept-completion)
  (setq copilot-idle-delay 0.5
        copilot-indent-offset-warning-disable t)
  ;; (setq copilot-lsp-settings '(:github (:copilot (:selectedCompletionModel "o4-mini"))))
  )

;;; copilot-chat
;;; does it only work under emacs 30?
;; (require-package 'copilot-chat)
;; (global-set-key (kbd "C-c C-y") #'copilot-chat-yank)
;; (global-set-key (kbd "C-c M-y") #'copilot-chat-yank-pop)
;; (global-set-key (kbd "C-c C-M-y") #'(lambda () (interactive) (copilot-chat-yank-pop -1)))


;;; In the future, I want to try:
;;; - https://github.com/h-ohsaki/chatgpt-el
;;; - https://github.com/stevemolitor/claude-code.el
;;; - https://github.com/cpoile/claudemacs

(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest))
(use-package ai-code-interface
  :vc (:url "https://github.com/tninja/ai-code-interface.el")
  :config
  (ai-code-set-backend  'codex) ;; use claude-code-ide as backend
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c C-a") #'ai-code-menu)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(provide 'init-aitools)
;;; init-aitools.el ends here
