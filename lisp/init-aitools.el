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
  (setq
   gptel-model 'gemini-2.5-flash-lite
   gptel-backend (gptel-make-gemini "Gemini"
                   :key gptel-api-key
                   :stream t))
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (gptel-make-gh-copilot "Copilot")
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '(llama3.1:8b)))

(require 'gptel)
(require 'org)
(require 'cl-lib)

(defgroup my-gptel-polish nil
  "Polish text in Org with gptel and insert an AI-foldable reference block."
  :group 'gptel)

(defcustom my-gptel-polish-system
  (mapconcat
   #'identity
   '("You are an expert English editor."
     "Polish non-native English to be fluent and natural while preserving meaning."
     "Do not change technical terms or add new facts."
     "Keep paragraphing similar to the original (do not over-segment)."
     ""
     "IMPORTANT: Output MUST follow the exact delimiter format below."
     "Do NOT use Markdown headings, code fences, or any extra text."
     ""
     "<<<A"
     "(Version A: minimal edits, safe and faithful)"
     "A>>>"
     "<<<B"
     "(Version B: more native, more concise)"
     "B>>>"
     "<<<N"
     "(Notes: max 8 short bullets; focus on grammar issues, unnatural phrasing, and key changes)"
     "N>>>")
   "\n")
  "System prompt used for polishing."
  :type 'string)

(defcustom my-gptel-polish-drawer-name "AI_BLOCK"
  "Name of the Org drawer used to store foldable AI reference content."
  :type 'string)

(defun my-gptel--region-or-paragraph ()
  "Return (BEG END TEXT). Prefer active region; otherwise use current paragraph."
  (if (use-region-p)
      (list (region-beginning) (region-end)
            (buffer-substring-no-properties (region-beginning) (region-end)))
    (save-excursion
      (let (beg end)
        (backward-paragraph)
        (setq beg (point))
        (forward-paragraph)
        (setq end (point))
        (list beg end (string-trim (buffer-substring-no-properties beg end)))))))

(defun my-gptel--extract (label response)
  "Extract section LABEL from RESPONSE.

Preferred format:
<<<A ... A>>>, <<<B ... B>>>, <<<N ... N>>>

Fallback format:
## Version A / ## Version B / ## Notes (also works with Org headings)."
  (when (stringp response)
    (let* ((case-fold-search nil)
           (start-re (format "^<<<%s\\s-*$" (regexp-quote label)))
           (end-re   (format "^%s>>>\\s-*$" (regexp-quote label))))
      (save-match-data
        (cond
         ;; 1) Delimiter blocks: find lines <<<X ... X>>>
         ((and (string-match start-re response)
               (let* ((content-beg (match-end 0))
                      (content-beg (if (and (< content-beg (length response))
                                            (eq (aref response content-beg) ?\n))
                                       (1+ content-beg)
                                     content-beg)))
                 (when (string-match end-re response content-beg)
                   (string-trim (substring response content-beg (match-beginning 0)))))))

         ;; 2) Fallback: Markdown/Org headings
         (t
          (let* ((title (pcase label
                          ("A" "Version A")
                          ("B" "Version B")
                          ("N" "Notes")))
                 (hdr-re (format
                          "^\\(?:##+\\|\\*+\\)\\s-*%s\\b[^\n]*\n\\([\\s\\S]*?\\)\\(?:\n\\(?:##+\\|\\*+\\)\\s-*\\(?:Version A\\|Version B\\|Notes\\)\\b\\|\\'\\)"
                          (regexp-quote title))))
            (when (string-match hdr-re response)
              (string-trim (match-string 1 response))))))))))

(defun my-gptel--make-drawer (drawer-name original b notes &optional raw)
  "Build the foldable Org drawer content. Optionally include RAW response."
  (let ((dn drawer-name))
    (concat
     (format ":%s:\n" dn)
     "IGNORE_FOR_AI: t\n"
     "IGNORE_FOR_SUMMARY: t\n"
     "KIND: polish\n\n"
     "Original\n"
     "#+begin_example\n"
     (string-trim original) "\n"
     "#+end_example\n\n"
     "Version B (alternative)\n"
     "#+begin_src text\n"
     (string-trim (or b "")) "\n"
     "#+end_src\n\n"
     "Notes\n"
     (string-trim (or notes "")) "\n\n"
     (when (and raw (not (string-empty-p (string-trim raw))))
       (concat
        "- RAW_MODEL_OUTPUT (debug)\n"
        "#+begin_example\n"
        (string-trim raw) "\n"
        "#+end_example\n\n"))
     ":END:\n")))

(defun my-gptel--fold-ai-block-at (pos)
  "Hide drawers and blocks in the vicinity of POS."
  (save-excursion
    (goto-char pos)
    (when (derived-mode-p 'org-mode)
      (ignore-errors (org-fold-hide-drawer-all))
      (ignore-errors (org-fold-hide-blocks)))))

;;;###autoload
(defun my-gptel-polish-org-insert ()
  "Polish region/paragraph and replace it with Version A.
Insert Original + Version B + Notes into a foldable Org drawer below."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command is intended for Org buffers"))
  (pcase-let* ((`(,beg ,end ,text) (my-gptel--region-or-paragraph)))
    (when (string-empty-p (string-trim text))
      (user-error "No text found in region/paragraph"))
    (let ((origin-buffer (current-buffer))
          (origin-beg (copy-marker beg))
          (origin-end (copy-marker end t)))
      (gptel-request
          (format
           "You MUST follow the exact delimiter format below (no extra text):\n\n<<<A\n...\nA>>>\n<<<B\n...\nB>>>\n<<<N\n...\nN>>>\n\nPolish this text:\n\n%s"
           text)
        :system my-gptel-polish-system
        :callback
        (lambda (response _info)
          (with-current-buffer origin-buffer
            (let* ((resp (if (stringp response) response ""))
                   (a (my-gptel--extract "A" resp))
                   (b (my-gptel--extract "B" resp))
                   (n (my-gptel--extract "N" resp)))
              (save-excursion
                (goto-char origin-beg)
                (delete-region origin-beg origin-end)
                (cond
                 ((and a (not (string-empty-p a)))
                  (insert (string-trim a) "\n")
                  (let ((drawer-pos (point)))
                    (insert (my-gptel--make-drawer my-gptel-polish-drawer-name text b n))
                    (my-gptel--fold-ai-block-at drawer-pos))
                  (message "Polish inserted (Version A visible; references folded)."))
                 (t
                  (insert (string-trim text) "\n")
                  (let ((drawer-pos (point)))
                    (insert (my-gptel--make-drawer my-gptel-polish-drawer-name
                                                   text (or b "") (or n "")
                                                   resp))
                    (my-gptel--fold-ai-block-at drawer-pos))
                  (message "Polish completed, but parsing failed (RAW output saved in drawer).")))
                (deactivate-mark)))))))))


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
(add-hook 'python-mode-hook #'copilot-mode)
;; ;; (setq copilot-network-proxy '(:host "127.0.0.1" :port 11435 :rejectUnauthorized :json-false))
;; ;; (setq copilot-lsp-settings
;; ;;       '(:http (:proxy "http://127.0.0.1:11435" :proxyStrictSSL :json-false)))
(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "C-<tab>" )
              #'copilot-accept-completion)
  (setq copilot-idle-delay 0.5
        copilot-indent-offset-warning-disable t)
  ;; (setq copilot-lsp-settings '(:github (:copilot (:selectedCompletionModel "o4-mini"))))
  (add-to-list 'copilot-major-mode-alist '("python-ts" . "python"))
  (add-to-list 'copilot-major-mode-alist '("jupyter-python" . "python")))

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
(use-package ai-code
  :vc (:url "https://github.com/tninja/ai-code-interface.el")
  :config
  (ai-code-set-backend  'codex) ;; use claude-code-ide as backend
  (setq claude-code-terminal-backend 'vterm)
  ;; Enable global keybinding for the main menu
  (global-set-key (kbd "C-c C-a") #'ai-code-menu)
  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el.git"))
(use-package shell-maker
  :vc (:url "https://github.com/xenodium/shell-maker.git"))
(use-package agent-shell
  :ensure t)


(provide 'init-aitools)
;;; init-aitools.el ends here
