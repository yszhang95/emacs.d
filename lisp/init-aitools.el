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

;; ;; Copilot
;; (require-package 'copilot)
;; (add-hook 'prog-mode-hook #'copilot-mode)
;; (add-hook 'cbase-mode-hook #'copilot-mode)
;; (add-hook 'python-base-mode-hook #'copilot-mode)
;; (add-hook 'python-mode-hook #'copilot-mode)
;; ;; ;; (setq copilot-network-proxy '(:host "127.0.0.1" :port 11435 :rejectUnauthorized :json-false))
;; ;; ;; (setq copilot-lsp-settings
;; ;; ;;       '(:http (:proxy "http://127.0.0.1:11435" :proxyStrictSSL :json-false)))
;; (with-eval-after-load 'copilot
;;   (define-key copilot-mode-map (kbd "C-<tab>" )
;;               #'copilot-accept-completion)
;;   (setq copilot-idle-delay 0.5
;;         copilot-indent-offset-warning-disable t)
;;   ;; (setq copilot-lsp-settings '(:github (:copilot (:selectedCompletionModel "o4-mini"))))
;;   (add-to-list 'copilot-major-mode-alist '("python-ts" . "python"))
;;   (add-to-list 'copilot-major-mode-alist '("jupyter-python" . "python")))

;; ;;; copilot-chat
;; ;;; does it only work under emacs 30?
;; ;; (require-package 'copilot-chat)
;; ;; (global-set-key (kbd "C-c C-y") #'copilot-chat-yank)
;; ;; (global-set-key (kbd "C-c M-y") #'copilot-chat-yank-pop)
;; ;; (global-set-key (kbd "C-c C-M-y") #'(lambda () (interactive) (copilot-chat-yank-pop -1)))


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
  ;; eat, not vterm: under a remote directory vterm ignores the program to run.
  (setq claude-code-terminal-backend 'eat
        ai-code-backends-infra-terminal-backend 'eat)
  ;; These filters drop height-only resizes, so eat and the pty disagree on rows
  ;; and Claude draws only part of the window after a vertical resize.
  (setq claude-code-optimize-window-resize nil
        ai-code-backends-infra-prevent-reflow-glitch nil)
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

;;; Remote Claude sessions

;; Emacs is only the terminal: Claude runs on the TRAMP host, and `claude --bg'
;; sessions live under its daemon, so they survive disconnects without tmux.
;; Use the eat backend; in a remote directory vterm ignores the program to run.
;;
;;   M-x my-claude-agents-attach          attach through claude-code.el
;;   M-x my-claude-agents-attach-ai-code  attach through ai-code

(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)

(defvar ai-code-claude-code-program)
(defvar ai-code-claude-code-program-switches)
(defvar ai-code-mcp-agent-enabled-backends)
(defvar claude-code--auto-mode-enabled)
(defvar claude-code-program)
(defvar claude-code-program-switches)
(defvar eat--t-term)
(defvar eat-terminal)
(declare-function ai-code-claude-code "ai-code-claude-code" (&optional arg))
(declare-function claude-code "claude-code" (&optional arg))
(declare-function eat--t-cur-x "eat" (cursor))
(declare-function eat--t-disp-cursor "eat" (display))
(declare-function eat--t-disp-width "eat" (display))
(declare-function eat--t-term-auto-margin "eat" (terminal))
(declare-function eat--t-term-display "eat" (terminal))
(declare-function eat-term-size "eat" (terminal))

(defgroup my-claude-agents nil
  "Attach to Claude Code background sessions."
  :group 'tools)

(defcustom my-claude-agents-program "claude"
  "Claude executable on the (possibly remote) host.
If TRAMP cannot find it on the remote PATH, set an absolute path."
  :type 'string)

(defcustom my-claude-agents-include-finished t
  "Non-nil passes --all, so completed sessions are listed too."
  :type 'boolean)

;;; Listing sessions

(defun my-claude-agents--root ()
  "Return the project root, or `default-directory'."
  (file-name-as-directory
   (expand-file-name
    (or (when-let* ((project (project-current))) (project-root project))
        default-directory))))

(defun my-claude-agents-list (&optional dir)
  "Return background sessions started under DIR, newest first.
Each element is an alist with keys such as id, name, state, cwd."
  (let* ((default-directory (or dir (my-claude-agents--root)))
         (local-dir (directory-file-name (file-local-name default-directory)))
         (args (append '("agents" "--json")
                       (when my-claude-agents-include-finished '("--all"))
                       (list "--cwd" local-dir))))
    (with-temp-buffer
      (let ((status (apply #'process-file my-claude-agents-program
                           nil t nil args)))
        (unless (eq status 0)
          (error "claude agents --json failed (exit %s): %s"
                 status (string-trim (buffer-string)))))
      ;; Skip any warning lines printed before the JSON.
      (goto-char (point-min))
      (unless (re-search-forward "^\\[" nil t)
        (error "No JSON array in output: %s" (string-trim (buffer-string))))
      (goto-char (match-beginning 0))
      (let ((sessions (json-parse-buffer :object-type 'alist
                                         :array-type 'list
                                         :null-object nil
                                         :false-object nil)))
        ;; Only background sessions have a short id; others cannot be attached.
        (sort (seq-filter (lambda (session) (alist-get 'id session)) sessions)
              (lambda (a b) (> (or (alist-get 'startedAt a) 0)
                               (or (alist-get 'startedAt b) 0))))))))

(defun my-claude-agents--label (session)
  "Return a display label for SESSION."
  (format "%-10s %-8s %s"
          (alist-get 'id session)
          (or (alist-get 'state session) "")
          (or (alist-get 'name session) "")))

(defun my-claude-agents--read-session (root)
  "Ask for a background session started under ROOT and return its id."
  (let* ((sessions (or (my-claude-agents-list root)
                       (user-error "No background sessions under %s" root)))
         (candidates (mapcar (lambda (session)
                               (cons (my-claude-agents--label session) session))
                             sessions))
         (choice (completing-read "Attach to session: " candidates nil t)))
    (alist-get 'id (cdr (assoc choice candidates)))))

;;; Attaching

;;;###autoload
(defun my-claude-agents-attach ()
  "Pick a Claude background session and attach to it with claude-code.el."
  (interactive)
  (require 'claude-code)
  (let* ((default-directory (my-claude-agents--root))
         (claude-code-program my-claude-agents-program)
         (claude-code-program-switches
          (list "attach" (my-claude-agents--read-session default-directory)))
         ;; `claude attach' takes only the session id.
         (claude-code--auto-mode-enabled nil))
    (claude-code)))

;;;###autoload
(defun my-claude-agents-attach-ai-code ()
  "Pick a Claude background session and attach to it with ai-code."
  (interactive)
  (require 'ai-code-claude-code)
  (let* ((default-directory (my-claude-agents--root))
         (ai-code-claude-code-program my-claude-agents-program)
         (ai-code-claude-code-program-switches
          (list "attach" (my-claude-agents--read-session default-directory)))
         ;; `claude attach' takes only the session id, not --mcp-config.
         (ai-code-mcp-agent-enabled-backends
          (remq 'claude-code ai-code-mcp-agent-enabled-backends)))
    (ai-code-claude-code)))

;;; ai-code on remote hosts

(defun my-claude-agents--skip-mcp-on-remote (orig backend working-dir argv)
  "Call ORIG with BACKEND, WORKING-DIR and ARGV unless WORKING-DIR is remote.
ai-code's MCP server listens on this machine and its Claude config is a local
temporary file, so a remote CLI could reach neither."
  (unless (file-remote-p working-dir)
    (funcall orig backend working-dir argv)))

(with-eval-after-load 'ai-code-mcp-agent
  (advice-add 'ai-code-mcp-agent-prepare-launch :around
              #'my-claude-agents--skip-mcp-on-remote))

;;; Wide characters at the right margin

;; `eat--t-write' wraps only once the cursor is past the last column.  With the
;; cursor on the last column a double-width character neither fits nor wraps, so
;; its write loop makes no progress and Emacs hangs.  Do what xterm does: pad the
;; last column and let the character wrap.  Remove once this is fixed upstream:
;; https://codeberg.org/akib/emacs-eat/issues/227

(defun my-claude-agents--write-around-margin (write str &optional beg end)
  "Call WRITE on STR from BEG to END, wrapping wide characters at the margin."
  (let* ((beg (or beg 0))
         (end (or end (length str)))
         (display (eat--t-term-display eat--t-term))
         (width (eat--t-disp-width display))
         (auto-margin (eat--t-term-auto-margin eat--t-term))
         (column (eat--t-cur-x (eat--t-disp-cursor display)))
         (start beg))
    (dotimes (offset (- end beg))
      (let* ((index (+ beg offset))
             (char-columns (char-width (aref str index))))
        (when (> char-columns 0)
          (when (> column width)
            (setq column (if auto-margin 1 width)))
          (when (> (+ column char-columns -1) width)
            (funcall write str start index)
            (setq start index)
            (if (and auto-margin (<= char-columns width))
                (progn
                  (funcall write (make-string (- (1+ width) column) ?\s))
                  (setq column 1))
              ;; It can never fit here, so drop it.
              (setq start (1+ index)
                    char-columns 0)))
          (setq column (+ column char-columns)))))
    (funcall write str start end)))

(with-eval-after-load 'eat
  (advice-add 'eat--t-write :around #'my-claude-agents--write-around-margin))

;;; Initial terminal size

;; `eat-exec' runs `stty rows R columns C' inside the new process, using the size
;; the terminal had before its buffer got a window, normally 80x24.  Emacs tends
;; to set the real size first, so stty overwrites it.  The width filters in
;; claude-code.el and ai-code have already recorded the real width by then, so
;; nothing corrects the pty until the width changes again.  stty runs before the
;; program does, so the first output means it is safe to set the size.

(defun my-claude-agents--sync-size-on-first-output (process)
  "Set the pty of PROCESS to its eat terminal size after its first output."
  ;; A flag, not `remove-function': ai-code wraps the filter after this runs.
  (let ((pending t))
    (add-function
     :after (process-filter process)
     (lambda (proc _output)
       (when pending
         (setq pending nil)
         (when-let* ((buffer (process-buffer proc))
                     ((buffer-live-p buffer)))
           (with-current-buffer buffer
             (when-let* (((bound-and-true-p eat-terminal))
                         (size (eat-term-size eat-terminal)))
               ;; The local pty of an ssh client may already have this size,
               ;; and setting it again would not make ssh send it.
               (when (file-remote-p default-directory)
                 (set-process-window-size proc (cdr size) (1+ (car size))))
               (set-process-window-size proc (cdr size) (car size))))))))))

(add-hook 'eat-exec-hook #'my-claude-agents--sync-size-on-first-output)

;;; Resizing with tramp-rpc 0.15

;; tramp-rpc 0.15 re-enters the advised terminal resize function through
;; `tramp-run-real-handler'.  The inner pass of claude-code.el's and ai-code's
;; width filters records the new width, so their outer pass sees no change and
;; returns nil, and the remote never gets the new size.  tramp-rpc 0.16 no longer
;; has this handler, so this section does nothing there; remove it once
;; upgraded.

(defun my-claude-agents--eat-adjust-once (handler process windows)
  "Call eat's original resize function for a direct SSH PROCESS.
Otherwise call HANDLER with PROCESS and WINDOWS."
  (if (and (processp process) (process-get process :tramp-rpc-direct-ssh))
      (funcall (advice--cd*r (symbol-function 'eat--adjust-process-window-size))
               process windows)
    (funcall handler process windows)))

(with-eval-after-load 'tramp-rpc-process
  (when (fboundp 'tramp-rpc-handle-eat--adjust-process-window-size)
    (advice-add 'tramp-rpc-handle-eat--adjust-process-window-size :around
                #'my-claude-agents--eat-adjust-once)))

(provide 'init-aitools)
;;; init-aitools.el ends here
