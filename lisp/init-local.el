;;; init-local.el --- Configure personal packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(progn
  (setq yz/my-home-dir (substitute-in-file-name "$HOME"))
  (when (eq system-type 'windows-nt)
    (setq yz/my-home-dir (replace-regexp-in-string "\\\\AppData.*" "" yz/my-home-dir))
    )
  )

;;; special setup on Windows64
;;; msys64 has been installed
(if (eq system-type 'windows-nt)
;;; ispell
    (progn
;;; ispell using msys64
      (setq ispell-program-name "C:\\msys64\\mingw64\\bin\\aspell")
;;; font face under resolution 1920x1080
;;; https://github.com/adobe-fonts/source-han-mono/releases
;;; https://www.emacswiki.org/emacs/ChangeFontsPermanentlyOnWindows
      (set-face-attribute 'default nil :family "Source Han Mono" :height 100)
      )
  )

;;; WSL setup
;;; I use Fedora and I ensure that the font installed
;;; sudo dnf install adobe-source-han-mono-fonts
(defun is-wsl ()
  "Check if it is WSL distributions via the environment variable PATH"
  ;; convert to t or nil
  (not (not
        (string-match "MicrosoftCorporationII.WindowsSubsystemForLinux"
                      (getenv "PATH")))))
(when (is-wsl)
  (set-face-attribute 'default nil :family "Source Han Mono" :height 120)
  )

;;; mac special face of font
(if (eq system-type 'darwin)
;;; https://osxdaily.com/2018/01/07/use-sf-mono-font-mac/
    (set-face-attribute 'default nil :font "SF Mono-12")
  )

;;; anki-editor
(require 'init-anki)

;;; configure Chinese input
;;; (require 'init-chinese)

;;; C++
;;; (require 'init-cpp)

;;; try this for remote git executable.
;;; However, magit suggest to edit `'tramp-remote-path`
;;; (setq magit-remote-git-executable "/cvmfs/sft.cern.ch/lcg/contrib/git/2.28.0/x86_64-centos7/bin/git")

;;; org GTD
(defun my-file-style-is-unix ()
  "Return true if the file style is Linux, bsd, macos style"
  (or (eq system-type 'darwin)
      (eq system-type 'gnu/linux)
      (eq system-type 'gnu/kfreebsd)
      (eq system-type 'cygwin)
      )
  )

;;; https://stackoverflow.com/a/3731037
(defun setup-org (my-pre-dir)
  (let ((default-directory my-pre-dir))
    (setq org-default-notes-file (expand-file-name "org/inbox.org"))
    (setq org-refile-targets (list `(,(expand-file-name "org/gtd.org") :maxlevel . 3)
                                   `(,(expand-file-name "org/someday.org") :level . 1)
                                   `(,(expand-file-name "org/tickler.org") :maxlevel . 2)))
    (setq org-agenda-files `(,(expand-file-name "org/inbox.org") ,(expand-file-name "org/gtd.org")))
    (setq org-directory (expand-file-name "org/"))
    (setq org-agenda-span 'week)
;;; overwrite the default value, (org-agenda-files)
    (setq org-mobile-files `(,(expand-file-name "org/inbox.org")
                             ,(expand-file-name "org/gtd.org")))
    (setq org-mobile-inbox-for-pull (expand-file-name "mobileorg/inbox.org"))
    (setq org-mobile-directory (expand-file-name "mobileorg/"))
    (with-eval-after-load 'projectile-mode-hook
      (add-to-list 'projectile-globally-ignored-directories
                   (list `(,(expand-file-name "org/") ,(expand-file-name "mobileorg/")))))))

(setup-org (concat yz/my-home-dir "/Koofr/"))

;;; A package that create table of contents
;;; https://github.com/alphapapa/org-make-toc
(maybe-require-package 'org-make-toc)

;;; Convert Org to twiki
(require-package 'ox-tiddly)
(require-package 'ox-twiki)

;;; python
;;; special for MacPorts
(when (eq system-type 'darwin)
  (setq treesit-extra-load-path nil))


;;; automatically enable eglot in C/C++, python
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)

(with-eval-after-load 'flymake-flycheck
  (with-eval-after-load 'flycheck
    ;; Disable flycheck backends in C/C++ after flymake-flycheck has activated
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(c/c++-clang c/c++-gcc c/c++-cppcheck)))))

(when (featurep 'init-treesitter)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure))

(setq c-block-comment-prefix "* ")

;; Enable Evil
(require-package 'evil)
;;; https://stackoverflow.com/a/56206909
(setq evil-disable-insert-state-bindings t)
(evil-mode 1)

(with-eval-after-load 'evil
  ;;; normal state
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "C-t") nil)
  ;;; motion state
  (define-key evil-motion-state-map (kbd "C-d") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-motion-state-map (kbd "C-y") nil)
  (define-key evil-motion-state-map (kbd "C-v") 'evil-scroll-down)
  (define-key evil-motion-state-map (kbd "M-v") 'evil-scroll-up)
  (define-key evil-motion-state-map (kbd ",") nil) ;; unbind existing command
  (evil-set-leader 'normal (kbd ","))  ; Set leader to Space
  (evil-define-key 'normal 'global (kbd "<leader>v") #'evil-visual-block)

  )


(defun my-org-fill-paragraph (&optional justify)
  "Custom `fill-paragraph` function for Org-mode.
   - Skips filling paragraphs containing multi-line LaTeX math (`\\[ ... \\]`).
   - Allows `M-q` to work normally on text and inline math (`\\(...\\)`)."
  (interactive "P")
  (let ((start (save-excursion (backward-paragraph) (point)))
        (end (save-excursion (forward-paragraph) (point)))
        block-math-found)
    (save-excursion
      (goto-char start)
      ;; Detect `\[ ... \]` blocks spanning multiple lines
      (while (re-search-forward "^\\\\\\[" end t)
        (if (re-search-forward "^\\\\\\]" end t)
            (setq block-math-found t))))

    (if block-math-found
        ;; Do nothing if multi-line block math is found, preserving formatting
        nil
      ;; Otherwise, fill the paragraph normally
      (org-fill-paragraph justify))))

(add-hook 'org-mode-hook
          (lambda ()
            (setq-local fill-paragraph-function #'my-org-fill-paragraph)))


;;; copied from https://github.com/tshu-w/.emacs.d/blob/9692c7d/lisp/editor-completion.el#L375
;;; upgrade eglot to latest version; changed eglot--uri-to-path to eglot-uri-to-path
;;; eglot
(use-package eglot
  :commands expand-absolute-name
  :init
  (setq read-process-output-max (* 1024 1024))
  ;; HOLD: https://github.com/joaotavora/eglot/issues/884
  :config
  (setq eglot-stay-out-of '(company)
        eglot-connect-timeout 10
        eglot-ignored-server-capabilities nil)

  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,(eglot-alternatives
                                                  '(("delance-langserver" "--stdio")
                                                    ("ruff" "server" "--preview")
                                                    "pylsp" "pyls" ("basedpyright-langserver" "--stdio")
                                                    ("pyright-langserver" "--stdio")
                                                    "jedi-language-server"))))

  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
        (if (and (eq nil flymake-no-changes-timeout)
                 (not (buffer-modified-p)))
            (flymake-start t)))))

  (when (fboundp #'tabnine-completion-at-point)
    (add-hook 'eglot-managed-mode-hook
              (defun eglot-capf ()
                (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
                (add-hook 'completion-at-point-functions
                          (cape-capf-super
                           #'eglot-completion-at-point
                           #'tabnine-completion-at-point) nil t))))

  (define-fringe-bitmap 'blank-fringe [0] 1 1 'center)
  (add-hook 'eglot-managed-mode-hook
            (defun inhibit-flymake-bitmap ()
              (put 'eglot-note 'flymake-bitmap '(blank-fringe))))

  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  ;; https://github.com/joaotavora/eglot/discussions/876
  ;; https://github.com/microsoft/pyright/issues/3282
  (defun eglot-uri-to-path@around (fun url)
    (let* ((uri (if (equal url "")
                    (project-root (eglot--project (eglot-current-server))) url))
           (path (funcall fun uri)))
      (if (file-directory-p path)
          (file-name-as-directory path)
        path)))
  (advice-add #'eglot-uri-to-path :around #'eglot-uri-to-path@around)
  )
;;; I added this line according to https://emacs.stackexchange.com/a/60823
(setq enable-remote-dir-locals t)

(defun gpt/eglot-print-server-command ()
  "Print the language server command used by Eglot. Generated by ChatGPT."
  (let ((server (eglot-current-server)))
    (if server
        (let ((proc (jsonrpc--process server)))
          (if (and proc (process-live-p proc))
              (let ((cmd (process-command proc)))
                (message "[Eglot] Server command: %s" (string-join cmd " ")))
            (message "[Eglot] Eglot server is not running.")))
      (message "[Eglot] No active Eglot server for this buffer."))))

(defun gpt/eglot-hook ()
  "Hook to run after Eglot starts. Generated by ChatGPT."
  (run-at-time "1 sec" nil #'gpt/eglot-print-server-command))

(add-hook 'eglot-managed-mode-hook #'gpt/eglot-hook)

(provide 'init-local)
;;; init-local.el ends here
