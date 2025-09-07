;;; init-local.el --- Configure personal packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(global-set-key (kbd "C-x 0") 'switch-window-then-delete)

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
;; (require 'init-chinese)
;; a lot of things broken

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
                                   `(,(expand-file-name "org/notes.org") :level . 1)
                                   `(,(expand-file-name "org/tickler.org") :maxlevel . 2)))
    (setq org-agenda-files `(,(expand-file-name "org/inbox.org") ,(expand-file-name "org/gtd.org")))
    (setq org-directory (expand-file-name "org/"))
    (setq org-agenda-span 'week)
;;; overwrite the default value, (org-agenda-files)
    (setq org-mobile-files `(,(expand-file-name "org/inbox.org")
                             ,(expand-file-name "org/gtd.org")))
    (setq org-mobile-inbox-for-pull (expand-file-name "mobileorg/inbox.org"))
    (setq org-mobile-directory (expand-file-name "mobileorg/"))
    ;; Don’t show planning info for DONE items in the agenda
    (setq org-agenda-skip-deadline-if-done t
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-timestamp-if-done t)
    (with-eval-after-load 'projectile-mode-hook
      (add-to-list 'projectile-globally-ignored-directories
                   (list `(,(expand-file-name "org/") ,(expand-file-name "mobileorg/")))))))

;; (setup-org (concat yz/my-home-dir "/Koofr/"))
(setup-org yz/my-home-dir)


;;; disable annoying completion-at-point-functions
(setq-default completion-at-point-functions nil)


(use-package org-journal
  :ensure t
  :after org
  :custom
  (org-element-use-cache nil)
  (org-journal-dir (expand-file-name "org/journal/" yz/my-home-dir))
  (org-journal-file-type 'weekly))


;;; A package that create table of contents
;;; https://github.com/alphapapa/org-make-toc
(maybe-require-package 'org-make-toc)

;;; Convert Org to twiki
(require-package 'ox-tiddly)
(require-package 'ox-twiki)

;;; special for MacPorts
(when (eq system-type 'darwin)
  (setq treesit-extra-load-path nil))

;;; python
;;; https://github.com/emacs-jupyter/jupyter
;;; https://youtu.be/KHu5OnHc6V8?si=Y2K-XpH8jqZsMPi9
;;; https://github.com/jkitchin/scimax/blob/master/scimax-jupyter.el
;;; To build zmq, you need a emacs binary under $PATH.
;;; ln -s "/Applications/MacPorts/EmacsMac.app/Contents/MacOS/Emacs" ~/.local/bin/emacs
;;; or follow https://github.com/nnicandro/emacs-zmq/issues/47#issuecomment-2833930521
;;; https://raw.githubusercontent.com/jkitchin/ox-ipynb/refs/heads/master/ox-ipynb.el
;;; https://kitchingroup.cheme.cmu.edu/blog/2017/01/21/Exporting-org-mode-to-Jupyter-notebooks/
;;; https://martibosch.github.io/jupyter-emacs-universe/
;;; I am forced to use "C-c '" in org-mode to edit the jupyter source block,
;;; https://github.com/emacs-jupyter/jupyter/issues/488.
;;; https://github.com/emacs-jupyter/jupyter/issues/478#issuecomment-1676442126
(require-package 'jupyter)
(require-package 'code-cells)
(add-hook 'python-base-mode-hook 'code-cells-mode-maybe)
(require 'ox-ipynb)

;; Compute current Python interpreter (from PATH, direnv, uv, etc.)
;; https://github.com/python-lsp/python-lsp-server/issues/29#issuecomment-882161177
;; (setq-default eglot-workspace-configuration
;;               `(:pylsp
;;                 (:plugins
;;                  (:jedi (:environment ,(or (executable-find "python3")
;;                                            (executable-find "python"))))
;;                  ;; optional: mypy via pylsp-mypy
;;                  (:mypy (:enabled t :live_mode t :dmypy t)))))

(with-eval-after-load 'org
  (require 'ob-jupyter)
  (add-to-list 'org-babel-load-languages '(jupyter . t) t)
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images))

(setq org-babel-default-header-args:jupyter-python
      '((:results . "both")
        ;; This seems to lead to buffer specific sessions!
        (:session . (lambda () (buffer-file-name)))
        (:kernel . "python3")
        (:pandoc . "t")
        (:exports . "both")
        (:cache .   "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:eval . "never-export")))

;;; automatically enable eglot in C/C++, python
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(with-eval-after-load 'flymake-flycheck
  (with-eval-after-load 'flycheck
    ;; Disable flycheck backends in C/C++ after flymake-flycheck has activated
    (setq-default
     flycheck-disabled-checkers
     (append (default-value 'flycheck-disabled-checkers)
             '(c/c++-clang c/c++-gcc c/c++-cppcheck)))))

(require-package 'cape)
;; (require-package 'company)
;; (require-package 'company-ctags)
;; (company-ctags-auto-setup)
;; (add-to-list 'completion-at-point-functions
;;              (cape-company-to-capf #'company-ctags))

(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(require-package 'citre)
(require 'citre-config)

(when (featurep 'init-treesitter)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-ts-mode-hook 'eglot-ensure))

(setq c-block-comment-prefix "* ")

;; Enable Evil
(require-package 'evil)
;;; https://stackoverflow.com/a/56206909
(setq evil-disable-insert-state-bindings t)
(evil-mode 1)
(add-hook 'special-mode-hook #'evil-emacs-state)

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
  (evil-define-key 'normal 'global (kbd "<leader>v") #'evil-visual-block))


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
  ;; (setq eglot-stay-out-of '(company)
  ;;       eglot-connect-timeout 10
  ;;       eglot-ignored-server-capabilities nil)

  (add-to-list 'eglot-server-programs
               `((python-mode python-ts-mode) . ,(eglot-alternatives
                                                  '("pylsp"
                                                    ("basedpyright-langserver" "--stdio")
                                                    ("pyright-langserver" "--stdio")
                                                    "ruff-lsp"
                                                    "jedi-language-server"))))

  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot--uri-to-path uri))))
      (with-current-buffer buffer
        (if (and (eq nil flymake-no-changes-timeout)
                 (not (buffer-modified-p)))
            (flymake-start t)))))

  ;; (when (fboundp #'tabnine-completion-at-point)
  ;;   (add-hook 'eglot-managed-mode-hook
  ;;             (defun eglot-capf ()
  ;;               (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
  ;;               (add-hook 'completion-at-point-functions
  ;;                         (cape-capf-super
  ;;                          #'eglot-completion-at-point
  ;;                          #'tabnine-completion-at-point) nil t))))

  (define-fringe-bitmap 'blank-fringe [0] 1 1 'center)
  (add-hook 'eglot-managed-mode-hook
            (defun inhibit-flymake-bitmap ()
              (put 'eglot-note 'flymake-bitmap '(blank-fringe))))

  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun expand-absolute-name (name)
    (if (file-name-absolute-p name)
        (tramp-file-local-name
         (expand-file-name
          (concat (file-remote-p default-directory) name)))
      name))

  ;; https://github.com/joaotavora/eglot/discussions/876
  ;; https://github.com/microsoft/pyright/issues/3282
  (defun eglot--uri-to-path@around (fun url)
    (let* ((uri (if (equal url "")
                    (project-root (eglot--project (eglot-current-server))) url))
           (path (funcall fun uri)))
      (if (file-directory-p path)
          (file-name-as-directory path)
        path)))
  (advice-add #'eglot--uri-to-path :around #'eglot--uri-to-path@around)
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

;; TRAMP
(use-package tramp
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/scpx:yousen@dekstop:")
                     "remote-shell" "/bin/bash")))

(use-package cdlatex
  :ensure t)
(use-package evil-tex
  :ensure t)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode)
  (add-hook 'LaTeX-mode-hook #'evil-tex-mode)
  (setq prettify-symbols-unprettify-at-point t)
  (add-hook 'LaTeX-mode-hook
            (lambda ()
              (setq TeX-view-program-selection
                    '((output-pdf "PDF Tools"))
                    TeX-source-correlate-method 'synctex
                    TeX-view-program-list
                    '(("PDF Tools" TeX-pdf-tools-sync-view)))))
  (pdf-tools-install)
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode)))

;;; AI tools
(require 'init-aitools)

;;; miscellaneous
(require 'org-protocol)
(add-to-list 'org-capture-templates
             '("p" "Protocol" entry
               (file+headline "~/org/inbox.org" "Protocol")
               "* %:description\n\n%:link\n\n%:initial"))


(require-package 'pdf-tools)
;;; if not found epdfinfo, do (pdf-tools-install)
(when (eq system-type 'darwin)  ;; I work with MacPorts
  ;; Set PKG_CONFIG_PATH for MacPorts
  (setenv "PKG_CONFIG_PATH"
          (concat "/opt/local/lib/pkgconfig:"
                  (or (getenv "PKG_CONFIG_PATH") "")))
  ;; Install or rebuild pdf-tools
  (pdf-tools-install))
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))

(use-package org-noter
  :ensure t
  :demand t
  :config
  (setq org-noter-default-notes-file (expand-file-name "~/org/Notes/notes.org")
        org-noter-notes-search-path '("~/org/roam/"))
;;; https://github.com/org-noter/org-noter/issues/100#issuecomment-3161843750
;;; https://github.com/org-noter/org-noter?tab=readme-ov-file#customization
  (org-noter-enable-org-roam-integration)
 ;;; https://github.com/org-noter/org-noter/issues/100#issuecomment-3161843750
 ;;; (require 'org-macs)
  )

;;; tested on emacs-mac-app; not official GNU/EMacs
(require-package 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(with-eval-after-load 'org
  (org-download-enable)
  (when (eq system-type 'darwin)
    (define-key org-mode-map (kbd "C-y") #'gpt/org-smart-yank-macos)
    (setq org-download-screenshot-method "screencapture -i %s"))
  (define-key org-mode-map (kbd "C-M-y") #'org-download-clipboard))
(setq-default org-download-image-dir (expand-file-name "~/org/images"))
(setq-default org-download-heading-lvl 1)
;; (setq org-download-method 'attach)
(setq org-image-actual-width 600)

(defun gpt/org-smart-yank-macos ()
  "In Org-mode, paste clipboard image via `org-download-clipboard` if clipboard has image; else, yank text."
  (interactive)
  (if (and (derived-mode-p 'org-mode)
           (display-graphic-p)
           (gpt/image-in-clipboard-macos-p))
      (org-download-clipboard)
    (org-yank)))

(defun gpt/image-in-clipboard-macos-p ()
  "Return non-nil if clipboard contains image data (macOS only, requires pngpaste)."
  (executable-find "pngpaste") ;; ensures pngpaste is installed
  ;; (eq 0 (call-process "pngpaste" nil nil nil "-b" "org.openclipboard.png" "-"))
  (eq 0 (call-process "pngpaste" nil nil nil "-")))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-ui
  :ensure t
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/org/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-library-paths '("~/references/"))
  ;; (citar-bibliography org-cite-global-bibliography)
  (citar-org-roam-note-title-template "${author} - ${title}\npdf: ${file}")
  (citar-notes-paths '("~/org/roam/"))
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;; https://github.com/wasamasa/nov.el
;;; git clone https://depp.brause.cc/nov.el.git
;;; git clone https://github.com/tali713/esxml.git
(require 'esxml-query)
(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode . visual-line-mode))

;;; alternative to C-s C-o which invokes occur in isearch-mode
(define-key isearch-mode-map (kbd "C-l") #'consult-line)


;;; eat-mode
;;; https://codeberg.org/akib/emacs-eat/issues/167#issuecomment-2078683
;;; I used macports so the tic points to /opt/local/bin/tic.
;;; I should use /usr/bin/tic to avoid the error, under macos and emacs-mac-app.
(use-package eat
  :ensure t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'eat-mode 'emacs))
  (with-eval-after-load 'evil-escape
    (add-hook 'eat-mode-hook (lambda () (setq-local evil-escape-inhibit t)))))


(provide 'init-local)
;;; init-local.el ends here


;; (use-package org-ref
;;   :ensure t
;;   :config
;;   (setq bibtex-completion-bibliography '("~/org/references.bib")
;;         ;; bibtex-completion-notes-path "~/Documents/org/refs/astro"
;;         bibtex-completion-pdf-field "file"
;;         ;; bibtex-completion-pdf-opn-function
;;         ;; (lambda (fpath)
;;         ;;   (call-process "open" nil 0 nil fpath)))
;;         ))
;; (use-package org-roam-bibtex
;;   :ensure t
;;   :after org-roam
;;   :config
;;   (require 'org-ref))
