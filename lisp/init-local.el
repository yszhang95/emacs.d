;;; init-local.el --- Configure personal packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
(require 'init-chinese)

;;; C++
(require 'init-cpp)

;;; try this for remote git executable.
;;; However, magit suggest to edit `'tramp-remote-path`
(setq magit-remote-git-executable "/cvmfs/sft.cern.ch/lcg/contrib/git/2.28.0/x86_64-centos7/bin/git")

;;; org GTD
(defun my-file-style-is-unix ()
  "Return true if the file style is Linux, bsd, macos style"
  (or (eq system-type 'darwin)
      (eq system-type 'gnu/linux)
      (eq system-type 'gnu/kfreebsd)
      (eq system-type 'cygwin)
      )
  )

(if (my-file-style-is-unix)
    (progn
      (setq org-refile-targets '(("~/Dropbox/org/gtd/gtd.org" :maxlevel . 3)
                                 ("~/Dropbox/org/gtd/someday.org" :level . 1)
                                 ("~/Dropbox/org/gtd/tickler.org" :maxlevel . 2)))
      (setq org-default-notes-file "~/Dropbox/org/gtd/inbox.org")
      (setq org-agenda-files '("~/Dropbox/org/gtd/inbox.org" "~/Dropbox/org/gtd/gtd.org") )
      )
  )

(if (eq system-type 'windows-nt)
    (progn
      (setq org-refile-targets '(("c:/Users/yousen/Dropbox/org/gtd/gtd.org" :maxlevel . 3)
                                 ("c:/Users/yousen/Dropbox/org/gtd/someday.org" :level . 1)
                                 ("c:/Users/yousen//Dropbox/org/gtd/tickler.org" :maxlevel . 2)))
      (setq org-default-notes-file "c:/Users/yousen/Dropbox/org/gtd/inbox.org")
      (setq org-agenda-files '("c:/Users/yousen/Dropbox/org/gtd/inbox.org"
                               "c:/Users/yousen/Dropbox/org/gtd/gtd.org") )
      )
  )

;;; A package that create table of contents
;;; https://github.com/alphapapa/org-make-toc
(maybe-require-package 'org-make-toc)

;;; Convert Org to twiki
(require-package 'ox-tiddly)
(require-package 'ox-twiki)

(provide 'init-local)
;;; init-local.el ends here
