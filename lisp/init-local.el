;;; init-local.el --- Configure personal packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; special setup on Windows64
;;; msys64 has been installed
(if (eq system-type 'windows-nt)
    ;;; ispell
    (setq ispell-program-name "C:\\msys64\\mingw64\\bin\\aspell")
  )
;;; I have issue of display for EmacsMac and Emacs WindowsNT
;;; override the vertical line for column indicator
(setq-default display-fill-column-indicator-character ?\u007c)

;;; anki-editor
(require 'init-anki)

;;; configure Chinese input
(require 'init-chinese)

(provide 'init-local)
;;; init-local.el ends here
