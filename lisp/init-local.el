;;; init-local.el --- Configure personal packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; special setup on Windows64
;;; msys64 has been installed
(if (eq system-type 'windows-nt)
    ;;; ispell
    (setq ispell-program-name "C:\\msys64\\mingw64\\bin\\aspell")
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

(provide 'init-local)
;;; init-local.el ends here
