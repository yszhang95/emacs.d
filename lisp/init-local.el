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
