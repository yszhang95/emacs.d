;;; init-cpp.el --- C++ -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(defun ys/cern-root-path ()
  "Return path or nil."
  (if (locate-file "root-config" exec-path)
      (shell-command-to-string
       "printf %s/root \"$(root-config --bindir)\"")
    (locate-file "root" exec-path)
    )
  )

(defun ys/cern-root-include-path ()
  "Return path or nil."
  (if (locate-file "root-config" exec-path)
      (shell-command-to-string
       "printf %s \"$(root-config --incdir)\"")
    (when(file-directory-p "/usr/include/root")
      "/usr/include/root"
      )
    )
  )

(add-hook 'c++-mode-hook
          (lambda ()
            (progn
              (when (ys/cern-root-include-path)
                (setq flycheck-gcc-include-path (list (ys/cern-root-include-path)) )
                (setq flycheck-clang-include-path (list (ys/cern-root-include-path)))
                )
              (setq flycheck-gcc-language-standard "c++17")
              (setq flycheck-clang-language-standard "c++17")
              (flymake-mode-off)
              (flycheck-mode-on-safe)
              )
            ))

(when (maybe-require-package 'cern-root-mode)
  (setq cern-root-filepath (ys/cern-root-path))
  )


(provide 'init-cpp)
;;; init-cpp.el ends here
