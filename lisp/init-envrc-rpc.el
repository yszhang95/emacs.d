;;; init-envrc-rpc.el --- envrc compatibility for tramp-rpc -*- lexical-binding: t -*-

(require 'envrc)
(require 'tramp-rpc)

(defun yz/envrc-rpc-export (original &rest args)
  "Export RPC environments synchronously until envrc supports remote processes.
Keep direnv's allow checks and envrc's buffer-local environment handling."
  (if (not (equal (file-remote-p default-directory 'method) "rpc"))
      (apply original args)
    ;; RPC must not preload direnv, otherwise its export contains no diff.
    (let ((tramp-rpc-use-direnv nil)
          (tramp-remote-process-environment
           (default-value 'tramp-remote-process-environment))
          (envrc--remote-path nil)
          (inhibit-read-only t))
      (setq envrc--direnv-global-process-environment
            (default-value 'process-environment))
      (envrc--direnv-set-status 'running)
      (condition-case err
          (pcase (envrc--direnv-allowed-status-code)
            ('nil (envrc--direnv-set-status 'none))
            ((or 1 2) (envrc--direnv-set-status 'denied))
            (0
             (let ((stderr-file (make-temp-file "envrc-rpc-"))
                   output exit-code)
               (unwind-protect
                   (progn
                     (setq output
                           (with-temp-buffer
                             (setq exit-code
                                   (process-file envrc-direnv-executable
                                                 nil (list t stderr-file) nil
                                                 "export" "json"))
                             (buffer-string)))
                     (erase-buffer)
                     (insert-file-contents stderr-file)
                     (if (zerop exit-code)
                         (let ((json-key-type 'string))
                           (setq envrc--direnv-result
                                 (unless (string-empty-p output)
                                   (json-read-from-string output)))
                           (envrc--direnv-set-status 'success))
                       (envrc--direnv-set-status 'error)))
                 (delete-file stderr-file))))
            (_ (error "Unexpected remote direnv status")))
        (error
         (insert (error-message-string err) "\n")
         (envrc--direnv-set-status 'error))))))

(defun yz/envrc-rpc-exec-path (original &rest args)
  "Let remote executable discovery use the current buffer's direnv PATH."
  (or (and (bound-and-true-p envrc-mode)
           (eq envrc--status 'on)
           envrc--remote-path)
      (apply original args)))

(advice-add 'envrc--direnv-export :around #'yz/envrc-rpc-export)
(advice-add 'tramp-rpc-handle-exec-path :around #'yz/envrc-rpc-exec-path)
(add-to-list 'envrc-supported-tramp-methods "rpc")
(setq envrc-remote t)

(provide 'init-envrc-rpc)
