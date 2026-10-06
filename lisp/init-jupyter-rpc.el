;;; init-jupyter-rpc.el --- emacs-jupyter workarounds for tramp-rpc -*- lexical-binding: t -*-
;;; Commentary:
;;; Workarounds for running `jupyter-run-repl' from /rpc:HOST: buffers.
;;; Code:

;; 1. `jupyter-runtime-directory' first computes a *local* runtime directory
;; by running a local `jupyter --runtime-dir', even when the kernel is remote.
;; Without a local jupyter this fails with "Searching for program: jupyter".
;; Presetting the cache variable skips that lookup; remote buffers still ask
;; the remote host for its own runtime directory.
;; Status: confirmed on emacs-jupyter 20260813.  Remove when: a local `jupyter'
;; is always installed (the `when' below then does nothing anyway).
(when (and (memq system-type '(darwin gnu/linux))
           (not (executable-find "jupyter")))
  (setq jupyter-runtime-directory
        (file-name-as-directory
         (expand-file-name (if (eq system-type 'darwin)
                               "~/Library/Jupyter/runtime"
                             "~/.local/share/jupyter/runtime")))))

;; 2. For remote hosts `jupyter-session-with-random-ports' waits for the
;; connection file to vanish with a zero second `jupyter-with-timeout'.  With
;; tramp-rpc that timeout never fires, so if the file lingers Emacs spins at
;; 100% CPU forever.  Use a bounded wait and delete the file ourselves.
;; Status: written for the first hang (a 25 minute busy loop).  It is a copy of
;; upstream code and is probably redundant now that section 3 keeps timers
;; alive, but I have not tested without it.  Remove when: `jupyter-run-repl'
;; works from a /rpc: buffer with this advice deleted.  It may break if
;; upstream changes `jupyter-session-with-random-ports'.
(with-eval-after-load 'jupyter-env
  (require 'rx)
  (declare-function jupyter-new-uuid "jupyter-messages")

  (defun yz/jupyter-session-with-random-ports (orig-fn &rest args)
    "Around advice for `jupyter-session-with-random-ports' on remote hosts.
Call ORIG-FN with ARGS for local directories."
    (if (not (file-remote-p default-directory))
        (apply orig-fn args)
      (with-temp-buffer
        (let ((process (start-file-process
                        "jupyter-session-with-random-ports" (current-buffer)
                        (jupyter-locate-python) "-c"
                        "from jupyter_client.kernelapp import main; main()")))
          (set-process-query-on-exit-flag process nil)
          (unwind-protect
              (progn
                (jupyter-with-timeout
                    (nil jupyter-long-timeout
                         (error "`jupyter kernel' failed to show connection file path"))
                  (and (process-live-p process)
                       (goto-char (point-min))
                       (re-search-forward (rx "Connection file: "
                                              (group (+ any) ".json")
                                              (* whitespace) line-end)
                                          nil t)))
                (let* ((conn-file (concat (save-match-data
                                            (file-remote-p default-directory))
                                          (match-string 1)))
                       (conn-info (jupyter-read-connection conn-file))
                       (deadline (+ (float-time) 3)))
                  ;; Ask `jupyter kernel' to shut down the kernel it launched.
                  (interrupt-process process)
                  (while (and (process-live-p process)
                              (< (float-time) deadline))
                    (accept-process-output process 0.1))
                  (ignore-errors (delete-file conn-file))
                  (let ((new-key (jupyter-new-uuid)))
                    (plist-put conn-info :key new-key)
                    (jupyter-session :conn-info conn-info :key new-key))))
            (when (process-live-p process)
              (delete-process process)))))))

  (advice-add 'jupyter-session-with-random-ports :around
              #'yz/jupyter-session-with-random-ports))

;; 3. TRAMP wraps remote file operations in `with-tramp-suspended-timers',
;; which cancels every `with-timeout' timer and re-arms it inside a `let'
;; that binds `timer-list' to nil, so the re-armed timers are thrown away.
;; Any `jupyter-with-timeout' loop that touches a remote file (for example
;; waiting for a kernel to read its connection file) then never times out and
;; Emacs sits there forever.  Keep timers running while jupyter starts kernels.
;; Status: the timer mechanism was confirmed by an A/B test on Emacs 31.1.
;; Remove when: a newer Emacs/TRAMP fixes `with-tramp-suspended-timers', or
;; `jupyter-run-repl' works without this advice (unknown whether newer versions
;; do).  The remaining 10-13 s wait per launch is jupyter waiting out its
;; timeout, because tramp-rpc reports file times in whole seconds.
(defvar tramp-dont-suspend-timers)

(defun yz/jupyter-keep-timers (fn &rest args)
  "Call FN with ARGS without letting TRAMP drop `with-timeout' timers."
  (let ((tramp-dont-suspend-timers t))
    (apply fn args)))

(dolist (fn '(jupyter-run-repl jupyter-connect-repl jupyter--start-kernel-process))
  (advice-add fn :around #'yz/jupyter-keep-timers))

(provide 'init-jupyter-rpc)
;;; init-jupyter-rpc.el ends here
