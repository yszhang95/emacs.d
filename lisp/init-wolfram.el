;;; init-wolfram.el --- run sci-wolfram Org blocks on a remote host, show results locally  -*- lexical-binding: t; -*-

;; Glue for https://github.com/TurbulenceChaos/sci-wolfram (its ob-wolfram.el), for the setup
;;   Mathematica kernel (licensed) on a remote host, Emacs + free Wolfram Player on the local Mac.
;;
;; What it does when `sci-wolfram-remote-directory' is set:
;;   1. the Wolfram REPL runs on the remote host (TRAMP `start-file-process');
;;   2. any `Get["/local/path/file.wl"]' in a block whose file exists locally is copied
;;      to the remote host and the path is rewritten (DisplayImage.wl lives on this machine);
;;   3. `[[file:/remote/path]]' links in results get the TRAMP prefix, so Org can
;;      display the remote PNGs inline (downloaded and cached by Org);
;;   4. `SciInteractive[expr]' (defined in the REPL) saves expr as a .nb on the remote
;;      host and prints a link; following it (C-c C-o) copies the file here and opens it
;;      in the local Wolfram Player.
;; With `sci-wolfram-remote-directory' nil everything below is inert.
;;
;; Requirements: `wolframscript' must be on the remote PATH *as TRAMP sees it*
;; (e.g. (add-to-list 'tramp-remote-path 'tramp-own-remote-path)).
;; Not supported: the CodeInspector syntax check (skipped when remote: it passes a
;; local temp file path to the remote kernel).
;;
;; Load with: (load "/path/to/sci-wolfram-remote.el") after ob-wolfram.

;;; Code:


(use-package sci-wolfram
  :vc (:url "https://github.com/TurbulenceChaos/sci-wolfram" :rev newst))

(require 'ob-wolfram)
(require 'tramp)
(require 'org)

(defgroup sci-wolfram-remote nil "Run sci-wolfram on a remote host." :group 'sci-wolfram)

(defcustom sci-wolfram-remote-directory nil
  "TRAMP directory in which the Wolfram REPL runs, e.g. \"/ssh:host:/tmp/\".
nil means run locally and disable this glue."
  :type '(choice (const :tag "Local (disabled)" nil) directory)
  :group 'sci-wolfram-remote)

(defcustom sci-wolfram-remote-player-app "Wolfram Player"
  "macOS application used to open .nb files (`open -a')."
  :type 'string :group 'sci-wolfram-remote)

(defcustom sci-wolfram-remote-notebook-cache
  (expand-file-name "sci-wolfram-notebooks/" temporary-file-directory)
  "Local directory where remote .nb files are copied before opening."
  :type 'directory :group 'sci-wolfram-remote)

(defun sci-wolfram-remote--prefix ()
  "TRAMP prefix of `sci-wolfram-remote-directory', e.g. \"/ssh:host:\"."
  (file-remote-p sci-wolfram-remote-directory))

;;;; 1. REPL on the remote host, and skip the (local-file based) syntax check

(define-advice ob-wolfram-make-repl (:around (fn &rest args) sci-wolfram-remote)
  (let ((default-directory (or sci-wolfram-remote-directory default-directory)))
    (apply fn args)))

(define-advice ob-wolfram-syntax-check (:around (fn body) sci-wolfram-remote)
  (if sci-wolfram-remote-directory
      (ob-wolfram-initiate-session)
    (funcall fn body)))

;;;; 2. Stage local .wl files on the remote host

(defvar sci-wolfram-remote--staged nil
  "Alist (LOCAL-FILE . MTIME) of scripts already copied to the remote host.")

(defun sci-wolfram-remote--stage (local)
  "Copy LOCAL to the remote directory (if changed); return its remote-local name."
  (let* ((dest (expand-file-name (file-name-nondirectory local) sci-wolfram-remote-directory))
         (mtime (file-attribute-modification-time (file-attributes local)))
         (done (assoc local sci-wolfram-remote--staged)))
    (unless (and done (equal (cdr done) mtime) (file-exists-p dest))
      (copy-file local dest t)
      (setf (alist-get local sci-wolfram-remote--staged nil nil #'equal) mtime))
    (file-local-name dest)))

(defun sci-wolfram-remote--rewrite-body (body)
  "Rewrite Get[\"/local/file.wl\"] in BODY to point at a staged remote copy."
  (if (not sci-wolfram-remote-directory)
      body
    (replace-regexp-in-string
     "Get\\[\"\\([^\"]+\\.wl\\)\"\\]"
     (lambda (m)
       (let ((path (save-match-data
                     (and (string-match "Get\\[\"\\([^\"]+\\)\"\\]" m) (match-string 1 m)))))
         (if (and path (not (file-remote-p path)) (file-exists-p path))
             (format "Get[\"%s\"]" (sci-wolfram-remote--stage path))
           m)))
     body t t)))

(define-advice ob-wolfram--evaluate-session (:filter-args (args) sci-wolfram-remote)
  (list (sci-wolfram-remote--rewrite-body (car args))))

(define-advice ob-wolfram-async-evaluate-session (:filter-args (args) sci-wolfram-remote)
  (list (sci-wolfram-remote--rewrite-body (car args))))

;;;; 3. Make remote file links resolvable from Emacs

(defun sci-wolfram-remote--fix-links (string)
  "Make remote file links in STRING usable from Emacs.
Absolute [[file:/x.png]] get the TRAMP prefix.  [[file:/x.nb]] becomes a
`wolfram-player:' link (Org opens *any* remote file in Emacs, ignoring `org-file-apps')."
  (if (and sci-wolfram-remote-directory (stringp string))
      (let ((prefix (sci-wolfram-remote--prefix)))
        (setq string (replace-regexp-in-string
                      "\\[\\[file:\\(/[^]\n]+\\.nb\\)\\]\\]"
                      (concat "[[wolfram-player:" prefix "\\1][open in Wolfram Player]]")
                      string))
        (replace-regexp-in-string "\\[\\[file:\\(/[^]\n]+\\)\\]\\]"
                                  (concat "[[file:" prefix "\\1]]")
                                  string))
    string))

(define-advice ob-wolfram--evaluate-session (:filter-return (res) sci-wolfram-remote-links)
  (sci-wolfram-remote--fix-links res))

(define-advice ob-wolfram-async-chunk-callback (:filter-return (res) sci-wolfram-remote-links)
  (sci-wolfram-remote--fix-links res))

(define-advice ob-wolfram-display-inline-images-in-babel-result (:around (fn &rest args) sci-wolfram-remote)
  (let ((org-display-remote-inline-images
         (if sci-wolfram-remote-directory 'cache org-display-remote-inline-images)))
    (apply fn args)))

;;;; 4. Interactive notebooks -> local Wolfram Player

(defconst sci-wolfram-remote--helpers
  "SciInteractive[expr_] := Module[{dir, nb},
dir = FileNameJoin[{Directory[], \"tmp\", \"wolfram\"}];
If[Not @ DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
nb = FileNameJoin[{dir, \"interactive-\" <> CreateUUID[] <> \".nb\"}];
Export[nb, Notebook[{Cell[BoxData @ ToBoxes @ expr, \"Output\"]}]];
WriteString[\"stdout\", \"[[file:\" <> nb <> \"]]\", \"\\n\"];
Null];
"
  "Wolfram code sent to a fresh remote REPL; defines SciInteractive[expr].")

(defvar sci-wolfram-remote--helper-process nil
  "REPL process that already received `sci-wolfram-remote--helpers'.")

(define-advice ob-wolfram-initiate-session (:after (&rest _) sci-wolfram-remote)
  (when sci-wolfram-remote-directory
    (let ((proc (get-buffer-process ob-wolfram-session)))
      (when (and proc (not (eq proc sci-wolfram-remote--helper-process)))
        (ob-wolfram--evaluate-session sci-wolfram-remote--helpers)
        (setq sci-wolfram-remote--helper-process proc)))))

(defun sci-wolfram-remote-open-notebook (file &optional _link)
  "Open the .nb FILE (local or TRAMP) in the local Wolfram Player."
  (let ((local (if (file-remote-p file)
                   (let ((dest (expand-file-name (file-name-nondirectory file)
                                                 sci-wolfram-remote-notebook-cache)))
                     (make-directory (file-name-directory dest) t)
                     (copy-file file dest t)
                     dest)
                 file)))
    (unless (eq system-type 'darwin)
      (user-error "sci-wolfram-remote-open-notebook: only implemented for macOS"))
    (call-process "open" nil 0 nil "-a" sci-wolfram-remote-player-app local)
    local))

;; [[wolfram-player:/ssh:host:/path/x.nb][...]] : C-c C-o / mouse-1 opens it in Wolfram Player.
(org-link-set-parameters
 "wolfram-player"
 :follow (lambda (path &optional _arg) (sci-wolfram-remote-open-notebook path)))

;; Local .nb files go through `org-file-apps'.  (Remote ones can't: Org's built-in
;; (remote . emacs) rule wins over the file-extension rules for every TRAMP path.)
(add-to-list 'org-file-apps '("\\.nb\\'" . sci-wolfram-remote-open-notebook))

(provide 'init-wolfram)
;;; sci-wolfram-remote.el ends here
