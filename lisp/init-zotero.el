;;; init-zotero.el --- Configure zotero -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  :config
  (setq citar-templates
        '((main . "${author editor:10%sn}  ${date year issued:4} ${title:48}")
          (suffix . "   ${=key= id:12}   ${=type=:12}   ${tags keywords:*}")
          (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . "Notes on ${author editor:%etal}, ${title}")))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require
  :config (citar-embark-mode))

;;; Consulted https://www.riccardopinosio.com/blog/posts/zotero_notes_article.html
;;; zotero:// handling for citar + org, and bib note syncing into org

(defun riccardo/zotero-url-p (s)
  (and (stringp s) (string-prefix-p "zotero://" s)))

(with-eval-after-load 'citar
  (defun riccardo/citar-file-open--zotero (orig file &rest args)
    (if (riccardo/zotero-url-p file)
        (browse-url file)
      (apply orig file args)))
  (advice-add 'citar-file-open :around #'riccardo/citar-file-open--zotero)

  (defun riccardo/citar-file--find-files-in-dirs--keep-zotero (orig files &rest args)
    (let* ((zot  (seq-filter #'riccardo/zotero-url-p files))
           (nonz (seq-remove #'riccardo/zotero-url-p files))
           (found (apply orig nonz args)))
      (append zot found)))
  (advice-add 'citar-file--find-files-in-dirs
              :around
              #'riccardo/citar-file--find-files-in-dirs--keep-zotero)

  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external)))


(with-eval-after-load 'org
  (org-link-set-parameters
   "zotero"
   :follow (lambda (zpath)
             (browse-url (format "zotero:%s" zpath)))))

;; (setq org-mouse-1-follows-link t)

(with-eval-after-load 'org
  (org-link-set-parameters
   "zotero"
   :follow (lambda (zpath)
             (browse-url (format "zotero:%s" zpath)))))

(defun riccardo/extract-citekey ()
  (save-excursion
    (goto-char (point-min))
    (cond
     ((re-search-forward "^[ \t]*:ROAM_REFS:[ \t]*@\\([^]\n\t ]+\\)" nil t)
      (match-string 1))
     ((re-search-forward "\\bcite:@\\([^]\n\t )]+\\)" nil t)
      (match-string 1))
     ((re-search-forward "\\B@\\([[:alnum:]][[:alnum:]:._-]*\\)\\b" nil t)
      (match-string 1))
     (t nil))))

(defun riccardo/grab-reference-items (citekey item-list)
  (with-temp-buffer
    (mapc (lambda (reference-file)
            (when (and (stringp reference-file) (file-readable-p reference-file))
              (insert-file-contents reference-file)))
          citar-bibliography)
    (goto-char (point-min))
    (let* ((ck-pos (search-forward (concat "{" citekey ",") nil t))
           (entry-beg (when ck-pos (line-beginning-position)))
           (entry-end (when ck-pos
                        (or (and (re-search-forward "\n@" nil t) (match-beginning 0))
                            (point-max)))))
      (mapcar
       (lambda (item-name)
         (if (and entry-beg entry-end)
             (save-excursion
               (goto-char entry-beg)
               (let* ((pat (format "^[ \t]*%s[ \t]*=[ \t]*\\({\\|\"\\)"
                                   (regexp-quote (format "%s" item-name))))
                      (hit (re-search-forward pat entry-end t)))
                 (if hit
                     (condition-case _
                         (let* ((delim (char-after (match-beginning 1))) ; ?{ or ?"
                                (beg (1+ (match-beginning 1))))        ; skip { or "
                           (goto-char beg)
                           (goto-char (match-beginning 1))
                           (forward-sexp 1)
                           (let* ((end (1- (point))) ; drop } or "
                                  (raw (buffer-substring-no-properties beg end)))
                             (string-trim raw)))
                       (error ""))
                   "")))
           ""))
       item-list))))

(defun riccardo/parse-annotation-par (s)
  (when (and (stringp s) (string-match ".*(\\(.*\\),\\(.*\\),.*p\\.[[:space:]]*\\([0-9]+\\))\\(.*\\)" s))
    (list
     :author (match-string 1 s)
     :year   (match-string 2 s)
     :page   (match-string 3 s)
     :content (match-string 4 s)
     :quote  (let ((m (string-match ".*``\\(.*\\)''" s)))
               (when m (match-string 1 s))))))

(defun riccardo/parse-annotation-components (section url)
  (let* ((m (and (stringp section)
                 (string-match ".*(\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\),[[:space:]]*\\(.+\\))" section)))
         (year  (when m (match-string 3 section)))
         (month (when m (match-string 2 section)))
         (day   (when m (match-string 1 section)))
         (ts    (when m (match-string 4 section)))
         (base-url (if (stringp url) (replace-regexp-in-string "\\?.*" "" url) ""))
         (parts (split-string (or section "") "\\\\par[ \t\n]+" t))
         (pars (delq nil (mapcar #'riccardo/parse-annotation-par parts)))
         (body
          (mapconcat
           (lambda (el)
             (let* ((quote (plist-get el :quote))
                    (page  (plist-get el :page))
                    (content (plist-get el :content))
                    (purl (if (and page (string-match-p "\\`[0-9]+\\'" page))
                              (concat base-url (format "?p%s" page))
                            base-url)))
               (concat
                (format "*** [[%s][note p%s]]\n" purl (or page ""))
                (when quote
                  (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" quote))
                (when content content))))
           pars "\n")))
    (format "** (%s-%s-%s), %s:\n%s"
            (or year "") (or month "") (or day "") (or ts "") body)))

(defun riccardo/parse-zotero-notes (notes url)
  (let* ((sections (split-string (or notes "") "\\\\section" t))
         (sections (seq-filter (lambda (x) (string-match-p "Annotation" x)) sections))
         (parsed (mapcar (lambda (x) (riccardo/parse-annotation-components x url)) sections)))
    (mapconcat #'identity parsed "\n")))

(defun riccardo/create-zotero-notes (notes file-url)
  (let* ((text (riccardo/parse-zotero-notes notes file-url))
         (data (org-element-parse-buffer 'greater-elements))
         (bounds
          (org-element-map data 'headline
            (lambda (el)
              (when (string= "zotero notes" (org-element-property :raw-value el))
                (list (org-element-property :begin el)
                      (org-element-property :end el))))
            nil t)))
    (when (and bounds (nth 0 bounds) (nth 1 bounds))
      (delete-region (nth 0 bounds) (nth 1 bounds)))
    (save-excursion
      (goto-char (point-max))
      (insert "* zotero notes\n" text "\n"))))

(defun riccardo/update-zotero-notes ()
  (interactive)
  (when (get-buffer-window)
    (let ((citekey (riccardo/extract-citekey)))
      (when citekey
        (let* ((items (riccardo/grab-reference-items citekey '(note file)))
               (note (nth 0 items))
               (file (nth 1 items)))
          (when (and (stringp note) (> (length note) 0))
            (riccardo/create-zotero-notes note file)))))))

(add-hook 'find-file-hook #'riccardo/update-zotero-notes)
(add-hook 'after-save-hook #'riccardo/update-zotero-notes)


(provide 'init-zotero)
;;; init-zotero.el ends here
