;;; init-anki.el --- anki setup -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; copy from https://yiufung.net/post/anki-org/
(require-package 'use-package)
(require-package 'sqlite3)

(use-package anki-editor
  :after org
  :commands anki-editor-mode
  :bind (:map org-mode-map
              ("<f12>" . anki-editor-cloze-region-auto-incr)
              ("<f11>" . anki-editor-cloze-region-dont-incr)
              ("<f10>" . anki-editor-reset-cloze-number)
              ("<f9>"  . anki-editor-push-note-at-point))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

(use-package ankiorg
  :commands
  ankiorg-pull-notes
  ankiorg-buffer-get-media-files
  ankiorg-pull-tags
  :custom
  (ankiorg-sql-database (concat yz/my-home-dir "/Koofr/anki/collection.anki2"))
  (ankiorg-media-directory (concat yz/my-home-dir "/Koofr/anki/img")))

;; Org-capture templates
(setq org-my-anki-file (concat yz/my-home-dir "/Koofr/org/anki.org"))

(add-to-list 'org-capture-templates
             '("a" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%Y-%m-%d %H:%M:%S>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
             '("A" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%Y-%m-%d %H:%M:%S>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))

(provide 'init-anki)
;;; init-anki.el ends here
