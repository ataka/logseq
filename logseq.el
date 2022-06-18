;; Put the following code into .dir-locals.el in your top directory of Logseq.
;;
;; ((nil (eval
;;        (lambda ()
;;          (when (and (not (null buffer-file-name))
;;                     (string-match-p "\\.md\\'" buffer-file-name)
;;                     (not (string= major-mode "logseq-mode")))
;;            (logseq-mode))))))
;;


(define-derived-mode logseq-mode markdown-mode "Logseq"
  "Edit Logseq journals and pages with Markdown"
  ;; Use TAB char instead of space
  (setq indent-tabs-mode t
        tab-width 4))

;; (add-hook 'logseq-mode-hook '(lambda ()
;;                                 (setq indent-tabs-mode t)))

(defun logseq-make-temp-buffer ()
  (interactive)
  "Logseq 用の temporary buffer を作る"
  (switch-to-buffer " *Logseq*")
  (logseq-mode t))

;; Logseq Directory

(defvar logseq-home-dir "~/Library/Mobile Documents/iCloud~com~logseq~logseq/Documents/")
(defvar logseq-graph-alist nil)
(defvar logseq-working-graph nil)

(defun logseq-make-graph-alist (home-dir)
  "Make alist of (graph-name . full-path)"
  (mapcar (lambda (full-path)
            (cons (file-name-nondirectory full-path) full-path))
          (directory-files home-dir t "^[^.]")))

(defun logseq-init ()
  (setq logseq-graph-alist (logseq-make-graph-alist logseq-home-dir)
        legseq-working-graph (caar logseq-graph-alist)))

(defun logseq-switch-to-graph (graph)
  (interactive (list (completing-read "Graph: " logseq-graph-alist)))
  (setq logseq-working-graph graph))

(defun logseq-graph-dir (graph)
  (file-name-as-directory (cdr (assoc graph logseq-graph-alist))))

(defun logseq-asset-dir (graph)
  (file-name-as-directory (concat (logseq-graph-dir graph) "assets")))
(defun logseq-journal-dir (graph)
  (file-name-as-directory (concat (logseq-graph-dir graph) "journals")))
(defun logseq-page-dir (graph)
  (file-name-as-directory (concat (logseq-graph-dir graph) "pages")))

(defun logseq-list-asset-files (graph)
  (directory-files (logseq-asset-dir graph) nil "^[^.]"))
(defun logseq-list-journal-files (graph)
  (directory-files (logseq-journal-path graph) nil "^[^.]"))
(defun logseq-list-page-files (graph)
  (directory-files (logseq-page-dir graph) nil "^[^.]"))

(defun logseq-journal-file (graph journal)
  (concat (logseq-journal-dir graph) journal))
(defun logseq-page-file (graph page)
  (concat (logseq-page-dir graph) page))

(defun logseq-validate-file (file)
  file)

(defun logseq-find-file (file-name file-path)
  (when (or (file-exists-p file-path)
            (y-or-n-p (format "Create file? \"%s\"" file-name)))
    (find-file file-path)))

(defun logseq-find-journal (graph journal)
  (interactive (list logseq-working-graph
                     (completing-read "Journal: " (logseq-list-journal-files logseq-working-graph))))
  (logseq-find-file journal
                    (logseq-page-file graph (logseq-validate-file journal))))
(defun logseq-find-page (graph page)
  (interactive (list logseq-working-graph
                     (completing-read "Page: " (logseq-list-page-files logseq-working-graph))))
  (logseq-find-file page
                    (logseq-page-file graph (logseq-validate-file page))))

;; Local tests

;; (logseq-make-graph-alist logseq-home-dir)
;; (logseq-list-page-files logseq-working-graph)
;; (logseq-init)


(provide 'logseq)
