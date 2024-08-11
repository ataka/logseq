;;; -*- lexical-binding: t -*-
;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>
;;
;; Put the following code into .dir-locals.el in your top directory of Logseq.
;;
;; ((nil (eval
;;        (lambda ()
;;          (when (and (not (null buffer-file-name))
;;                     (string-match-p "\\.md\\'" buffer-file-name)
;;                     (not (string= major-mode "logseq-markdown-mode")))
;;            (logseq-markdown-mode))))))
;;

;;
;; Logseq HTTP Server
;;

(defvar logseq-token nil)

(defmacro logseq--post-to-http-server (parameters callback)
  `(let ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(format "Bearer %s" logseq-token))))
         (url-request-data ,(json-encode parameters)))
     (url-retrieve "http://127.0.0.1:12315/api" ,callback nil t)))

(defun logseq--callback-skip-header ()
  (search-forward "\n\n" nil t))

;;
;; Pages
;;

(defvar logseq-pages-result nil)

(defun logseq-get-all-pages ()
  (interactive)
  (logseq--post-to-http-server (("method" . "logseq.Editor.getAllPages"))
                               'logseq--get-all-pages-callback))

(defun logseq--get-all-pages-callback (status)
  (unless (plist-get status :error)
    (let ((json-object-type 'plist))
      (unwind-protect
          (let* ((raw-string (progn (logseq--callback-skip-header)
                                    (buffer-substring-no-properties (point) (point-max))))
                 (json-data (json-read-from-string (decode-coding-string raw-string 'utf-8))))
            (setq logseq-pages-result json-data))))))

;;
;; outline-minor-mode
;;

(setq logseq-markdown-outline-regexp "\t*- ")

(defun logseq-markdown-outline-level ()
  (1- (length (match-string-no-properties 0))))

;;
;; imenu
;;

(defconst logseq-markdown-regex-header
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\t*- \\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)\\)$"
  "Regexp identifying Markdown headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")

(defun logseq-markdown-imenu-create-nested-index ()
  "Create and return a nested imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((root '(nil . nil))
         (min-level 9999)
         hashes headers)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward logseq-markdown-regex-header (point-max) t)
        (unless (or (markdown-code-block-at-point-p)
                    (and (match-beginning 3)
                         (get-text-property (match-beginning 3) 'markdown-yaml-metadata-end)))
          (cond
           ((match-string-no-properties 2) ;; level 1 setext
            (setq min-level 1)
            (push (list :heading (match-string-no-properties 1)
                        :point (match-beginning 1)
                        :level 1) headers))
           ((match-string-no-properties 3) ;; level 2 setext
            (setq min-level (min min-level 2))
            (push (list :heading (match-string-no-properties 1)
                        :point (match-beginning 1)
                        :level (- 2 (1- min-level))) headers))
           ((setq hashes (markdown-trim-whitespace
                          (match-string-no-properties 4)))
            (setq min-level (min min-level (length hashes)))
            (push (list :heading (logseq--remove-wiki-link-in-string
                                  (match-string-no-properties 5))
                        :point (match-beginning 4)
                        :level (- (length hashes) (1- min-level))) headers)))))
      (cl-loop with cur-level = 0
               with cur-alist = nil
               with empty-heading = "-"
               with self-heading = "."
               for header in (reverse headers)
               for level = (plist-get header :level)
               do
               (let ((alist (list (cons (plist-get header :heading) (plist-get header :point)))))
                 (cond
                  ((= cur-level level)  ; new sibling
                   (setcdr cur-alist alist)
                   (setq cur-alist alist))
                  ((< cur-level level)  ; first child
                   (dotimes (_ (- level cur-level 1))
                     (setq alist (list (cons empty-heading alist))))
                   (if cur-alist
                       (let* ((parent (car cur-alist))
                              (self-pos (cdr parent)))
                         (setcdr parent (cons (cons self-heading self-pos) alist)))
                     (setcdr root alist)) ; primogenitor
                   (setq cur-alist alist)
                   (setq cur-level level))
                  (t                    ; new sibling of an ancestor
                   (let ((sibling-alist (last (cdr root))))
                     (dotimes (_ (1- level))
                       (setq sibling-alist (last (cdar sibling-alist))))
                     (setcdr sibling-alist alist)
                     (setq cur-alist alist))
                   (setq cur-level level)))))
      (setq root (copy-tree root))
      ;; Footnotes
      (let ((fn (markdown-get-defined-footnotes)))
        (if (or (zerop (length fn))
                (null markdown-add-footnotes-to-imenu))
            (cdr root)
          (nconc (cdr root) (list (cons "Footnotes" fn))))))))

;;
;; logseq-markdown-mode
;;

(define-derived-mode logseq-markdown-mode markdown-mode "Logseq"
  "Edit Logseq journals and pages with Markdown"
  ;; Use TAB char instead of space
  (setq indent-tabs-mode t
        tab-width 4)
  (setq-local outline-regexp logseq-markdown-outline-regexp)
  (setq-local outline-level #'logseq-markdown-outline-level)
  (markdown-toggle-wiki-links t)
  ;; imenu
  (setq imenu-create-index-function #'logseq-markdown-imenu-create-nested-index))

;;
;; key map
;;

(let ((map logseq-mode-map))
  (define-key map "\C-c\C-r" 'logseq-get-all-pages)
)

(provide 'logseq-markdown)
