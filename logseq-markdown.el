;;; -*- lexical-binding: t -*-

;; Author: Masayuki Ataka <masayuki.ataka@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README.md file for details.


;;; Code:

(require 'seq)


;;; Constants =================================================================

(defconst logseq-markdown-mode-version "0.1"
  "Logseq markdown mode version number.")


;;; Global Variables ==========================================================

(defvar logseq-pages-result nil)


;;; Customizable Variables ====================================================

(defvar logseq-token nil)
(defvar logseq-directory "~/Logseq")
(defvar logseq-current-graph (expand-file-name "main" logseq-directory))


;;; Regular Expressions =======================================================

(defconst logseq-markdown-regex-header
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\t*- \\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]*#*\\)?\\)$"
  "Regexp identifying Markdown headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")

(defconst logseq-markdown-regex-header-atx
  "^\t*- \\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)$"
  "Regular expression for generic atx-style (hash mark) headers.")

(defconst logseq-markdown-regex-gfm-code-block-open
  "^[[:blank:]]*\\(- \\)?\\(?1:```\\)\\(?2:[[:blank:]]*{?[[:blank:]]*\\)\\(?3:[^`[:space:]]+?\\)?\\(?:[[:blank:]]+\\(?4:.+?\\)\\)?\\(?5:[[:blank:]]*}?[[:blank:]]*\\)$"
  "Regular expression matching opening of GFM code blocks.
Group 1 matches the opening three backquotes and any following whitespace.
Group 2 matches the opening brace (optional) and surrounding whitespace.
Group 3 matches the language identifier (optional).
Group 4 matches the info string (optional).
Group 5 matches the closing brace (optional), whitespace, and newline.
Groups need to agree with `markdown-regex-tilde-fence-begin'.")

(defconst logseq-markdown-regex-gfm-code-block-close
  "^[[:blank:]]*\\(?1:```\\)\\(?2:\\s *?\\)$"
  "Regular expression matching closing of GFM code blocks.
Group 1 matches the closing three backquotes.
Group 2 matches any whitespace and the final newline.")


;;; Syntax ====================================================================

(defvar logseq-markdown--syntax-properties
  (list 'markdown-tilde-fence-begin nil
        'markdown-tilde-fence-end nil
        'markdown-fenced-code nil
        'markdown-yaml-metadata-begin nil
        'markdown-yaml-metadata-end nil
        'markdown-yaml-metadata-section nil
        'markdown-gfm-block-begin nil
        'markdown-gfm-block-end nil
        'markdown-gfm-code nil
        'markdown-list-item nil
        'markdown-pre nil
        'markdown-blockquote nil
        'markdown-hr nil
        'markdown-comment nil
        'markdown-heading nil
        'markdown-heading-1-setext nil
        'markdown-heading-2-setext nil
        'markdown-heading-1-atx nil
        'markdown-heading-2-atx nil
        'markdown-heading-3-atx nil
        'markdown-heading-4-atx nil
        'markdown-heading-5-atx nil
        'markdown-heading-6-atx nil
        'markdown-metadata-key nil
        'markdown-metadata-value nil
        'markdown-metadata-markup nil)
  "Property list of all Logseq Markdown syntactic properties.")

(defvar logseq-markdown-literal-faces
  '(markdown-code-face
    markdown-inline-code-face
    markdown-pre-face
    markdown-math-face
    markdown-url-face
    markdown-plain-url-face
    markdown-language-keyword-face
    markdown-language-info-face
    markdown-metadata-key-face
    markdown-metadata-value-face
    markdown-html-entity-face
    markdown-html-tag-name-face
    markdown-html-tag-delimiter-face
    markdown-html-attr-name-face
    markdown-html-attr-value-face
    markdown-reference-face
    markdown-footnote-marker-face
    markdown-line-break-face
    markdown-comment-face)
  "A list of logseq-markdown-mode faces that contain literal text.
Literal text treats backslashes literally, rather than as an
escape character (see `markdown-match-escape').")

(defalias 'logseq-markdown-syntax-propertize-extend-region 'markdown-syntax-propertize-extend-region)

(defun logseq-markdown-font-lock-extend-region-function (start end _)
  "Used in `jit-lock-after-change-extend-region-functions'.
Delegates to `logseq-markdown-syntax-propertize-extend-region'. START
and END are the previous region to refontify."
  (let ((res (logseq-markdown-syntax-propertize-extend-region start end)))
    (when res
      ;; syntax-propertize-function is not called when character at
      ;; (point-max) is deleted, but font-lock-extend-region-functions
      ;; are called.  Force a syntax property update in that case.
      (when (= end (point-max))
        ;; This function is called in a buffer modification hook.
        ;; `markdown-syntax-propertize' doesn't save the match data,
        ;; so we have to do it here.
        (save-match-data
          (logseq-markdown-syntax-propertize (car res) (cdr res))))
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

(defun logseq-markdown-syntax-propertize-headings (start end)
  "Match headings of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward logseq-markdown-regex-header end t)
    (unless (markdown-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'markdown-heading
       (match-data t))
      (put-text-property
       (match-beginning 0) (match-end 0)
       (cond ((match-string-no-properties 2) 'markdown-heading-1-setext)
             ((match-string-no-properties 3) 'markdown-heading-2-setext)
             (t (let ((atx-level (length (markdown-trim-whitespace
                                          (match-string-no-properties 4)))))
                  (intern (format "markdown-heading-%d-atx" atx-level)))))
       (match-data t)))))

(defun logseq-markdown-syntax-propertize (start end)
  "Function used as `syntax-propertize-function'.
START and END delimit region to propertize."
  (with-silent-modifications
    (save-excursion
      (remove-text-properties start end logseq-markdown--syntax-properties)
      (markdown-syntax-propertize-fenced-block-constructs start end)
      (markdown-syntax-propertize-list-items start end)
      (markdown-syntax-propertize-pre-blocks start end)
      (markdown-syntax-propertize-blockquotes start end)
      (logseq-markdown-syntax-propertize-headings start end)
      (markdown-syntax-propertize-hrs start end)
      (markdown-syntax-propertize-comments start end))))

;;
;; Logseq HTTP Server
;;

(defmacro logseq--post-to-http-server (parameters callback)
  `(let ((url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")
                                      ("Authorization" . ,(format "Bearer %s" logseq-token))))
         (url-request-data ,(json-encode parameters)))
     (url-retrieve "http://127.0.0.1:12315/api" ,callback nil t)))

(defun logseq--callback-skip-header ()
  (search-forward "\n\n" nil t))

;;
;; Directory (Pages, Journals)
;;

(defun logseq--page-directory ()
  (expand-file-name "pages" logseq-current-graph))

(defun logseq--journal-directory ()
  (expand-file-name "journals" logseq-current-graph))

(defun logseq--page-directory-files ()
  (directory-files (logseq--page-directory) nil directory-files-no-dot-files-regexp))

;;
;; Pages
;;

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

(defun logseq--pages ()
  (seq-keep (lambda (page)
              (when (eq (plist-get page :journal?) :json-false)
                (plist-get page :originalName)))
            logseq-pages-result))

;; Page

(defun logseq--page-names ()
  (if logseq-pages-result
      (logseq--pages)
    (mapcar #'logseq--page-file-fullname-to-page-name
            (logseq--page-directory-files))))

(defun logseq--page-file-fullname-to-page-name (file-fullname)
  (logseq--convert-file-name-to-page-name
   (file-name-sans-extension file-fullname)))

(defun logseq--page-page-name-to-file-fullname (page-name)
  (file-name-with-extension
   (logseq--convert-page-name-to-file-name page-name) 
   "md"))

(defun logseq--convert-file-name-to-page-name (file-name)
  (let ((acc file-name))
    (dolist (replace '(("___" . "/") ("%3A" . ":")) acc)
      (setq acc (replace-regexp-in-string (car replace) (cdr replace) acc)))))

(defun logseq--convert-page-name-to-file-name (page-name)
  (let ((acc page-name))
    (dolist (replace '(("/" . "___") (":" . "%3A")) acc)
      (setq acc (replace-regexp-in-string (car replace) (cdr replace) acc)))))

(defun logseq--page-file-path (page-name)
  (expand-file-name
   (logseq--page-page-name-to-file-fullname page-name)
   (logseq--page-directory)))

;;
;; Move
;;

(defun logseq-follow-page-at-point (&optional arg)
  (interactive "P")
  (if (markdown-wiki-link-p)
      (logseq--follow-page (markdown-wiki-link-link) arg)
    (user-error "Point is not at a Wiki Link")))

(defun logseq--follow-page (page-name arg)
  (find-file (logseq--page-file-path page-name)))

;;
;; Edit
;;

(defun logseq-markdown-electric-open-blacket (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (when (looking-back "\\[\\[")
      (replace-match "")
      (call-interactively #'logseq-markdown--insert-page-link)))

(defun logseq-markdown--insert-page-link (page-name)
  (interactive (list (completing-read "Page: " (logseq--page-names))))
  (insert (format "[[%s]]" page-name)))

;;
;; Export
;;

(defun logseq-markdown-export (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (logseq-markdown-export-region beg end))

(defun logseq-markdown-export-region (beg end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (let ((page-property (logseq-markdown--parse-page-property))
            (ast (logseq-markdown--parse-body)))
        (switch-to-buffer-other-window " *Logseq-markdown*")
        (goto-char (point-min))
        (logseq-markdown--export-as-markdown ast)
        (logseq-markdown--remove-wiki-link)))))

(defun logseq-markdown--parse-page-property ()
  (let ((property '())
        (regex "\\(.+\\)::\s+\\(.+\\)"))
    (while (looking-at regex)
      (let ((key (match-string-no-properties 1))
            (value (match-string-no-properties 2)))
        (push (cons key value) property))
      (forward-line 1)
      (skip-chars-forward " \t\n"))
    property))

(defun logseq-markdown--parse-body ()
  (let ((acc '(:mode text :ast nil))
        (parsing t))
    (while parsing
      (setq acc (logseq-markdown--append-line-to-ast acc (logseq-markdown--scan-line)))
      (forward-line 1)
      (setq parsing (not (eobp))))
    (plist-get acc :ast)))

(defun logseq-markdown--scan-line ()
  (let ((regex "\\(\t*\\)\\(-\\)?\\( *\\)\\(.*\\)"))
    (looking-at regex)
    (let ((tabs (length (match-string-no-properties 1)))
          (prefix (string= "-" (match-string-no-properties 2)))
          (spaces (length (match-string-no-properties 3)))
          (text (match-string-no-properties 4)))
      `(:tabs ,tabs :hasPrefix ,prefix :spaces ,spaces :text ,text))))

(defun logseq-markdown--append-line-to-ast (acc line)
  (let ((mode (plist-get acc :mode))
        (ast (plist-get acc :ast)))
    (cond
     ((eq mode 'text)
      (cond
       ((logseq-markdown--parse-section line)
        (setq ast (cons `(:type section :depth ,(plist-get line :tabs) :text (,(plist-get line :text))) ast)))

       ((logseq-markdown--parse-src-begin line)
        (setq mode 'src
              ast (cons `(:type src :depth ,(plist-get line :tabs) :text (,(plist-get line :text))) ast)))

       ((logseq-markdown--parse-quote-begin line)
        (setq mode 'quote
              ast (cons `(:type quote :depth ,(plist-get line :tabs)) ast)))

       ((logseq-markdown--parse-table line)
        (let* ((type (plist-get (car ast) :type))
               (original-text (plist-get (car ast) :text))
               (text (plist-get line :text)))
          (if (eq type 'table)
              (setf (plist-get (car ast) :text)
                    (cons text original-text))
            (setq ast (cons `(:type table :depth ,(plist-get line :tabs) :text (,text)) ast)))))

       ((logseq-markdown--parse-text line)
        (setq ast (cons `(:type text :depth ,(plist-get line :tabs) :text (,(plist-get line :text))) ast)))

       ((logseq-markdown--parse-block-property line)
        (let* ((properties (plist-get (car ast) :properties))
               (text (plist-get line :text))
               (key (match-string-no-properties 1 text))
               (value (match-string-no-properties 2 text)))
          (setf (plist-get (car ast) :properties)
                (cons `(:key ,key :value ,value) properties))))

       ((logseq-markdown--parse-continuous-text line)
        (let* ((original-text (plist-get (car ast) :text))
               (text (plist-get line :text)))
          (setf (plist-get (car ast) :text)
                (cons text original-text))))
       ))

     ((eq mode 'src)
      (cond
       ((logseq-markdown--parse-src-end line)
        (setq mode 'text)
        (let* ((original-text (plist-get (car ast) :text))
               (text (plist-get line :text)))
          (setf (plist-get (car ast) :text)
                (cons text original-text))))

       (t
        (let* ((original-text (plist-get (car ast) :text))
               (spaces (make-string (- (plist-get line :spaces) 2) ? ))
               (text (concat spaces (plist-get line :text))))
          (setf (plist-get (car ast) :text)
                (cons text original-text))))
       ))

     ((eq mode 'quote)
      (cond
       ((logseq-markdown--parse-quote-end line)
        (setq mode 'text))

       (t
        (let* ((original-text (plist-get (car ast) :text))
               (text (plist-get line :text)))
          (setf (plist-get (car ast) :text)
                (cons text original-text))))
       ))
     )
    `(:mode ,mode :ast ,ast)))

(defun logseq-markdown--parse-section (line)
  (and (plist-get line :hasPrefix)
       (string-match-p "^#+ " (plist-get line :text))))

(defun logseq-markdown--parse-block-property (line)
  (and (not (plist-get line :hasPrefix))
       (= 2 (plist-get line :spaces))
       (string-match "^\\(.+\\)::\s+\\(.+\\)" (plist-get line :text))))

(defun logseq-markdown--parse-text (line)
  (plist-get line :hasPrefix))

(defun logseq-markdown--parse-continuous-text (line)
  (not (plist-get line :hasPrefix)))

(defun logseq-markdown--parse-src-begin (line)
  (string-match-p "^```[^`]*" (plist-get line :text)))

(defun logseq-markdown--parse-src-end (line)
  (string-match-p "^```$" (plist-get line :text)))

(defun logseq-markdown--parse-quote-begin (line)
  (string-match-p "^#\\+BEGIN_QUOTE" (plist-get line :text)))

(defun logseq-markdown--parse-quote-end (line)
  (string-match-p "^#\\+END_QUOTE" (plist-get line :text)))

(defun logseq-markdown--parse-table (line)
  (string-match-p "^|.+|" (plist-get line :text)))

;; export

(defun logseq-markdown--export-as-markdown (ast)
  (let (type
        (section-depth 0))
    (dolist (node (nreverse ast))
      (setq type (plist-get node :type))
      (cond
       ((eq type 'section)
        (setq section-depth (1+ (plist-get node :depth)))
        (dolist (line (nreverse (plist-get node :text)))
          (insert line "\n")))

       ((eq type 'text)
        (let* ((depth (max 0 (- (plist-get node :depth) section-depth)))
               (indent (make-string (* 2 depth) ? ))
               (count 0))
          (dolist (line (nreverse (plist-get node :text)))
            (if (= count 0)
                (if (seq-find (lambda (elm) (string= (plist-get elm :key) "logseq.order-list-type")) (plist-get node :properties))
                    (insert indent "1. " line "\n")
                  (insert indent "- " line "\n"))
              (insert indent "  " line "\n"))
            (setq count (1+ count)))))

       ((eq type 'src)
        (dolist (line (nreverse (plist-get node :text)))
          (insert line "\n")))

       ((eq type 'quote)
        (dolist (line (nreverse (plist-get node :text)))
          (insert "> " line "\n")))

       ((eq type 'table)
        (dolist (line (nreverse (plist-get node :text)))
          (insert line "\n")))

       ))))

;; misc

(defun logseq-markdown--remove-wiki-link ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[\\([^]]+\\)\\]\\]" nil t)
      (replace-match (match-string 1)))))

(defun logseq--remove-wiki-link-in-string (str)
  (replace-regexp-in-string "\\[\\[\\(.+\\)\\]\\]" "\\1" str))

;;
;; outline-minor-mode
;;

(setq logseq-markdown-outline-regexp "\t*- ")

(defun logseq-markdown-outline-level ()
  (1- (length (match-string-no-properties 0))))

;;
;; imenu
;;

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

(define-derived-mode logseq-markdown-mode markdown-mode "Logseq (MD)"
  "Edit Logseq journals and pages with Markdown"
  ;; Use TAB char instead of space
  (setq indent-tabs-mode t
        tab-width 4)
  ;; Syntax
  (add-hook 'syntax-propertize-extend-region-functions
            #'logseq-markdown-syntax-propertize-extend-region nil t)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'logseq-markdown-font-lock-extend-region-function t t)
  (setq-local syntax-propertize-function #'logseq-markdown-syntax-propertize)
  (syntax-propertize (point-max)) ;; Propertize before hooks run, etc.
  ;; ?
  (setq-local outline-regexp logseq-markdown-outline-regexp)
  (setq-local outline-level #'logseq-markdown-outline-level)
  (markdown-toggle-wiki-links t)
  ;; imenu
  (setq imenu-create-index-function #'logseq-markdown-imenu-create-nested-index))

;;
;; key map
;;

(let ((map logseq-markdown-mode-map))
  (define-key map "[" 'logseq-markdown-electric-open-blacket)
  (define-key map "\C-c\r"   'logseq-follow-page-at-point)
  (define-key map "\C-c\C-r" 'logseq-get-all-pages)
)

(provide 'logseq-markdown)
