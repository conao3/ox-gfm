;;; ox-gfm.el --- Github Flavored Markdown Back-End for Org Export Engine

;; Copyright (C) 2014-2017 Lars Tveito

;; Author: Lars Tveito
;; Version: 1.0
;; Keywords: org, wp, markdown, github
;; Package-Requires: ((emacs "26.1") (org "9.0"))
;; URL: https://github.com/larstvei/ox-gfm

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

;; This library implements a Markdown back-end (github flavor) for Org
;; exporter, based on the `md' back-end.

;;; Code:

(require 'cl-lib)
(require 'ox-md)
(require 'ox-publish)

;;; User-Configurable Variables

(defgroup org-export-gfm nil
  "Options specific to Markdown export back-end."
  :tag "Org Github Flavored Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))


;;; Define Back-End

(org-export-define-derived-backend 'gfm 'md
  :menu-entry
  '(?g "Export to Github Flavored Markdown"
       ((?G "To temporary buffer"
            (lambda (a s v b) (org-gfm-export-as-markdown a s v)))
        (?g "To file" (lambda (a s v b) (org-gfm-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-gfm-export-to-markdown t s v)
                (org-open-file (org-gfm-export-to-markdown nil s v)))))))
  :translate-alist '((headline . org-gfm-headline)
                     (inner-template . org-gfm-inner-template)
                     (paragraph . org-gfm-paragraph)
                     (strike-through . org-gfm-strike-through)
                     (src-block . org-gfm-src-block)
                     (table-cell . org-gfm-table-cell)
                     (table-row . org-gfm-table-row)
                     (table . org-gfm-table)
                     (template . org-gfm-template))
  ;; KEY KEYWORD OPTION DEFAULT BEHAVIOR
  :options-alist '((:last-modified "LAST_MODIFIED" nil (format-time-string "<%Y-%m-%d %a>"))
                   (:gfm-headline-offset "GFM_HEADLINE_OFFSET" nil org-gfm-headline-offset)
                   (:gfm-layout "GFM_LAYOUT" nil org-gfm-layout)
                   (:gfm-category "GFM_CATEGORY" nil org-gfm-category)
                   (:gfm-tags "GFM_TAGS" nil org-gfm-tags)
                   (:gfm-preamble "GFM_PREAMBLE" nil org-gfm-preamble)
                   (:gfm-postamble "GFM_POSTAMBLE" nil org-gfm-postamble)
                   (:gfm-custom-front-matter "GFM_CUSTOM_FRONT_MATTER" nil nil space)))


;;; Functions

(defcustom org-gfm-date-format "%Y-%m-%d"
  "Date format for `org-gfm--create-date-string'."
  :group 'org-export-gfm
  :type 'string)

(defcustom org-gfm-layout "post"
  "Default layout for GFM frontmatter."
  :group 'org-export-gfm
  :type 'string)

(defcustom org-gfm-preamble ""
  "Default header."
  :group 'org-export-gfm
  :type 'string)

(defcustom org-gfm-postamble "\
<!--
This file is generated from org file.
Please edit that org source instead of this file.

;; Local Variables:
;; buffer-read-only: t
;; End:
-->"
  "Default footer."
  :group 'org-export-gfm
  :type 'string)

(defcustom org-gfm-category ""
  "Default gfm category."
  :group 'org-export-gfm
  :type 'string)

(defcustom org-gfm-tags ""
  "Default gfm tags devided by spaces."
  :group 'org-export-gfm
  :type 'string)

(defun org-gfm--parse-property-arguments (str)
  "Return an alist converted from a string STR of Hugo property value.

STR is of type \":KEY1 VALUE1 :KEY2 VALUE2 ..\".  Given that, the
returned value is ((KEY1 . VALUE1) (KEY2 . VALUE2) ..).

Example: Input STR \":foo bar :baz 1 :zoo \\\"two words\\\"\" would
convert to ((foo . \"bar\") (baz . 1) (zoo . \"two words\"))."
  (mapcar
   (lambda (elm)
     `(,(intern (substring (symbol-name (car elm)) 1)) . ,(cdr elm)))
   (org-babel-parse-header-arguments str)))



;;; Transcode Functions

;;;; Headline

(defcustom org-gfm-headline-offset 0
  "Headline offset."
  :group 'org-export-gfm
  :type 'integer)

(defun org-gfm-headline (headline contents info)
  "Make HEADLINE string.
CONTENTS is the headline contents.
INFO is a plist used as a communication channel."
  (cl-flet ((parsenum (str) (if (not (stringp str))
                                0
                              (string-to-number str))))
    (let* ((num (parsenum (plist-get info :gfm-headline-offset)))
           (info (if (= 0 num)
                     info
                   (plist-put info :headline-offset num))))
      (org-md-headline headline contents info))))

;;;; Paragraph

(defun org-gfm-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Github Flavoured Markdown format.
CONTENTS is the paragraph contents.  INFO is a plist used as a
communication channel."
  (unless (plist-get info :preserve-breaks)
    (setq contents (concat (mapconcat 'identity (split-string contents) " ") "\n")))
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
        (replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Src Block

(defun org-gfm-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown format.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))


;;;; Strike-Through

(defun org-gfm-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH from Org to Markdown (GFM).
CONTENTS is the text with strike-through markup.  INFO is a plist
holding contextual information."
  (format "~~%s~~" contents))


;;;; Table-Common

(defvar org-gfm-width-cookies nil)
(defvar org-gfm-width-cookies-table nil)

(defconst org-gfm-table-left-border "|")
(defconst org-gfm-table-right-border " |")
(defconst org-gfm-table-separator " |")

(defun org-gfm-table-col-width (table column info)
  "Return width of TABLE at given COLUMN.
INFO is a plist used as communication channel.
Width of a column is determined either by inquerying `org-gfm-width-cookies'
in the column, or by the maximum cell with in the column."
  (let ((cookie (when (hash-table-p org-gfm-width-cookies)
                  (gethash column org-gfm-width-cookies))))
    (if (and (eq table org-gfm-width-cookies-table)
             (not (eq nil cookie)))
        cookie
      (progn
        (unless (and (eq table org-gfm-width-cookies-table)
                     (hash-table-p org-gfm-width-cookies))
          (setq org-gfm-width-cookies (make-hash-table))
          (setq org-gfm-width-cookies-table table))
        (let ((max-width 0)
              (specialp (org-export-table-has-special-column-p table)))
          (org-element-map
              table
              'table-row
            (lambda (row)
              (setq max-width
                    (max (length
                          (org-export-data
                           (org-element-contents
                            (elt (if specialp (car (org-element-contents row))
                                   (org-element-contents row))
                                 column))
                           info))
                         max-width)))
            info)
          (puthash column max-width org-gfm-width-cookies))))))


(defun org-gfm-make-hline-builder (table info char)
  "Return a function to build horizontal line in TABLE with given CHAR.
INFO is a plist used as a communication channel."
  `(lambda (col)
     (let ((max-width (max 3 (org-gfm-table-col-width table col info))))
       (when (< max-width 1)
         (setq max-width 1))
       (make-string max-width ,char))))


;;;; Table-Cell

(defun org-gfm-table-cell (table-cell contents info)
  "Transcode TABLE-CELL element from Org into GFM.
CONTENTS is content of the cell.
INFO is a plist used as a communication channel."
  (let* ((table (org-export-get-parent-table table-cell))
         (column (cdr (org-export-table-cell-address table-cell info)))
         (width (org-gfm-table-col-width table column info))
         (left-border (if (org-export-table-cell-starts-colgroup-p table-cell info) "| " " "))
         (right-border " |")
         (data (or contents "")))
    (setq contents
          (concat data
                  (make-string (max 0 (- width (string-width data)))
                               ?\s)))
    (concat left-border contents right-border)))


;;;; Table-Row

(defun org-gfm-table-row (table-row contents info)
  "Transcode TABLE-ROW element from Org into GFM.
CONTENTS is cell contents of TABLE-ROW.
INFO is a plist used as a communication channel."
  (let ((table (org-export-get-parent-table table-row)))
    (when (and (eq 'rule (org-element-property :type table-row))
               ;; In GFM, rule is valid only at second row.
               (eq 1 (cl-position
                      table-row
                      (org-element-map table 'table-row 'identity info))))
      (let* ((table (org-export-get-parent-table table-row))
             (header-p (org-export-table-row-starts-header-p table-row info))
             (build-rule (org-gfm-make-hline-builder table info ?-))
             (cols (cdr (org-export-table-dimensions table info))))
        (setq contents
              (concat org-gfm-table-left-border
                      (mapconcat (lambda (col) (funcall build-rule col))
                                 (number-sequence 0 (- cols 1))
                                 org-gfm-table-separator)
                      org-gfm-table-right-border))))
    contents))



;;;; Table

(defun org-gfm-table (table contents info)
  "Transcode TABLE element into Github Flavored Markdown table.
CONTENTS is the contents of the table.
INFO is a plist holding contextual information."
  (let* ((rows (org-element-map table 'table-row 'identity info))
         (no-header (or (<= (length rows) 1)))
         (cols (cdr (org-export-table-dimensions table info)))
         (build-dummy-header
          (function
           (lambda ()
             (let ((build-empty-cell (org-gfm-make-hline-builder table info ?\s))
                   (build-rule (org-gfm-make-hline-builder table info ?-))
                   (columns (number-sequence 0 (- cols 1))))
               (concat org-gfm-table-left-border
                       (mapconcat (lambda (col) (funcall build-empty-cell col))
                                  columns
                                  org-gfm-table-separator)
                       org-gfm-table-right-border "\n" org-gfm-table-left-border
                       (mapconcat (lambda (col) (funcall build-rule col))
                                  columns
                                  org-gfm-table-separator)
                       org-gfm-table-right-border "\n"))))))
    (concat (when no-header (funcall build-dummy-header))
            (replace-regexp-in-string "\n\n" "\n" contents))))


;;;; Table of contents

(defun org-gfm-format-toc (headline)
  "Return an appropriate table of contents entry for HEADLINE.
INFO is a plist used as a communication channel."
  (let* ((title (org-export-data
                 (org-export-get-alt-title headline info) info))
         (level (1- (org-element-property :level headline)))
         (indent (concat (make-string (* level 2) ? )))
         (anchor (or (org-element-property :CUSTOM_ID headline)
                     (org-export-get-reference headline info))))
    (concat indent "- [" title "]" "(#" anchor ")")))


;;;; Footnote section

(defun org-gfm-footnote-section (info)
  "Format the footnote section.
INFO is a plist used as a communication channel."
  (let* ((fn-alist (org-export-collect-footnote-definitions info))
         (fn-alist
          (cl-loop for (n type raw) in fn-alist collect
                   (cons n (org-trim (org-export-data raw info))))))
    (when fn-alist
      (format
       "## %s\n%s"
       "Footnotes"
       (format
        "\n%s\n"
        (mapconcat
         (lambda (fn)
           (let ((n (car fn)) (def (cdr fn)))
             (format
              "%s %s\n"
              (format
               (plist-get info :html-footnote-format)
               (org-html--anchor
                (format "fn.%d" n)
                n
                (format " class=\"footnum\" href=\"#fnr.%d\"" n)
                info))
              def)))
         fn-alist
         "\n"))))))


;;;; Template

(defun org-gfm-inner-template (contents info)
  "Return body of document after converting it to Markdown syntax.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let* ((depth (plist-get info :with-toc))
         (headlines (and depth (org-export-collect-headlines info depth)))
         (toc-string (or (mapconcat 'org-gfm-format-toc headlines "\n") ""))
         (toc-tail (if headlines "\n\n" "")))
    (org-trim (concat toc-string toc-tail contents "\n" (org-gfm-footnote-section info)))))
        
(defun org-gfm-template (contents info)
  "Add frontmatter in Github Flavoured Markdown format.
CONTENTS is GFM formart string, INFO is communication channel."
  (cl-flet ((strgen (key fmt &optional fn)
                    (let ((val (plist-get info key)))
                      (when (and val
                                 (if (stringp val)
                                     (not (string-empty-p val))
                                   t))
                        (format fmt (funcall (or fn 'identity) (plist-get info key)))))))
    (concat
     "---\n"
     (strgen :gfm-layout "layout: %s\n")
     (strgen :author "author: [%s]\n" (lambda (elm) (string-join elm ", ")))
     (strgen :title "title: \"%s\"\n" (lambda (elm) (car elm)))
     (strgen :description "description: \"%s\"\n")
     (strgen :gfm-category "category: %s\n")
     (strgen :gfm-tags "tags: [%s]\n"
             (lambda (elm) (string-join (split-string elm " " 'omit) ", ")))
     (strgen :keywords "keywords: [%s]\n"
             (lambda (elm) (string-join (split-string elm " " 'omit) ", ")))
     (strgen :date "date: %s\n"
             (lambda (elm)
               (let ((val (if (listp elm) (car elm) elm)))
                 (if (eq 'timestamp (car-safe val))
                     (org-timestamp-format val org-gfm-date-format)
                   (format-time-string org-gfm-date-format val)))))
     (strgen :last-modified "last_modified: %s\n"
             (lambda (elm)
               (let ((val (if (listp elm) (car elm) elm)))
                 (if (eq 'timestamp (car-safe val))
                     (org-timestamp-format val org-gfm-date-format)
                   (format-time-string org-gfm-date-format (date-to-time val))))))
     (strgen :gfm-custom-front-matter
             "%s\n"
             (lambda (elm)
               (mapconcat
                (lambda (elm)
                  (format "%s: %s" (car elm) (cdr elm)))
                (org-gfm--parse-property-arguments elm)
                "\n")))
     "---\n"
     (or (strgen :gfm-preamble "%s\n") "\n")
     contents
     (strgen :gfm-postamble "\n\n%s\n"))))



;;; Interactive function

;;;###autoload
(defun org-gfm-export-as-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org GFM Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'gfm "*Org GFM Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))


;;;###autoload
(defun org-gfm-convert-region-to-md ()
  "Assume `org-mode' syntax, and convert it to Github Flavored Markdown.
This can be used in any buffer.  For example, you can write an
itemized list in `org-mode' syntax in a Markdown buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'gfm))


;;;###autoload
(defun org-gfm-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Github Flavored Markdown file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'gfm outfile async subtreep visible-only)))

;;;###autoload
(defun org-gfm-publish-to-gfm (plist filename pub-dir)
  "Publish an org file to Markdown.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.
Return output file name."
  (org-publish-org-to 'gfm filename ".md" plist pub-dir))

(provide 'ox-gfm)

;;; ox-gfm.el ends here
