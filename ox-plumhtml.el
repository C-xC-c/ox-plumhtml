;;; ox-plumhtml.el --- Sane HTML export for org-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Plum

;; Author: Plum <boku@plum.moe>
;; Created: June 2020
;; Package-Version: 1.0.2
;; Keywords: org-export
;; URL: https://words.plum.moe/ox-plumhtml.html
;; Package-Requires: ((emacs "26.1") (ox-slimhtml "0.4.5"))

;; This file is not part of GNU Emacs

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Sane extensions to ox-slimhtml

;;; Code:
(require 'ox-slimhtml)
ox-slimhtml

;; Variables
(defvar ox-plumhtml-export-header-ids nil)

;; Utils
(defun ox-plumhtml--table-header-p (element info)
  "Return t if the table has a header else nil."
  (or (org-export-table-has-header-p element info)
      (org-export-table-has-header-p (org-export-get-parent-table element) info)))

;; org-export Translations
(defun ox-plumhtml-paragraph (paragraph contents info)
  "This is the same as `ox-slimhtml-paragraph' but doesn't add a
<p> before export-snippet blocks like #+begin_src .... #+end_src"
  (when contents
    (if (or (ox-slimhtml--immediate-child-of-p paragraph 'item)
            (ox-slimhtml--immediate-child-of-p paragraph 'special-block)
            (ox-slimhtml--has-immediate-child-of-p paragraph 'export-snippet))
        contents
      (if (ox-slimhtml--has-immediate-child-of-p paragraph 'link)
          (format "<p>%s</p>" contents)
        (format "<p%s>%s</p>" (ox-slimhtml--attr paragraph) contents)))))

(defun ox-plumhtml-table (table contents info)  
  (format "<table>%s</table>" contents))

(defun ox-plumhtml-table-row (table-row contents info)
  "Transcodes an org table-row to HTML
Implement <thead> and <tbody>"
  (when (eq 'standard (org-element-property :type table-row))
    (let* ((open (org-export-table-row-starts-rowgroup-p table-row info))
           (close (org-export-table-row-ends-rowgroup-p table-row info))
           (first-row (= 0 (org-export-table-row-number table-row info)))
           (tags
            (cond
             ((and (ox-plumhtml--table-header-p table-row info)
                   (or (equal '(top) open)
                       (equal '(below) close)))
              '("<thead>" . "</thead>"))
             ((or (equal '(above) open)
                  (equal '(bottom) close)
                  first-row)
              '("<tbody>" . "</tbody>")))))
      (concat (and (or open first-row) (car tags))
              (format "<tr>%s</tr>" contents)
              (and close (cdr tags))))))

(defun ox-plumhtml-table-cell (table-cell contents info)
  "Transcodes and org table-cell to HTML
Uses <th> for table headers"
  (if (and (ox-plumhtml--table-header-p table-cell info)
           (org-export-table-row-in-header-p (org-export-get-parent table-cell) info))
      (format "<th>%s</th>" contents)
    (format "<td>%s</td>" contents)))

(defun ox-plumhtml-code (code contents info)
  (format "<code>%s</code>" (org-html-do-format-code (org-element-property :value code))))

(defun ox-plumhtml-verbatim (verbatim contents info)
  (format "<code>%s</code>" (org-element-property :value verbatim)))

(defun ox-plumhtml-headline (headline contents info)
  (let* ((text (org-export-data (org-element-property :title headline) info))
         (level (org-export-get-relative-level headline info))
         (attributes (org-element-property :ATTR_HTML headline))
         (container (org-element-property :HTML_CONTAINER headline))
         (container-class (and container (org-element-property :HTML_CONTAINER_CLASS headline))))
    (when attributes
      (setq attributes
            (format " %s" (org-html--make-attribute-string
                           (org-export-read-attribute 'attr_html `(nil
                                                                   (attr_html ,(split-string attributes))))))))
    (when (not (org-export-low-level-p headline info))
      (if ox-plumhtml-export-header-ids
          (format "<h%d id=\"%s\">%s</h%d>%s" level (org-export-get-reference headline info) text level (or contents ""))
        (format "<h%d%s>%s</h%d>%s" level (or attributes "") text level (or contents ""))))))

(defun ox-plumhtml-link (link contents info)
  "Transcode LINK from Org to HTML.

CONTENTS is the text of the link.
INFO is a plist holding contextual information."
  (cond
   ((ox-slimhtml--immediate-child-of-p link 'link)
    (org-element-property :raw-link link))
   ((not contents)
    (format "<em>%s</em>" (org-element-property :path link)))
   (t (let ((link-type (org-element-property :type link))
            (href (org-element-property :raw-link link))
            (attributes (if (ox-slimhtml--immediate-child-of-p link 'paragraph)
                            (ox-slimhtml--attr (org-export-get-parent link))
                          ""))
            (element "<a href=\"%s\"%s>%s</a>"))
        (cond
         ((string= "file" link-type)
          (let ((html-extension (or (plist-get info :html-extension) ""))
                (use-abs-url (plist-get info :html-link-use-abs-url))
                (link-org-files-as-html (plist-get info :html-link-org-as-html))
                (path (or (org-element-property :path link) "")))
            (format element
                    (concat (if (and use-abs-url (file-name-absolute-p path)) "file:" "")
                            (if (and link-org-files-as-html (string= "org" (downcase (or (file-name-extension path) ""))))
                                (if (and html-extension (not (string= "" html-extension)))
                                    (concat (file-name-sans-extension path) "." html-extension)
                                  (file-name-sans-extension path))
                              path))
                    attributes contents)))
         ((and (string= "fuzzy" link-type)
               ox-plumhtml-export-header-ids)
          (format "<a href=\"#%s\"%s>%s</a>"
                  (org-export-get-reference (org-export-resolve-fuzzy-link link info) info)
                  attributes contents))
         (t (format element href attributes contents)))))))

;; org-export backend and export/publish functions
(org-export-define-derived-backend 'plumhtml
    'slimhtml
  :translate-alist
  '((table . ox-plumhtml-table)
    (table-row . ox-plumhtml-table-row)
    (table-cell . ox-plumhtml-table-cell)
    (paragraph . ox-plumhtml-paragraph)
    (code . ox-plumhtml-code)
    (verbatim . ox-plumhtml-verbatim)
    (headline . ox-plumhtml-headline)
    (link . ox-plumhtml-link)))

;;;###autoload
(defun ox-plumhtml-publish-to-html (plist filename pub-dir)
  (org-publish-org-to 'plumhtml filename ".html" plist pub-dir))

;;;###autoload
(defun ox-plumhtml-export-as-html
    (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'plumhtml "*Org PlumHTML Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun ox-plumhtml-export-to-html (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((org-export-coding-system org-html-coding-system))
    (org-export-to-file 'plumhtml
        (org-export-output-file-name ".html" subtreep)
      async subtreep visible-only body-only ext-plist)))

(provide 'ox-plumhtml)
;;; ox-plumhtml.el ends here
