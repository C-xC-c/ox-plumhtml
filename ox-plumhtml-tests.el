(require 'package)
(package-initialize)

(load-file "ox-plumhtml.el")

(defun should-render-as (expected-result org-source &optional info skip-newline)
  (let ((expected-result (if skip-newline expected-result (concat expected-result "\n")))
        (info (plist-put info :html-container nil)))
    (should (string= expected-result
                     (org-export-string-as org-source 'plumhtml t info)))))

(defun regexp-render-as (expected-regexp org-source &optional info)
  (let ((info (plist-put info :html-container nil)))
    (should (string-match-p expected-regexp
                            (org-export-string-as org-source 'plumhtml t info)))))

;; Tables
(ert-deftest body ()
  (should-render-as
   "<table><tbody><tr><td>body</td></tr></tbody>\n</table>"
   "|body|"))

(ert-deftest multi-line-body ()
  (should-render-as
   "<table><tbody><tr><td>two</td></tr>\n<tr><td>lines</td></tr></tbody>\n</table>"
   "|two|\n|lines|"))

(ert-deftest header-no-body ()
  (should-render-as
   "<table><tbody><tr><td>just a header</td></tr></tbody>\n</table>"
   "|just a header|\n|---|"))

(ert-deftest header-and-body ()
  (should-render-as
   "<table><thead><tr><th>header</th></tr></thead>\n<tbody><tr><td>body</td></tr></tbody>\n</table>"
   "|header|\n|---|\n|body|"))

(ert-deftest header-and-multi-line-body ()
  (should-render-as
   "<table><thead><tr><th>header</th></tr></thead>\n<tbody><tr><td>line 1</td></tr>\n<tr><td>line 2</td></tr>\n<tr><td>line 3</td></tr></tbody>\n</table>"
   "|header|\n|---|\n|line 1|\n|line 2|\n|line 3|"))

(ert-deftest header-and-multiple-bodies ()
  (should-render-as
   "<table><thead><tr><th>header</th></tr></thead>\n<tbody><tr><td>two</td></tr></tbody>\n<tbody><tr><td>bodies</td></tr></tbody>\n</table>"
   "|header|\n|---|\n|two|\n|---|\n|bodies|"))

(ert-deftest header-and-body ()
  (should-render-as
   "<table><thead><tr><th>multi-line</th></tr>\n<tr><th>header</th></tr></thead>\n<tbody><tr><td>body</td></tr></tbody>\n</table>"
   "|multi-line|\n|header|\n|---|\n|body|"))

;; Headers and IDs
(ert-deftest header ()
  (should-render-as
   "<h1>mwee</h1>"
   "* mwee"))

(ert-deftest header-export-ids ()
  (regexp-render-as
   "<h1 id=\"org[[:alnum:]]\\{7\\}\">mwee</h1>"
   "* mwee" '(:export-header-ids t)))

;; Links and IDs
(ert-deftest link ()
  (should-render-as
   "<p><a href=\"https://example.com\">example</a></p>"
   "[[https://example.com][example]]"))
