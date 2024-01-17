;;; github-explorer.el --- Explore a GitHub repository on the fly -*- lexical-binding: t -*-

;; Copyright (C) 2019 Giap Tran <txgvnn@gmail.com>

;; Author: Giap Tran <txgvnn@gmail.com>
;; URL: https://github.com/KarimAziev/github-explorer
;; Version: 1.1.0
;; Package-Requires: ((emacs "27.1") (graphql "0.1.1") (cl-lib "1.0"))
;; Keywords: comm
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; M-x github-explorer "txgvnn/github-explorer"

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'graphql)
(require 'url)
(require 'hi-lock)



(defgroup github-explorer nil
  "Major mode of GitHub configuration file."
  :group 'languages
  :prefix "github-explorer-")

(defcustom github-explorer-hook nil
  "*Hook run by `github-explorer'."
  :type 'hook
  :group 'github-explorer)

(defcustom github-explorer-name "GitHub"
  "*Modeline of `github-explorer'."
  :type 'string
  :group 'github-explorer)

(defcustom github-explorer-sourcegraph-url "https://github1s.com/api/sourcegraph"
  "Sourcegraph API path."
  :type 'string
  :group 'github-explorer)

(defvar github-explorer-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "o") #'github-explorer-at-point)
    (define-key keymap (kbd "RET") #'github-explorer-at-point)
    (define-key keymap (kbd "f") #'github-explorer-find-file)
    (define-key keymap (kbd "s") #'github-explorer-search)
    (define-key keymap (kbd "l") #'previous-buffer)
    (define-key keymap (kbd "n") #'next-line)
    (define-key keymap (kbd "p") #'previous-line)
    keymap)
  "Keymap for GitHub major mode.")

(defface github-explorer-directory-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for a directory.")

(defvar github-explorer-buffer-temp nil)
(defvar-local github-explorer-repository nil)
(defvar github-explorer-paths (make-hash-table :test 'equal))

(defun github-explorer-paths--put (repo paths)
  "Put REPO to `github-explorer-paths' at PATHS."
  (puthash repo paths github-explorer-paths))

(defun github-explorer-paths--get (repo)
  "Get REPO from `github-explorer-paths' at PATHS."
  (gethash repo github-explorer-paths))

(defun github-explorer-util-package-try-get-package-url ()
  "Try and get single a package url under point.

From URL `https://github.com/akshaybadola/emacs-util'
`util/package-try-get-package-url'."
  (require 'package)
  (when (and (eq major-mode 'package-menu-mode))
    (let ((pkg-desc (tabulated-list-get-id (point))))
      (when (and pkg-desc
                 (fboundp 'package-desc-extras)
                 (package-desc-extras pkg-desc))
        (message (cdr (assq :url (package-desc-extras  pkg-desc))))
        (cdr (assq :url (package-desc-extras  pkg-desc)))))))

(defun github-explorer-repo-from-url (url)
  "Get repo user/name from URL."
  (string-join (last (split-string url "/") 2) "/"))





(declare-function json-read "json")

(defun github-explorer-json-read-buffer (&optional object-type array-type
                                                   null-object false-object)
  "Parse json from the current buffer using specified object and array types.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'/`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))

(defun github-explorer-fetch-repo-paths (repo)
  (url-retrieve
   (format
    "https://api.github.com/repos/%s/git/trees/HEAD:?recursive=1"
    repo)
   (lambda (arg)
     (cond ((equal :error (car arg))
            (message arg))
           (t
            (with-current-buffer (current-buffer)
              (goto-char (point-min))
              (re-search-forward "^$")
              (delete-region (+ 1 (point))
                             (point-min))
              (goto-char (point-min))
              (let*
                  ((paths
                    (remove nil
                            (mapcar
                             (lambda (x)
                               (pcase (cdr (assq 'type x))
                                 ("blob" (cdr (assq 'path x)))))
                             (cdr
                              (assoc 'tree
                                     (github-explorer-json-read-buffer)))))))
                (github-explorer-paths--put repo paths)
                (github-explorer--tree repo
                                       (format
                                        "https://api.github.com/repos/%s/git/trees/%s"
                                        repo "HEAD")
                                       "/"))))))))

;;;###autoload
(defun github-explorer (&optional repo)
  "Go REPO github."
  (interactive (list (or (and (thing-at-point 'url t)
                              (github-explorer-repo-from-url (thing-at-point
                                                              'url t)))
                         (and (eq major-mode 'org-mode)
                              (and (fboundp 'org-element-type)
                                   (fboundp 'org-element-context))
                              (eq (org-element-type (org-element-context)) 'link)
                              (when (fboundp 'org-element-property)
                                (ignore-errors
                                  (github-explorer-repo-from-url
                                   (org-element-property :raw-link
                                                         (org-element-context))))))
                         (and (eq major-mode 'package-menu-mode)
                              (github-explorer-repo-from-url
                               (github-explorer-util-package-try-get-package-url)))
                         (when (and
                                (require 'gh-repo nil t)
                                (fboundp 'gh-repo-search-repos))
                           (gh-repo-search-repos))
                         (thing-at-point 'symbol))))
  (let ((repo (read-string "Repository: " repo)))
    (url-retrieve (format
                   "https://api.github.com/repos/%s/git/trees/HEAD:?recursive=1"
                   repo)
                  (lambda (arg)
                    (cond ((equal :error (car arg))
                           (message arg))
                          (t
                           (with-current-buffer (current-buffer)
                             (goto-char (point-min))
                             (re-search-forward "^$")
                             (delete-region (+ 1 (point))
                                            (point-min))
                             (goto-char (point-min))
                             (let*
                                 ((paths
                                   (remove nil
                                           (mapcar
                                            (lambda (x)
                                              (pcase (cdr (assq 'type x))
                                                ("blob" (cdr (assq 'path x)))))
                                            (cdr
                                             (assoc 'tree
                                                    (github-explorer-json-read-buffer)))))))
                               (github-explorer-paths--put repo paths)
                               (github-explorer--tree repo
                                                      (format
                                                       "https://api.github.com/repos/%s/git/trees/%s"
                                                       repo "HEAD")
                                                      "/")))))))))

(defun github-explorer-find-file ()
  "Find file in REPO."
  (interactive)
  (let ((path (completing-read "Find file: " (github-explorer-paths--get github-explorer-repository))))
    (unless (eq (length path) 0)
      (github-explorer--raw github-explorer-repository (format "/%s" path)))))

(defun github-explorer-at-point()
  "Go to path in buffer GitHub tree."
  (interactive)
  (let (url path (pos 0) matches repo base-path item)
    (setq item (get-text-property (line-beginning-position) 'invisible))
    (setq url (cdr (assoc 'url item)))
    (setq path (cdr (assoc 'path item)))

    (setq repo (buffer-name))
    (while (string-match ":\\(.+\\)\\*" repo pos)
      (setq matches (concat (match-string 0 repo)))
      (setq pos (match-end 0)))
    (setq base-path (substring matches 1 (- (length matches) 1)))
    (setq repo (car (split-string base-path ":")))
    (setq path (format "%s%s"  (car (cdr (split-string base-path ":"))) path))
    (if (string= (cdr (assoc 'type item)) "tree")
        (setq path (concat path "/")))
    (message "%s" path)
    (if (string= (cdr (assoc 'type item)) "tree")
        (github-explorer--tree repo url path)
      (github-explorer--raw repo path))))

(defun github-explorer--tree (repo url path)
  "Get trees by URL of PATH github.
This function will create *GitHub:REPO:* buffer"
  (setq github-explorer-buffer-temp (format "*%s:%s:%s*" github-explorer-name repo path))
  (url-retrieve url
                (lambda (arg)
                  (cond
                   ((equal :error (car arg))
                    (message arg))
                   (t
                    (with-current-buffer (current-buffer)
                      (let (github-explorer-object)
                        (goto-char (point-min))
                        (re-search-forward "^$")
                        (delete-region (point) (point-min))
                        (goto-char (point-min))
                        (setq github-explorer-object (json-read))
                        (with-current-buffer (get-buffer-create github-explorer-buffer-temp)
                          (read-only-mode -1)
                          (erase-buffer)
                          (github-explorer--render-object github-explorer-object)
                          (goto-char (point-min))
                          (github-explorer-mode)
                          (setq-local github-explorer-repository repo)
                          (switch-to-buffer (current-buffer))))))))))

(defun github-explorer--raw (repo path)
  "Get raw of PATH in REPO github."
  (let (url)
    (setq url (format "https://raw.githubusercontent.com/%s/HEAD%s" repo path))
    (setq github-explorer-buffer-temp (format "*%s:%s:%s*" github-explorer-name
                                              repo path))
    (url-retrieve url
                  (lambda (arg)
                    (cond ((equal :error (car arg))
                           (message arg))
                          (t
                           (with-current-buffer (current-buffer)
                             (let (data)
                               (goto-char (point-min))
                               (re-search-forward "^$")
                               (delete-region (+ 1 (point))
                                              (point-min))
                               (goto-char (point-min))
                               (setq data (buffer-string))
                               (with-current-buffer (get-buffer-create
                                                     github-explorer-buffer-temp)
                                 (insert data)
                                 (github-explorer-apply-auto-mode)
                                 (pop-to-buffer (current-buffer))
                                 (goto-char (point-min))
                                 (setq-local github-explorer-repository repo))))))))))

(defun github-explorer-apply-auto-mode (&rest _)
  "Apply auto-mode for buffer GitHub.
pop-to-buffer(BUFFER-OR-NAME &OPTIONAL ACTION NORECORD)"
  (unless buffer-file-name
    (when (string-match-p (regexp-quote (format "*%s:" github-explorer-name))
                          (buffer-name))
      (setq buffer-file-name (concat
                              (or (when (fboundp 'temporary-file-directory)
                                    (temporary-file-directory))
                                  "/tmp/")
                              (file-name-nondirectory (substring (buffer-name) 0
                                                                 -1))))
      (set-auto-mode t))))


(defun github-explorer--item-path (item)
  "Get the path for an ITEM from GitHub."
  (cdr (assoc 'path item)))

(defun github-explorer--item-type (item)
  "Get the type for an ITEM from GitHub."
  (cdr (assoc 'type item)))


(defun github-explorer--render-object (github-explorer-object)
  "Render the GITHUB-EXPLORER-OBJECT."
  (let ((trees (cdr (assoc 'tree github-explorer-object))))
    (cl-loop
     for item across trees
     for path = (github-explorer--item-path item)
     if (string= (github-explorer--item-type item) "blob")
     do (insert (format "  |----- %s" path))
     else do
     (insert (format "  |--[+] %s" path))
     (left-char (length path))
     (put-text-property (point)
                        (+ (length path)
                           (point))
                        'face
                        'github-explorer-directory-face)
     do
     (put-text-property (line-beginning-position)
                        (1+ (line-beginning-position)) 'invisible item)
     (end-of-line)
     (insert "\n")))
  (insert (substitute-command-keys
           (format "\\{%s}" (symbol-name
                             'github-explorer-mode-map))
           t nil)))

(define-derived-mode github-explorer-mode special-mode github-explorer-name
  "Major mode for exploring GitHub repository on the fly."
  (setq buffer-auto-save-file-name nil
        buffer-read-only t))

(defun github-explorer-build-graphql (variable-query)
  "Build Graphql query.
VARIABLE-QUERY"
  (list
   (cons "query"
         (graphql-query
          (:arguments
           (($query . String!))
           (search
            :arguments
            ((query . ($ query)))
            (results limitHit
                     (results
                      ... on (FileMatch
                              (file name path)
                              (lineMatches preview lineNumber offsetAndLengths))))))))
   (cons "variables" (list (cons "query" variable-query)))))

(defun github-explorer-search (&optional prefix)
  "Using sourcegraph to search pattern.
If PREFIX is set, prompt repo"
  (interactive "P")
  (let ((repo
         (if (or (not github-explorer-repository) (and github-explorer-repository prefix))
             (read-string "Repository: ")
           github-explorer-repository))
        (query (read-string "Search: ")))
    (github-explorer-search-f repo query)))

(defun github-explorer-search-f (&optional repo query)
  "Search QUERY in REPO by using Sourcegraph API."
  (let ((url-request-method (encode-coding-string "POST" 'us-ascii))
        (url-request-extra-headers '())
        (url-request-data (json-encode (github-explorer-build-graphql (format "repo:^github.com/%s$ %s" repo query))))
        (url-mime-charset-string (url-mime-charset-string)))
    (url-retrieve github-explorer-sourcegraph-url
                  #'github-explorer-search-response
                  (append (list repo query)))))

(defun github-explorer-search--write (data)
  "Parse graphql DATA into current buffer."
  (dolist (linematch (cdr (car (cdr (cdr (car (cdr (car (cdr (car data))))))))))
    (insert (cdr (car (cdr (cdr (car linematch))))))
    (insert "\n")
    (dolist (line  (cdr (car (cdr linematch))))
      (insert (format "%s:" (cdr (assoc 'lineNumber line))))
      (insert (make-string (car (car (cdr(assoc 'offsetAndLengths line)))) #x20))
      (insert (cdr(assoc 'preview line)))
      (insert "\n"))))

(defun github-explorer-search-response (status repo query)
  "Render the response(STATUS) of searching QUERY in REPO."
  (if (= (point-min) (point-max))
      (signal (car (plist-get status :error)) (cdr (plist-get status :error)))
    (when (buffer-live-p (current-buffer))
      (with-current-buffer (current-buffer)
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point) (point-min))
        (goto-char (point-min))
        (let* ((json-object-type 'alist)
               (json-array-type 'list)
               (data (json-read))
               (bufname (format "*%s:%s?%s*" github-explorer-name repo query)))
          (with-current-buffer (get-buffer-create bufname)
            (github-explorer-search--write data)
            (hi-lock-set-pattern query font-lock-keyword-face nil nil t nil)
            (read-only-mode 1)
            (setq-local github-explorer-repository repo)
            (switch-to-buffer (current-buffer))))))))

;; (github-explorer-search-f "txgvnn/github-explorer" "defun")

(provide 'github-explorer)
;;; github-explorer.el ends here
