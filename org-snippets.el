;;; org-snippets.el --- A code snippet manager with Org and Helm
;;
;; Filename: org-snippets.el
;; Description: A code snippet manager with Org and Helm
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Created: March 11, 2017
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") helm org)
;; Keywords: tools
;; Compatibility: GNU Emacs: 24.4+
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; This package collects code snippets under the inner-most Org heading. It
;; provides the following features:
;;
;; - List all Org headings with at least a code snippet. Each entry can be
;; visited for viewing.
;;
;; - Insert a code snippet into the current buffer. The description text is
;; stripped and all code snippets are concantenated into a single snippet and
;; then it is insert into current buffer.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'subr-x)
(require 'thingatpt)
;; (defvar org-snippets-cache nil)
(defvar org-snippets-file-list nil)

;; (defun org-snippets-invalidate-cache ()
;;   (interactive)
;;   (setq org-snippets-cache nil))

(defmacro org-snippets--get-heading-face (headline)
  `(intern-soft (concat "org-level-" (number-to-string (org-element-property :level ,headline)))))

(defmacro org-snippets--get-file (c)
  `(nth 0 ,c))

(defmacro org-snippets--get-line (c)
  `(nth 1 ,c))

(defmacro org-snippets--get-src-blocks (c)
  `(nth 2 ,c))

(defmacro org-snippets--get-real (candidate)
  `(cdr ,candidate))

(defun org-snippets ()
  "docstring"
  (interactive)
  (helm :sources (org-snippets--build-source)
        :buffer "*helm sync source*"))

(defun org-snippets--build-source ()
  "docstring"
  (interactive "P")
  (helm-build-sync-source "Org Snippets"
    :candidates (org-snippets--get-candidates)
    :action '(("Jump to snippet" . org-snippets--persistent-view)
              ("Insert code" . org-snippets--insert))
    :keymap org-snippets-map
    :persistent-action 'org-snippets--persistent-view))

(setq org-snippets-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (define-key map (kbd "C-c C-i") 'org-snippets-insert)
        (define-key map (kbd "C-c i") 'org-snippets-insert)
        (delq nil map)
        map))

(defun org-snippets--persistent-view (c)
  (find-file (org-snippets--get-file c))
  (goto-line (org-snippets--get-line c))
  (org-show-subtree))

(defun org-snippets-insert ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'org-snippets--insert)))

(defun org-snippets--insert (c)
  (maphash (lambda (file src-str)
             (let ((file-empty (string-equal file "empty")))
               (with-current-buffer (if file-empty
                                        (current-buffer)
                                      (find-file-noselect file))
                 (let ((start (point)))
                   (insert src-str)
                   (indent-region start (point))
                   (unless file-empty (save-buffer))))))
           (org-snippets--distribute-src-blocks-strings (org-snippets--get-src-blocks c))))

(defun org-snippets--distribute-src-blocks-strings (src-blocks)
  (let* ((dist-table (make-hash-table :test #'equal)))
    (mapcar (lambda (s)
              (let* ((src-data (org-snippets--process-src-block s))
                     (file (car src-data))
                     (new-str (cadr src-data))
                     (old-str (gethash file dist-table)))
                (puthash file (if old-str
                                  (concat old-str new-str)
                                new-str)  dist-table)))
            src-blocks)
    dist-table))

(defun org-snippets--src-string (src-block)
  (org-element-property :value src-block))

(defun org-snippets--process-src-block (s)
  "docstring"
  (let* ((src-parameters (org-babel-parse-header-arguments (org-element-property :parameters s)))
         (file (cdr (assoc :file src-parameters)))
         (src-string (org-snippets--src-string s)))
    (list (or file "empty") src-string)))

(defun org-snippets--get-candidates (&optional recipe)
  (-flatten-n
   1
   (delq
    nil
    (mapcar (lambda (f)
              (org-snippets--collect-snippets f recipe))
            (when (featurep 'org-wiki)
              (mapcar (lambda (f)
                        (concat org-wiki-location "/" f))
                      (org-wiki--page-files)))))))

(defun org-snippets--collect-snippets (f &optional recipe)
  (with-current-buffer (find-file-noselect f)
    (org-element-map (org-element-parse-buffer 'element) 'headline
      (lambda (headline)
        (let* ((src-blocks (org-element-map headline 'src-block 'identity))
               (symbol (org-element-property :SYMBOL headline))
               (src-blocks-parent (org-element-map headline 'headline 'identity))
               (linum (line-number-at-pos
                       (org-element-property :begin headline))))
          (when (and src-blocks
                     (eq (length src-blocks-parent) 1)
                     (or (null recipe)
                         (equal symbol (symbol-name recipe))))
            (cons (concat (concat (file-relative-name f org-wiki-location) ":")
                          (concat (number-to-string linum) ":")
                          " "
                          (when symbol (propertize (concat  "[" symbol "]  ") 'face 'font-lock-type-face))
                          (org-snippets--get-parent-string headline)
                          (propertize (org-element-property :title headline) 'face (org-snippets--get-heading-face headline)))
                  (list f
                        linum
                        src-blocks))))))))

(defun org-snippets--get-parent-string (headline)
  (when-let ((parent (org-element-property :parent headline))
             (parent-str (org-element-property :raw-value parent)))
    (concat (org-snippets--get-parent-string parent)
            (propertize parent-str 'face (org-snippets--get-heading-face parent))
            " / ")))

(defun org-snippets-dwim ()
  (interactive)
  (if-let ((recipe-list (list-at-point)))
      (progn
        (org-snippets--delete-thing-at-point recipe-list)
        (let ((start (point)))
          (mapcar (lambda (r)
                    (org-snippets--insert-recipe r)
                    (newline))
                  recipe-list)
          (indent-region start (point))))
    (when-let ((symbol (symbol-at-point)))
      (org-snippets--delete-thing-at-point symbol)
      (let ((start (point)))
        (org-snippets--insert-recipe symbol)
        (indent-region start (point))
        (newline)))))

(defun org-snippets--insert-recipe (recipe)
  (mapcar (lambda (c)
            (org-snippets--insert (org-snippets--get-real c)))
          (org-snippets--get-candidates recipe)))

(defun org-snippets--delete-thing-at-point (thing)
  (cond ((listp thing)
         (if (looking-at "\(")
             (kill-sexp)
           (backward-up-list)
           (kill-sexp)))
        ((symbolp thing)
         (mark-sexp)
         (delete-region (region-beginning) (region-end)))))

(provide 'org-snippets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-snippets.el ends here
;; End:
