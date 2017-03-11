;;; helm-org-snippets.el --- A code snippet manager with Org and Helm
;;
;; Filename: helm-org-snippets.el
;; Description: A code snippet manager with Org and Helm
;; Author: Tu, Do Hoang <tuhdo1710@gmail.com>
;; URL      : https://github.com/tuhdo/semantic-refactor
;; Maintainer: Tu, Do Hoang
;; Created: March 11, 2017
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") helm org)
;; Keywords: tools
;; Compatibility: GNU Emacs: 24.3+
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

;; (defvar hos-cache nil)
(defvar hos-org-file-list nil)

;; (defun hos-invalidate-cache ()
;;   (interactive)
;;   (setq hos-cache nil))

(defmacro hos--get-heading-face (headline)
  `(intern-soft (concat "org-level-" (number-to-string (org-element-property :level ,headline)))))

(defmacro hos--get-file (c)
  `(nth 0 ,c))

(defmacro hos--get-line (c)
  `(nth 1 ,c))

(defmacro hos--get-code (c)
  `(nth 2 ,c))

(defun helm-org-snippets ()
  "docstring"
  (interactive)
  (helm :sources (hos--build-source)
        :buffer "*helm sync source*"))

(defun hos--build-source ()
  "docstring"
  (interactive "P")
  (helm-build-sync-source "*hos*"
    :candidates (-flatten-n 1
                            (delq nil
                                  (mapcar (lambda (f)
                                            (hos--collect-snippets f))
                                          (when (featurep 'org-wiki)
                                            (mapcar (lambda (f)
                                                      (concat org-wiki-location "/" f))  (org-wiki--page-files))))))
    :action '(("Jump to snippet" . hos--persistent-view)
              ("Insert code" . hos--insert))
    :keymap hos-map
    :persistent-action 'hos--persistent-view))

(setq hos-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map helm-map)
        (define-key map (kbd "C-c C-i") 'hos-insert)
        (define-key map (kbd "C-c i") 'hos-insert)
        (delq nil map)
        map))

(defun hos--persistent-view (c)
  (find-file (hos--get-file c))
  (goto-line (hos--get-line c))
  (org-show-subtree))

(defun hos-insert ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action 'hos--insert)))

(defun hos--insert (c)
  (insert (hos--get-code c)))

(defun hos--collect-snippets (f)
  (with-current-buffer (find-file-noselect f)
    (org-element-map (org-element-parse-buffer 'element) 'headline
      (lambda (headline)
        (let* ((src-blocks (org-element-map headline 'src-block
                             (lambda (s)
                               (org-element-property :value s))))
               (src-blocks-parent (org-element-map headline 'headline 'identity))
               (linum (line-number-at-pos
                       (org-element-property :begin headline))))
          (when (and src-blocks
                     (eq (length src-blocks-parent) 1))
            (cons (concat (propertize (concat (file-relative-name f org-wiki-location) ":") 'face 'dired-directory)
                          (propertize (concat (number-to-string linum) ":")
                                      'face 'compilation-line-number)
                          " "
                          (hos--get-parent-string headline)
                          (propertize (org-element-property :title headline) 'face (hos--get-heading-face headline)))
                  (list f linum (mapconcat 'identity src-blocks  "")))))))))

(defun hos--get-parent-string (headline)
  (when-let ((parent (org-element-property :parent headline))
             (parent-str (org-element-property :raw-value parent)))
    (concat (hos--get-parent-string parent)
            (propertize parent-str 'face (hos--get-heading-face parent))
            " / ")))

(provide 'helm-org-snippets)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; helm-org-snippets.el ends here
;; End:
