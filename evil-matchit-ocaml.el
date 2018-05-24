;;; evil-matchit-ocaml.el -- tuareg-mode  plugin of evil-matchit

;; Copyright (C) 2014-2017 Chen Bin <chenbin.sh@gmail.com>

;; Author: Tomasz Kołodziejski <tkolodziejski@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'evil-matchit-sdk)

(defvar evilmi-ocaml-keywords
  '((("struct" "begin" "sig" "object") ("end"))
    (("if") ("then"))
    (("match") ("with"))
    (("match" "try") ("with"))
    (("while" "for") ("done"))
    (("let") ("in"))
    ())
  "Ocaml keywords.")

(defvar evilmi-ocaml-all-keywords
  (apply 'append (apply 'append evilmi-ocaml-keywords)))

(defvar evilmi-ocaml-keywords-regex
  (format "\\<\\(%s\\)\\>" (mapconcat 'identity evilmi-ocaml-all-keywords "\\|"))
  "Regexp to find next/previous keyword.")

(defun evilmi-ocaml-row-regex (tag-info)
  "Build regexp to find next/previous keyword in a row."
  (format "\\<\\(%s\\)\\>" (mapconcat 'identity (apply 'append tag-info) "\\|")))

(defun evilmi-ocaml-in-keyword-p (pos)
  "Check character at POS is keyword by comparing font face."
  (evilmi-current-font-among-fonts-p pos '(tuareg-font-lock-governing-face
                                           font-lock-keyword-face)))

;; jumps to next keyword. Returs nil if there's no next word
(defun evilmi-ocaml-next-possible-keyword (direction keywords-regex)
  (if (= direction 0)
      (let ((new-point (save-excursion
                         (forward-char)
                         (if (search-forward-regexp keywords-regex nil t)
                             (search-backward-regexp keywords-regex)
                           nil)
                         )))
        (if new-point (goto-char new-point)))
    (search-backward-regexp keywords-regex nil t)))

(defun evilmi-ocaml-next-keyword (direction &optional keywords-regex)
  "Jump to next keyword in a valid position. Return nil if no
such keyword is available."
  (let ((keywords-regex (or keywords-regex evilmi-ocaml-keywords-regex))
        (found-keyword-p nil)
        (keyword-exist-p t))
    (while (and (not found-keyword-p) keyword-exist-p)
      (setq keyword-exist-p (evilmi-ocaml-next-possible-keyword direction keywords-regex))
      (if (and keyword-exist-p (evilmi-ocaml-in-keyword-p (point)))
          (setq found-keyword-p t)))
    found-keyword-p))

(defun evilmi-ocaml-end-word ()
  (save-excursion
    (search-forward-regexp "\\>")
    (point)))

(defun evilmi-ocaml-get-word ()
  (buffer-substring-no-properties (point) (evilmi-ocaml-end-word)))

(defun evilmi-ocaml-is-keyword (l keyword)
  "Checks if the keyword belongs to a row."
  (find-if (lambda (w) (string-equal w keyword)) (apply 'append l)))

(defun evilmi-ocaml-get-tag-info (keyword)
  "Find the row in the evilmi-ocaml-keywords."
  (find-if (lambda (l) (evilmi-ocaml-is-keyword l keyword)) evilmi-ocaml-keywords))

;; 0 - forward
;; 1 - backward
(defun evilmi-ocaml-go (tag-info level direction)
  (let ((stop-p nil)
        (keywords-regex (evilmi-ocaml-row-regex tag-info)))
    (while (and (not stop-p) (/= level 0))
      (if (evilmi-ocaml-next-keyword direction keywords-regex)
          (if (member (evilmi-ocaml-get-word) (nth direction tag-info))
              (setq level (1+ level))
            (setq level (1- level)))
        (setq stop-p t)))
    (if (= level 0) (point))))

(defun evilmi-ocaml-goto-word-beginning ()
  (let ((bounds (bounds-of-thing-at-point 'word))
        (word (thing-at-point 'word))
        (line-end (line-end-position)))
    (if bounds (goto-char (car bounds)))
    (let ((next-keyword
           (save-excursion
             (if (find word evilmi-ocaml-all-keywords :test 'equal)
                 (point)
               (evilmi-ocaml-next-keyword 0)
               (if (< (point) line-end) (point))))))
      (if next-keyword (goto-char next-keyword)))))

;;;###autoload
(defun evilmi-ocaml-get-tag ()
  "Return information of current tag: (list position-of-word word)."
  (save-excursion
    (evilmi-ocaml-goto-word-beginning)
    (list (car (bounds-of-thing-at-point 'word))
          (evilmi-ocaml-get-word))))

;;;###autoload
(defun evilmi-ocaml-jump (rlt num)
  (let* ((keyword (cadr rlt))
         (tag-info (evilmi-ocaml-get-tag-info keyword))
         (direction (if (member keyword (car tag-info)) 0 1)))
    (let ((new-point (save-excursion
                       (evilmi-ocaml-goto-word-beginning)
                       (evilmi-ocaml-go tag-info 1 direction))))
      (if new-point (goto-char new-point)))))

(provide 'evil-matchit-ocaml)
