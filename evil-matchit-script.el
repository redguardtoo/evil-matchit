;;; evil-matchit-script.el ---script (ruby/lua) plugin of evil-matchit

;; Copyright (C) 2013  Chen Bin <chenbin.sh@gmail.com>

;; Author: Chen Bin <chenbin.sh@gmail.com>

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

(require 'evil-matchit)

;; ruby/bash/lua/vimrc
(defvar evilmi--script-match-tags
  '((("unless" "if") ("elif" "elsif" "elseif" "else") ( "end" "fi" "endif"))
    ("begin" ("rescue" "ensure") "end")
    ("case" ("when" "else") ("esac" "end"))
    (("fun!" "function!" "class" "def" "while" "function" "do") () ("end" "endfun" "endfunction"))
    ("repeat" ()  "until")
    )
  "the table we look up matched tags"
  )

(defun evilmi--script-get-tag-info (tag)
  "return (row column)"
  (let (rlt elems elem tag-type
        found i j)

    (setq i 0)
    (while (and (< i (length evilmi--script-match-tags)) (not found))
      (setq elems (nth i evilmi--script-match-tags))
      (setq j 0)
      (while (and (not found) (< j (length elems)))
        (setq elem (nth j elems))
        (cond
         ((stringp elem)
          (if (string= tag elem)
              (setq found t)
            ))
         ((listp elem)
          (if (member tag elem)
              (setq found t)
            ))
         )
        (if (not found) (setq j (1+ j)))
        )
      (if (not found) (setq i (1+ i)))
      )
    (if found
        (setq rlt (list i j))
      )
    rlt
    )
  )

(defun evilmi--script-extract-keyword (cur-line)
  "extract keyword from cur-line"
  (let (keyword
        (regexp "^[ \t]*\\([a-z]+\!?\\)\\( .*\\| *\\)$")
        (regexp-do "^.* \\(do\\) |[a-z0-9A-Z,|]+|$")
        )
    (if (string-match regexp cur-line)
        (setq keyword (match-string 1 cur-line))
      )

    (if (evilmi--member keyword evilmi--script-match-tags) keyword
      (when (string-match regexp-do cur-line)
          (setq keyword (match-string 1 cur-line))
          (if (evilmi--member keyword evilmi--script-match-tags) keyword)
        )
      )
    )
  )

;;;###autoload
(defun evilmi-script-get-tag ()
  (let (rlt
        keyword
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        tag-info)
    (when (setq keyword (evilmi--script-extract-keyword cur-line))
      ;; since we mixed ruby and lua mode here
      ;; maybe we should be strict at the keyword
      (if (setq tag-info (evilmi--script-get-tag-info keyword))
          ;; 0 - open tag; 1 - middle tag; 2 - close tag;
          (setq rlt (list
                     (if (= 2 (nth 1 tag-info))
                         (line-end-position)
                       (line-beginning-position))
                     tag-info))
        )
      )
    rlt
    ))

;;;###autoload
(defun evilmi-script-jump (rlt NUM)
  (let ((orig-tag-type (nth 1 (nth 1 rlt)))
        cur-tag-type
        (level 1)
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
        keyword
        found
        where-to-jump-in-theory
        )

    (while (not found)
      (forward-line (if (= orig-tag-type 2) -1 1))
      (setq cur-line (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))
            )

      (setq keyword (evilmi--script-extract-keyword cur-line))
      (when keyword
        (setq cur-tag-type (nth 1 (evilmi--script-get-tag-info keyword)))

        ;; key algorithm
        (cond
         ;; handle open tag
         ;; open (0) -> mid (1)  found when level is one else ignore
         ((and (= orig-tag-type 0) (= cur-tag-type 1))
          (when (= 1 level)
            (back-to-indentation)
            (setq where-to-jump-in-theory (1- (line-beginning-position)))
            (setq found t)
            )
          )
         ;; open (0) -> closed (2) found when level is zero, level--
         ((and (= orig-tag-type 0) (= cur-tag-type 2))
          (setq level (1- level))
          (when (= 0 level)
            (goto-char (line-end-position))
            (setq where-to-jump-in-theory (line-end-position))
            (setq found t)
            )
          )
         ;; open (0) -> open (0) level++
         ((and (= orig-tag-type 0) (= cur-tag-type 0))
          (setq level (1+ level))
          )

         ;; now handle mid tag
         ;; mid (1) -> mid (1) found when level is zero else ignore
         ((and (= orig-tag-type 1) (= cur-tag-type 1))
          (when (= 1 level)
            (back-to-indentation)
            (setq where-to-jump-in-theory (1- (line-beginning-position)))
            (setq found t)
            )
          )
         ;; mid (1) -> closed (2) found when level is zero, level --
         ((and (= orig-tag-type 1) (= cur-tag-type 2))
          (setq level (1- level))
          (when (= 0 level)
            (goto-char (line-end-position))
            (setq where-to-jump-in-theory (line-end-position))
            (setq found t)
            )
          )
         ;; mid (1) -> open (0) level++
         ((and (= orig-tag-type 1) (= cur-tag-type 0))
          (setq level (1+ level))
          )

         ;; now handle closed tag
         ;; closed (2) -> mid (1) ignore,impossible
         ((and (= orig-tag-type 2) (= cur-tag-type 1))
          (message "impossible to be here")
          )
         ;; closed (2) -> closed (2) level++
         ((and (= orig-tag-type 2) (= cur-tag-type 2))
          (setq level (1+ level))
          )
         ;; closed (2) -> open (0) found when level is zero, level--
         ((and (= orig-tag-type 2) (= cur-tag-type 0))
          (setq level (1- level))
          (when (= 0 level)
            (setq where-to-jump-in-theory (line-beginning-position))
            (back-to-indentation)
            (setq found t)
            )
          )
         (t (message "why here?"))
         )
        )

      ;; we will stop at end or beginning of buffer anyway
      (if (or (= (line-end-position) (point-max))
              (= (line-beginning-position) (point-min))
              )
          (setq found t)
        )
      )
    where-to-jump-in-theory
    )
  )

(provide 'evil-matchit-script)