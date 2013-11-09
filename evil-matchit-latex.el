;;; evil-matchit-latex.el ---latex plugin of evil-matchit

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

(autoload 'LaTeX-find-matching-begin "latex-mode" nil t)
(autoload 'LaTeX-find-matching-end "latex-mode" nil t)

;;;###autoload
(defun evilmi-latex-get-tag ()
  (let (rlt
        (regexp (concat (regexp-quote "\\") "\\(begin\\|end\\)\\b"))
        keyword
        p
        )
    (skip-chars-backward "a-zA-Z \t{}")
    ;; move cursor to the beginning of tag
    (unless (bolp)
      (backward-char 1)
      )
    (re-search-forward regexp (line-end-position) t)
    (setq keyword (match-string 1))
    (setq rlt (list p
                    (if (string= keyword "begin")
                        0
                      (if (string= keyword "end")
                          1
                        -1))
                    )
          )
    rlt
    )
  )

;;;###autoload
(defun evilmi-latex-jump (rlt NUM)
  (let ((p (nth 0 rlt))
        (tag-type (nth 1 rlt))
        )
    (if (=  1 tag-type) (LaTeX-find-matching-begin))
    (if (=  0 tag-type) (LaTeX-find-matching-end))
    )
  )

(provide 'evil-matchit-latex)