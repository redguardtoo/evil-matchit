;;; evil-matchit-c.el ---c like language (c/c++/perl/java/javascript) plugin of evil-matchit

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

(defun evilmi--c-find-open-brace (cur-line)
  (let (rlt)
    (if (string-match "^[ \t]* [a-zA-Z0-9]+.*[{] *$" cur-line)
        (setq rlt 1)
      (save-excursion
        (forward-line)
        (setq cur-line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
        (if (string-match "^[ \t]*{ *$" cur-line)
            (setq rlt 2)
          )
        )
      )
    rlt
    )
  )

;;;###autoload
(defun evilmi-c-get-tag ()
  (let (rlt
        (cur-line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position)))
        )

    ;; only handle open tag
    (when (and (not (memq (following-char) (string-to-list "{[(}}])")))
               (setq rlt (evilmi--c-find-open-brace cur-line))
               )
      (when rlt
        (forward-line (1- rlt))
        (search-forward "{" nil nil)
        (backward-char)
        )
      )
    rlt
    )
  )

;;;###autoload
(defun evilmi-c-jump (rlt NUM)
  (if rlt (evil-jump-item))
  )

(provide 'evil-matchit-c)