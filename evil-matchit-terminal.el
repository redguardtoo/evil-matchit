;;; evil-matchit-terminal.el --- terminal plugin of evil-matchit

;; Copyright (C) 2020 Chen Bin <chenbin DOT sh AT gmail DOT com>

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

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


;;; Commentary:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
;;
;;; Commentary:
;;
;;; Code:

(require 'evil-matchit-sdk)

(defvar evilmi-terminal-primary-prompt "^\\$ "
  "The primary prompt of terminal")

(defvar evilmi-terminal-ps1-line-number 1
  "The line containing PS1.")

;;;###autoload
(defun evilmi-terminal-get-tag ()
  "Get tag at point."
  (let* ((curline (evilmi-sdk-curline))
         (prompt evilmi-terminal-primary-prompt)
         line1
         line2)
    (cond
     ((string-match prompt curline)
      ;; open tag
      (list (line-beginning-position) 0))
     (t
      (save-excursion
        (forward-line)
        (setq line1 (evilmi-sdk-curline))
        (forward-line)
        (setq line2 (evilmi-sdk-curline)))
      (when (and line1 line2 (or (string-match prompt line1)
                                 (string-match prompt line2)))
        (list (line-end-position) 1))))))

;;;###autoload
(defun evilmi-terminal-jump (info num)
  "Use INFO to jump NUM times."
  (ignore num)
  (when info
    (let* ((type (nth 1 info))
           (prompt evilmi-terminal-primary-prompt)
           pos)
      (cond
       ;; from open tag
       ((eq type 0)
        (save-excursion
          ;; skip current line
          (forward-line 1)
          (when (re-search-forward prompt (point-max) t)
            (forward-line -1)
            (forward-line (- evilmi-terminal-ps1-line-number))
            (setq pos (line-end-position))))
        (when pos (goto-char pos)))

       ;; from close tag
       ((eq type 1)
        (save-excursion
          ;; skip current line
          (when (re-search-backward prompt (point-min) t)
            (setq pos (line-beginning-position))))
        (when pos (goto-char pos))))
      pos)))

(provide 'evil-matchit-terminal)
;;; evil-matchit-terminal.el ends here
