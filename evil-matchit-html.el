;;; evil-matchit-html.el ---html plugin of evil-matchit

;; Copyright (C) 2014-2019 Chen Bin <chenbin.sh@gmail.com>

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

(require 'evil-matchit-sdk)

(autoload 'sgml-skip-tag-backward "sgml-mode" nil t)
(autoload 'sgml-skip-tag-forward "sgml-mode" nil t)

;;;###autoload
(defun evilmi-html-get-tag ()
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (looping t)
         (char (following-char))
         (p (point))
         (found_tag -1))

    (save-excursion
      ;; search backward
      (unless (eq char ?<)
        (while (and looping (<= b (point)) (not (eq char ?<)))
          (setq char (following-char))
          (setq p (point))
          (if (eq p (point-min))
              ;; need get out of loop anyway
              (setq looping nil)
            (backward-char))))

      ;; search forward
      (unless (eq char ?<)
        (save-excursion
          (while (and (>= e (point)) (not (eq char ?<)))
            (setq char (following-char))
            (setq p (point))
            (forward-char))))

      ;; is end tag?
      (when (and (eq char ?<) (< p e))
        (goto-char p)
        (forward-char)
        (cond
         ((eq (following-char) ?/)
          ;; </
          (skip-chars-forward "^>")
          (forward-char)
          (setq found_tag 1))
         (t
          ;; < , looks fine
          (backward-char)
          (setq found_tag 0)))
        (setq p (point))))

    (when (eq found_tag 0)
      ;; sgml-skip-tag-forward can't handle the open html tag whose attribute containing "<" or ">" character
      ;; UNLESS the start position is above "<" character
      (goto-char p) ; move to the closest "<"
      (when (or (evilmi-among-fonts-p (point) evilmi-ignored-fonts)
                ;; In `rjsx-mode', the attribute value's font face could be nil
                ;; like <Component a1={3 > 2} />
                (and (eq ?< (following-char))
                     ;; if it's html tag, the character next "<" should
                     ;; have some font face
                     (not (get-text-property (1+ p) 'face))))
        ;; since current "<" is not part of open html tag,
        ;; skip backward to move cursor over the "<" of open html tag
        (sgml-skip-tag-backward 1)
        (setq p (point))))
    (list p found_tag "")))

;;;###autoload
(defun evilmi-html-jump (rlt num)
  (let* ((tag-type (nth 1 rlt))
         ;; `web-mode-forward-sexp' is assigned to `forward-sexp-function'
         ;; it's buggy in web-mode v11, here is the workaround
         (forward-sexp-function nil))
    (if (eq 1 tag-type) (sgml-skip-tag-backward num))
    (if (eq 0 tag-type) (sgml-skip-tag-forward num))
    (point)))

(provide 'evil-matchit-html)
