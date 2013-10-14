;;; evil-matchit --- Vim matchit ported to Evil

;; Copyright (C) 2013 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-matchit
;; Version: 0.0.3
;; Keywords: matchit vim evil
;; Package-Requires: ((evil "1.0.7"))
;;
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
;;
;; This program emulates matchit.vim by Benji Fisher.
;; It allows you use % to match items.
;;
;; This program requires EVIL (http://gitorious.org/evil)
;;

;;; Code:

(require 'evil)

(defvar evilmi-html-major-modes
  '(nxml-mode
    web-mode
    html-mode
    nxhtml-mode
    )
  "major modes containing html tags")

;; @return (list position_first_char found_tag is_end_tag)
(defun evilmi--find-lt-or-gt-char-at-current-line ()
  (interactive)
  (let ((b (line-beginning-position))
        (e (line-end-position))
        (char (following-char))
        (p (point))
        (found_tag nil)
        (is_end_tag nil)
        )

    (save-excursion
      ;; search backward
      (if (not (= char 60))
          (while (and (<= b (point)) (not (= char 60)))
            (setq char (following-char))
            (setq p (point))
            (backward-char)
            )
        )
      ;; search forward
      (if (not (= char 60))
          (save-excursion
            (while (and (>= e (point)) (not (= char 60)))
              (setq char (following-char))
              (setq p (point))
              (forward-char)
              )
            )
        )

      ;; is end tag?
      (when (and (= char 60) (< p e))
        (goto-char p)
        (forward-char)
        (if (= (following-char) 47)
            (progn
              ;; </
              (skip-chars-forward "^>")
              (forward-char)
              (setq p (point))
              (setq found_tag t)
              (setq is_end_tag t)
              )
          (progn
            ;; < , looks fine
            (backward-char)
            (setq found_tag t)
            )
          )
        )
      )
    (list p found_tag is_end_tag)
    ))

(defun evilmi--operate-on-item (NUM fn)
  (if (memq major-mode evilmi-html-major-modes)
      (let ((rlt (evilmi--find-lt-or-gt-char-at-current-line)))
        ;; prepare to jump
        (when (nth 1 rlt)
          (if (nth 2 rlt)
              ;; it's the end tag
              (progn
                (funcall fn (nth 0 rlt))
                (sgml-skip-tag-backward NUM)
                )
            ;; open tag
            (progn
              (funcall fn (nth 0 rlt))
              (sgml-skip-tag-forward NUM)
              )
            )
          )
        )
    ;; just use evil-jump item
    (progn
      ;; evil has its own API, so normail Emacs API may not work
      (if (eq fn 'evilmi--push-mark)
          (evil-visual-char)
          )
      (evil-jump-item)
      )
    )
  )

(defun evilmi--push-mark (p)
  (push-mark p t t)
  )

;;;###autoload
(defun evilmi-jump-items (&optional NUM)
  "jump between item/tag(s)"
  (interactive "p")
  (evilmi--operate-on-item NUM 'goto-char)
  )

;;;###autoload
(defun evilmi-select-items (&optional NUM)
  "select item/tag(s)"
  (interactive "p")
  (evilmi--operate-on-item NUM 'evilmi--push-mark)
  )

;;;###autoload
(defun evilmi-delete-items (&optional NUM)
  "delete item/tag(s)"
  (interactive "p")
  (evilmi--operate-on-item NUM 'evilmi--push-mark)
  (kill-region (region-beginning) (region-end))
  )

;;;###autoload
(define-minor-mode evil-matchit-mode
  "Buffer-local minor mode to emulate matchit.vim"
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps)
  )

;;;###autoload
(defun turn-on-evil-matchit-mode ()
  "Enable evil-matchit-mode in the current buffer."
  (evil-matchit-mode 1))

;;;###autoload
(defun turn-off-evil-matchit-mode ()
  "Disable evil-matchit-mode in the current buffer."
  (evil-matchit-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-matchit-mode
  evil-matchit-mode turn-on-evil-matchit-mode
  "Global minor mode to emulate matchit.vim")

(define-key evil-normal-state-map "%" 'evilmi-jump-items)
(define-key evil-normal-state-map ",si" 'evilmi-select-items)
(define-key evil-normal-state-map ",di" 'evilmi-delete-items)

(provide 'evil-matchit)

;;; evil-matchit.el ends here
