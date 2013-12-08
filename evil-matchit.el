;;; evil-matchit.el --- Vim matchit ported to Evil

;; Copyright (C) 2013 Chen Bin

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-matchit
;; Version: 1.0.2
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

(defvar evilmi-plugins '(emacs-lisp-mode
                         ((evilmi-simple-get-tag evilmi-simple-jump))
                         ))

(defun evilmi--operate-on-item (NUM &optional FUNC)
  (let (plugin
        rlt
        (jumped nil)
        where-to-jump-in-theory
        )

    (setq plugin (plist-get evilmi-plugins major-mode))

    (if plugin
        (mapc
         (lambda (elem)
           (setq rlt (funcall (nth 0 elem)))
           (when (and rlt (not jumped))
             ;; before jump, we may need some operation
             (if FUNC (funcall FUNC rlt))
             ;; jump now
             (setq where-to-jump-in-theory (funcall (nth 1 elem) rlt NUM))
             ;; jump only once if the jump is successful
             (setq jumped t)
             )
           )
         plugin
         )
      )
    (if (not jumped)
        (evilmi-simple-jump nil NUM)
        )
    where-to-jump-in-theory
    )
  )

(defun evilmi--push-mark (rlt)
    (push-mark (nth 0 rlt) t t)
  )

(defun evilmi--init-plugins ()
  (interactive)
  ;; html
  (autoload 'evilmi-html-get-tag "evil-matchit-html" nil t)
  (autoload 'evilmi-html-jump "evil-matchit-html" nil t)
  (mapc (lambda (mode)
          (plist-put evilmi-plugins mode '((evilmi-simple-get-tag evilmi-simple-jump)
                                           (evilmi-html-get-tag evilmi-html-jump)))
          )
        '(web-mode html-mode nxml-mode nxhtml-mode sgml-mode))

  ;; latex
  (autoload 'evilmi-latex-get-tag "evil-matchit-latex" nil t)
  (autoload 'evilmi-latex-jump "evil-matchit-latex" nil t)
  (plist-put evilmi-plugins 'latex-mode '((evilmi-latex-get-tag evilmi-latex-jump)))

  ;; python
  (autoload 'evilmi-python-get-tag "evil-matchit-python" nil t)
  (autoload 'evilmi-python-jump "evil-matchit-python" nil t)
  (plist-put evilmi-plugins 'python-mode '((evilmi-simple-get-tag evilmi-simple-jump)
                                           (evilmi-python-get-tag evilmi-python-jump)))

  ;; c like language (c/c++/perl/java/javascript)
  (autoload 'evilmi-c-get-tag "evil-matchit-c" nil t)
  (autoload 'evilmi-c-jump "evil-matchit-c" nil t)
  (mapc (lambda (mode)
          (plist-put evilmi-plugins mode '((evilmi-c-get-tag evilmi-c-jump)))
          )
        '(c-mode c++-mode java-mode js-mode js2-mode javascript-mode perl-mode cperl-mode))
  )

;;;###autoload
(defun evilmi-jump-items (&optional NUM)
  "jump between item/tag(s)"
  (interactive "p")
  (evilmi--operate-on-item NUM)
  )

;;;###autoload
(defun evilmi-select-items (&optional NUM)
  "select item/tag(s)"
  (interactive "p")
  (let (where-to-jump-in-theory )
    (setq where-to-jump-in-theory (evilmi--operate-on-item NUM 'evilmi--push-mark))
    (if where-to-jump-in-theory (goto-char where-to-jump-in-theory))
    )
  )

;;;###autoload
(defun evilmi-delete-items (&optional NUM)
  "delete item/tag(s)"
  (interactive "p")
  (let (where-to-jump-in-theory )
    (setq where-to-jump-in-theory (evilmi--operate-on-item NUM 'evilmi--push-mark))
    (if where-to-jump-in-theory (goto-char where-to-jump-in-theory))
    (kill-region (region-beginning) (region-end))
    )
  ;; need some hook here
  )

;;;###autoload
(define-minor-mode evil-matchit-mode
  "Buffer-local minor mode to emulate matchit.vim"
  :keymap (make-sparse-keymap)
  (if (fboundp 'evilmi-customize-keybinding)
      (evilmi-customize-keybinding)
    (evil-define-key 'normal evil-matchit-mode-map
      "%" 'evilmi-jump-items
      ",si" 'evilmi-select-items
      ",di" 'evilmi-delete-items
      )
    )
  (evil-normalize-keymaps)
  (evilmi--init-plugins)
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

;; @return (list position-first-char tag-type tag-keyword)

;; {{ simple find/jump
;; @return (list position-first-char tag-type tag-keyword) or nil
(defun evilmi-simple-get-tag ()
  (interactive)
  (let ((char (following-char))
        (tag-type -1)
        (tag-keyword "")
        (rlt nil)
        )

    (if (memq char (string-to-list "{[("))
        (setq rlt (list (point) 0 (char-to-string char)))
      )
    (if (memq char (string-to-list "}])"))
        (setq rlt (list (point) 1 (char-to-string char)))
      )
    rlt
    )
  )

(defun evilmi-simple-jump (rlt NUM)
  (interactive)
  (evil-jump-item)
  )
;; }}

(provide 'evil-matchit)

;;; evil-matchit.el ends here
