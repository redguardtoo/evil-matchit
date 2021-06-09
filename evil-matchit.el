;;; evil-matchit.el --- Vim matchit ported to Evil

;; Copyright (C) 2014-2020 Chen Bin <chenbin DOT sh AT gmail DOT com>

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: http://github.com/redguardtoo/evil-matchit
;; Version: 2.3.12
;; Keywords: matchit vim evil
;; Package-Requires: ((evil "1.2.0") (emacs "25.1"))
;;
;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; This program is free software: you can redistribute it and/or
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
;; Add `(global-evil-matchit-mode 1)' into Emacs setup.
;; Then press % or `evilmi-jump-items' to jump between then matched pair.
;; Text object "%" is also provided.
;;
;; The shortcut "%" is defined in `evilmi-shortcut'.  It's both the name of
;; text object and shortcut of `evilmi-jump-items'.  Some people prefer set it
;; to "m".  Here is sample setup:
;;
;;   (setq evilmi-shortcut "m")
;;   (global-evil-matchit-mode 1)
;;
;; See https://github.com/redguardtoo/evil-matchit/ for help.
;;
;; This program requires EVIL (https://github.com/emacs-evil/evil)
;;

;;; Code:

(eval-when-compile
  (require 'evil-macros))
(require 'evil-matchit-sdk)

(defvar evilmi-plugins '(emacs-lisp-mode ((evilmi-simple-get-tag evilmi-simple-jump)))
  "The Matrix to of algorithms.")

(defvar evilmi-may-jump-by-percentage t
  "Simulate `evil-jump-item'.
For example, `50%' jumps to 50 percentage of buffer.
If nil, `50%' jumps 50 times.")

(defvar evilmi-shortcut "%"
  "The keybinding of `evilmi-jump-items' and then text object shortcut.
Some people prefer using \"m\" instead.")

(defun evilmi--operate-on-item (num &optional func)
  "Jump NUM times and apply function FUNC."
  (when evilmi-debug
    (message "evilmi--operate-on-item called => %s (point)=%d" num (point)))
  (let* ((jump-rules (plist-get evilmi-plugins major-mode))
         rlt
         jumped
         ideal-dest)

    (unless num (setq num 1))

    (when jump-rules
      (dolist (rule jump-rules)
        ;; execute evilmi-xxxx-get-tag
        ;; every rule should be executed.
        ;; the simple rule might just forward a word
        (setq rlt (funcall (nth 0 rule)))
        (when (and rlt (not jumped))
          ;; before jump, we may need some operation
          (if func (funcall func rlt))
          ;; jump now, execute evilmi-xxxx-jump
          (setq ideal-dest (funcall (nth 1 rule) rlt num))
          ;; jump only once if the jump is successful
          (setq jumped t))
        (when evilmi-debug
          (message "rlt=%s rule=%s p=%s jumped=%s idea-dest=%s"
                   rlt
                   rule
                   (point)
                   jumped
                   ideal-dest))))

    ;; give `evilmi-sdk-simple-jump' a second chance
    (unless jumped
      (if func (funcall func (list (point))))
      (evilmi-sdk-simple-jump)
      (setq ideal-dest (point)))

    (if evilmi-debug (message "evilmi--operate-on-item called. Return: %s" ideal-dest))
    ideal-dest))

(defun evilmi--push-mark (position)
  "Pus POSITION as marker."
  (push-mark (nth 0 position) t t))

(defun evilmi--convert-rules (rules)
  "Convert RULES to function pairs list."
  (let* (rlt)
    (dolist (rule rules)
      (let* ((rule-filename (concat "evil-matchit-" (symbol-name rule)))
             (fn-prefix (concat "evilmi-" (symbol-name rule)))
             (get-tag-function (intern (concat fn-prefix "-get-tag")))
             (jump-function (intern (concat fn-prefix "-jump"))))
        (autoload get-tag-function rule-filename nil)
        (autoload jump-function rule-filename nil)
        (push (list get-tag-function jump-function) rlt)))
    (nreverse rlt)))

;;;###autoload
(defun evilmi-load-plugin-rules(modes rules)
  "Load MODES's plugin RULES."
  (dolist (mode modes)
    (setq evilmi-plugins (plist-put evilmi-plugins
                                    mode
                                    (evilmi--convert-rules rules)))))

;;;###autoload
(defun evilmi-init-plugins ()
  "Load plugins."
  (interactive)
  ;; simple matching for languages containing brackets
  (evilmi-load-plugin-rules '(java-mode perl-mode cperl-mode go-mode)
                            '(simple))

  ;; Javascript/Typescript
  (evilmi-load-plugin-rules '(js-mode
                              json-mode
                              js2-mode
                              js3-mode
                              javascript-mode
                              rjsx-mode
                              js2-jsx-mode
                              react-mode
                              typescript-mode
                              typescript-tsx-mode)
                            '(simple javascript html))

  ;; Html
  (evilmi-load-plugin-rules '(web-mode
                              html-mode
                              nxml-mode
                              nxhtml-mode
                              sgml-mode
                              php-mode
                              message-mode
                              mhtml-mode)
                            '(template simple html))

  ;; Emacs Org-mode
  (evilmi-load-plugin-rules '(org-mode) '(simple org))

  ;; Markdown
  (evilmi-load-plugin-rules '(markdown-mode) '(markdown))

  ;; Latex
  (evilmi-load-plugin-rules '(latex-mode) '(latex simple))

  ;; Ocaml
  (evilmi-load-plugin-rules '(tuareg-mode) '(simple ocaml))

  ;; Octave
  (evilmi-load-plugin-rules '(octave-mode) '(simple octave))

  ;; Python
  (evilmi-load-plugin-rules '(python-mode) '(simple python))

  ;; SQL
  (evilmi-load-plugin-rules '(sql-mode) '(simple sql))

  ;; C/C++
  (evilmi-load-plugin-rules '(c-mode c++-mode) '(c simple))

  ;; Diff/Patch
  (evilmi-load-plugin-rules '(diff-mode ffip-diff-mode magit-diff-mode)
                            '(simple diff))

  ;; Fortran
  (evilmi-load-plugin-rules '(f90-mode fortran-mode) '(fortran))

  ;; CMake (http://www.cmake.org)
  (evilmi-load-plugin-rules '(cmake-mode) '(cmake))

  ;; sh-mode
  (evilmi-load-plugin-rules '(sh-mode) '(sh))

  ;; verilog-mode
  (evilmi-load-plugin-rules '(verilog-mode) '(verilog))

  ;; Lua or script
  (evilmi-load-plugin-rules '(lua-mode vimrc-mode) '(simple script))

  ;; css/scss/less
  (evilmi-load-plugin-rules '(css-mode less-mode scss-mode) '(simple))

  ;; Ruby
  (evilmi-load-plugin-rules '(ruby-mode enh-ruby-mode) '(simple ruby))

  ;; terminal
  (evilmi-load-plugin-rules '(term-mode shell-mode) '(simple terminal))

  ;; Elixir
  (evilmi-load-plugin-rules '(elixir-mode enh-elixir-mode) '(simple elixir)))


(defun evilmi--region-to-select-or-delete (num &optional is-inner)
  "Get NUM region(s) to select or delete.
If IS-INNER is t, the region is inner text object."
  (let* (ideal-dest b e)
    (save-excursion
      (setq ideal-dest (evilmi--operate-on-item num #'evilmi--push-mark))
      (if ideal-dest (goto-char ideal-dest))
      (setq b (region-beginning))
      (setq e (region-end))
      (goto-char b)

      ;; for inner text object, forward a line at the beginning
      (cond
       (is-inner
        (forward-line 1)
        (setq b (line-beginning-position)))
       (t
        (if (string-match "[ \t]*" (buffer-substring-no-properties (line-beginning-position) b))
            (setq b (line-beginning-position)))))

      ;; for inner text object, backward a line at the end
      ;; but in python-mode, last line is also code line
      (when (and is-inner (not (eq major-mode 'python-mode)))
        (goto-char e)
        (forward-line -1)
        (setq e (line-end-position))))

    (if evilmi-debug (message "evilmi--region-to-select-or-delete called. Return: %s" (list b e)))
    (list b e)))

(evil-define-text-object evilmi-inner-text-object (&optional num begin end type)
  "Inner text object describing the region selected when you press % from evil-matchit"
  :type line
  (let* ((selected-region (evilmi--region-to-select-or-delete num t)))
    (evil-range (car selected-region) (cadr selected-region) 'line)))

(evil-define-text-object evilmi-outer-text-object (&optional num begin end type)
  "Outer text object describing the region selected when you press % from evil-matchit"
  :type line
  (let ((selected-region (evilmi--region-to-select-or-delete num)))
    (evil-range (car selected-region) (cadr selected-region) 'line)))

(define-key evil-inner-text-objects-map evilmi-shortcut 'evilmi-inner-text-object)
(define-key evil-outer-text-objects-map evilmi-shortcut 'evilmi-outer-text-object)

;;;###autoload
(defun evilmi-select-items (&optional num)
  "Select NUM items/tags and the region between them."
  (interactive "p")
  (let* ((selected-region (evilmi--region-to-select-or-delete num)))
    (when selected-region
      (evilmi--push-mark selected-region)
      (goto-char (cadr selected-region)))))

;;;###autoload
(defun evilmi-delete-items (&optional num)
  "Delete NUM items/tags and the region between them."
  (interactive "p")
  (let* ((selected-region (evilmi--region-to-select-or-delete num)))
    ;; 1+ because the line feed
    (kill-region (car selected-region) (1+ (cadr selected-region)))))

;;;###autoload
(defun evilmi-jump-to-percentage (num)
  "Like Vim %, NUM is the percentage of location."
  (interactive "P")
  (let* (dst)
    (when (and num (> num 0))
      (setq dst (let ((size (- (point-max) (point-min))))
                  (+ (point-min)
                     (if (> size 80000)
                         (* num (/ size 100))
                       (/ (* num size) 100)))))
      (cond
       ((< dst (point-min))
        (setq dst (point-min)))
       ((> dst (point-max))
        (setq dst (point-max))))
      (goto-char dst)
      (back-to-indentation))))

;;;###autoload (autoload 'evilmi-jump-items "evil-matchit" nil t)
(evil-define-command evilmi-jump-items (&optional num)
  "Jump between items NUM times."
  :repeat nil
  :jump t
  (interactive "P")
  (cond
   ((and evilmi-may-jump-by-percentage num)
    (evilmi-jump-to-percentage num))
   (t
    (evilmi--operate-on-item num))))

;;;###autoload
(defun evilmi-version()
  "Print version."
  (interactive)
  (message "2.3.12"))

(defvar evil-matchit-mode-map (make-sparse-keymap)
  "Keymap used by the minor mode.")

;;;###autoload
(define-minor-mode evil-matchit-mode
  "Buffer-local minor mode to emulate matchit.vim."
  :keymap (make-sparse-keymap)
  ;; get correct value of `(point)` in visual-line mode
  ;; @see https://bitbucket.org/lyro/evil/issues/540/get-the-char-under-cusor-in-visual-line
  (evil-set-command-property 'evilmi-jump-items :keep-visual t)
  (if (fboundp 'evilmi-customize-keybinding)
      ;; use user's own key bindings
      (evilmi-customize-keybinding)
    ;; else use default key bindings
    (evil-define-key 'normal evil-matchit-mode-map evilmi-shortcut 'evilmi-jump-items)
    (evil-define-key 'visual evil-matchit-mode-map evilmi-shortcut 'evilmi-jump-items))

  (evil-normalize-keymaps))

;;;###autoload
(defun turn-on-evil-matchit-mode ()
  "Enable the minor mode in the current buffer."
  (evil-matchit-mode 1))

;;;###autoload
(defun turn-off-evil-matchit-mode ()
  "Disable the minor mode in the current buffer."
  (evil-matchit-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-matchit-mode
  evil-matchit-mode turn-on-evil-matchit-mode
  "Global minor mode to emulate matchit.vim.")

;; initialize evilmi-plugins only once
(evilmi-init-plugins)

(provide 'evil-matchit)
;;; evil-matchit.el ends here
