;;; evil-matchit.el --- Vim matchit ported to Evil

;; Copyright (C) 2014-2018 Chen Bin <chenbin.sh@gmail.com>

;; Author: Chen Bin <chenbin.sh@gmail.com>
;; URL: http://github.com/redguardtoo/evil-matchit
;; Version: 2.3.3
;; Keywords: matchit vim evil
;; Package-Requires: ((evil "1.2.0") (emacs "24.4"))
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
;; Add `(global-evil-matchit-mode 1)' into Emacs setup.
;; Then press % or `evilmi-jump-items' to jump between then matched pair.
;; Text object "%" is also provided.
;;
;; The shortcut "%" is defined in `evilmi-shortcut'. It's both the name of
;; text object and shortcut of `evilmi-jump-items'. Some people prefer set it
;; to "m". Here is sample setup:
;;
;;   (setq evilmi-shortcut "m")
;;   (global-evil-matchit-mode 1)
;;
;; See https://github.com/redguardtoo/evil-matchit/ for help.
;;
;; This program requires EVIL (http://gitorious.org/evil)
;;

;;; Code:

(require 'evil)
(require 'evil-matchit-sdk)

(defvar evilmi-plugins '(emacs-lisp-mode
                         ((evilmi-simple-get-tag evilmi-simple-jump)))
  "The Matrix to of algorithms.")

(defvar evilmi-may-jump-by-percentage t
  "Simulate `evil-jump-item'.
For example, `50%' jumps to 50 percentage of buffer.
If nil, `50%' jumps 50 times.")

(defvar evilmi-shortcut "%"
  "The keybinding of `evilmi-jump-items' and then text object shortcut.
Some people prefer using \"m\" instead.")

(defvar evilmi-always-simple-jump nil
  "`major-mode' like `python-mode' use optimized algorithm by default.
If it's t, use simple jump.")

(defvar evilmi-forward-chars (string-to-list "[{("))
(defvar evilmi-backward-chars (string-to-list "]})"))
(defvar evilmi-quote-chars (string-to-list "'\"/"))

(defun evilmi--char-is-simple (ch)
  "Special handling of character CH for `python-mode'."
  (let* (rlt)
    (cond
     ((and (not evilmi-always-simple-jump)
           (memq major-mode '(python-mode))
           ;; in evil-visual-state, (point) could equal to (line-end-position)
           (>= (point) (1- (line-end-position))))
      ;; handle follow python code,
      ;;
      ;; if true:
      ;;     a = "hello world"
      ;;
      ;; If current cursor is at end of line, rlt should be nil!
      ;; or else, matching algorithm can't work in above python sample
      (setq rlt nil))
     (t
      (setq rlt (or (memq ch evilmi-forward-chars)
                    (memq ch evilmi-backward-chars)
                    ;; sorry we could not jump between ends of string in python-mode
                    (memq ch evilmi-quote-chars)))))
    rlt))

(defun evilmi--get-char-at-position (pos)
  "Get character at POS."
  (let* ((ch (char-after pos)))
    (if evilmi-debug (message "evilmi--get-char-at-position called. Return: %s" (string ch)))
    ch))

(defun evilmi--get-char-under-cursor ()
  "Return: (character position)."
  (let* ((ch (following-char))
         (p (point)))
    (if evilmi-debug (message "evilmi--get-char-under-cursor called. Return: (%d %s)" ch p))
    (list ch p)))

(defun evilmi--is-jump-forward ()
  "Return: (forward-direction font-face-under-cursor character-under-cursor).
If font-face-under-cursor is NOT nil, the quoted string is being processed."
  (if evilmi-debug (message "evilmi--is-jump-forward called"))
  (let* ((tmp (evilmi--get-char-under-cursor))
         (ch (car tmp))
         (p (cadr tmp))
         ff
         (rlt t))
    (cond
     ((memq ch evilmi-backward-chars)
      (setq rlt nil))
     ((memq ch evilmi-quote-chars)
      (setq rlt (eq (setq ff (get-text-property p 'face))
                    (get-text-property (+ 1 p) 'face)))))

    (if evilmi-debug (message "evilmi--is-jump-forward return (%s %s %s)" rlt ff (string ch)))
    (list rlt ff ch)))

(defun evilmi--scan-sexps (is-forward)
  "Get the position of matching tag.
If IS-FORWARD is t, jump forward; or else jump backward."
  (if evilmi-debug (message "evilmi--scan-sexps called => %s" is-forward))
  (let* ((start-pos (if is-forward (point) (+ 1 (point))))
         (arg (if is-forward 1 -1))
         (limit (if is-forward (point-max) (point-min)))
         (lvl 1)
         (b (following-char))
         (e (cond
             ;; {}
             ((= b 123) 125)
             ((= b 125) 123)
             ;; ()
             ((= b 40) 41)
             ((= b 41) 40)
             ;; []
             ((= b 91) 93)
             ((= b 93) 91)))
         (rlt start-pos))
    (cond
     ((evilmi-in-comment-p (point))
      ;; Matching tag in comment.
      ;; Use own algorithm instead of `scan-sexps'
      ;; because `scan-sexps' not work in some major-mode
      (save-excursion
        (setq start-pos (point))
        (while (and (not (= start-pos limit))
                    (> lvl 0))
          (goto-char (setq start-pos (+ start-pos arg)))
          (when (evilmi-in-comment-p start-pos)
            (cond
             ((= (following-char) b)
              (setq lvl (1+ lvl)))
             ((= (following-char) e)
              (setq lvl (1- lvl))))))
        (when (= lvl 0)
          (setq rlt (+ start-pos (if is-forward 1 0))))))
     (t
      ;; not comment
      ;; search but ignore comments
      (let* ((parse-sexp-ignore-comments t))
        (setq rlt (scan-sexps start-pos arg)))))

    (if evilmi-debug (message "evilmi--scan-sexps called => rlt=%s lvl=%s" rlt lvl))
    rlt))

(defun evilmi--find-the-other-quote-char (ff is-forward ch)
  "The end character under cursor has different font-face from FF."
  (let* (rlt
         got
         (delta (if is-forward 1 -1))
         (pos (+ delta (point)))
         (end (if is-forward (point-max) (point-min))))
    (while (not got)
      (cond
       ((or (= pos end)
            (and (= ch (evilmi--get-char-at-position (- pos delta)))
                 (not (eq ff (get-text-property pos 'face)))))
        (setq rlt (if is-forward pos (+ 1 pos)))
        (setq got t))
       (t
        (setq pos (+ delta pos)))))
    (if evilmi-debug (message "evilmi--find-the-other-quote-char called Return: %s" rlt))
    rlt))

(defun evilmi--adjust-jumpto (is-forward rlt)
  ;; normal-state hack!
  (unless (eq evil-state 'visual)
    (if is-forward (setq rlt (- rlt 1))))
  (if evilmi-debug (message "evilmi--adjust-jumpto called. Return: %s" rlt))
  rlt)

;; @see http://emacs.stackexchange.com/questions/13222/a-elisp-function-to-jump-between-matched-pair
(defun evilmi--find-position-to-jump (ff is-forward ch)
  (if evilmi-debug (message "evilmi--find-position-to-jump called => %s %s %s %d" ff is-forward ch (point)))
  "Non-nil ff means jumping between quotes"
  (let* ((rlt (if ff (evilmi--find-the-other-quote-char ff is-forward ch)
                (evilmi--scan-sexps is-forward))))
    (if evilmi-debug (message "evilmi--find-position-to-jump return %s" (evilmi--adjust-jumpto is-forward rlt)))
    (evilmi--adjust-jumpto is-forward rlt)))

(defun evilmi--tweak-selected-region (ff jump-forward)
  ;; visual-state hack!
  (if (and jump-forward (eq evil-state 'visual) (not ff))
      ;; if ff is non-nil, I control the jump flow from character level,
      ;; so hack to workaround scan-sexps is NOT necessary
        (evil-backward-char)))

(defun evilmi--simple-jump ()
  "Alternative for `evil-jump-item'."
  (interactive)
  (if evilmi-debug (message "evilmi--simple-jump called (point)=%d" (point)))
  (let* ((tmp (evilmi--is-jump-forward))
         (jump-forward (car tmp))
         ;; if ff is not nil, it's jump between quotes
         ;; so we should not use (scan-sexps)
         (ff (nth 1 tmp))
         (ch (nth 2 tmp)))
    (goto-char (evilmi--find-position-to-jump ff jump-forward ch))
    (evilmi--tweak-selected-region ff jump-forward)))

(defun evilmi--operate-on-item (num &optional FUNC)
  (if evilmi-debug (message "evilmi--operate-on-item called => %s (point)=%d" num (point)))
  (let* ((plugin (plist-get evilmi-plugins major-mode))
         rlt
         jumped
         where-to-jump-in-theory)

    (unless num (setq num 1))

    (if plugin
        (mapc
         (lambda (elem)
           ;; execute evilmi-xxxx-get-tag
           (setq rlt (funcall (nth 0 elem)))
           (when (and rlt (not jumped))
             ;; before jump, we may need some operation
             (if FUNC (funcall FUNC rlt))
             ;; jump now, execute evilmi-xxxx-jump
             (setq where-to-jump-in-theory (funcall (nth 1 elem) rlt num))
             ;; jump only once if the jump is successful
             (setq jumped t)))
         plugin))

    ;; give `evilmi--simple-jump' a chance
    (when (not jumped)
      (if FUNC (funcall FUNC (list (point))))
      (evilmi--simple-jump)
      (setq where-to-jump-in-theory (point)))

    (if evilmi-debug (message "evilmi--operate-on-item called. Return: %s" where-to-jump-in-theory))
    where-to-jump-in-theory))

(defun evilmi--push-mark (rlt)
    (push-mark (nth 0 rlt) t t))

;;;###autoload
(defun evilmi-load-plugin-rules(modes rules)
  "Load MODES's plugin RULES."
  (dolist (mode modes)
    (setq evilmi-plugins
          (plist-put evilmi-plugins
                     mode
                     (mapcar (lambda (rule)
                               (let* ((rule-filename (concat "evil-matchit-" (symbol-name rule)))
                                      (fn-prefix (concat "evilmi-" (symbol-name rule)))
                                      (get-tag-function (intern (concat fn-prefix "-get-tag")))
                                      (jump-function (intern (concat fn-prefix "-jump"))))
                                 (autoload get-tag-function rule-filename nil)
                                 (autoload jump-function rule-filename nil)
                                 (list get-tag-function jump-function)))
                             rules)))))

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
                              message-mode
                              mhtml-mode)
                            '(template simple html))

  ;; Emacs Org-mode
  (evilmi-load-plugin-rules '(org-mode) '(org))

  ;; Markdown
  (evilmi-load-plugin-rules '(markdown-mode) '(markdown))

  ;; Latex
  (evilmi-load-plugin-rules '(latex-mode) '(latex simple))

  ;; Ocaml
  (evilmi-load-plugin-rules '(tuareg-mode) '(simple ocaml))

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
  (evilmi-load-plugin-rules '(f90 fortran-mode) '(fortran))

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

  ;; Elixir
  (evilmi-load-plugin-rules '(elixir-mode enh-elixir-mode) '(simple elixir)))


(defun evilmi--region-to-select-or-delete (num &optional is-inner)
  (let* (where-to-jump-in-theory b e)
    (save-excursion
      (setq where-to-jump-in-theory (evilmi--operate-on-item num 'evilmi--push-mark))
      (if where-to-jump-in-theory (goto-char where-to-jump-in-theory))
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
  "Select items/tags and the region between them."
  (interactive "p")
  (let* ((selected-region (evilmi--region-to-select-or-delete num)))
    (when selected-region
      (evilmi--push-mark selected-region)
      (goto-char (cadr selected-region)))))

;;;###autoload
(defun evilmi-delete-items (&optional num)
  "Delete items/tags and the region between them."
  (interactive "p")
  (let* ((selected-region (evilmi--region-to-select-or-delete num)))
    ;; 1+ because the line feed
    (kill-region (car selected-region) (1+ (cadr selected-region)))))

;;;###autoload
(defun evilmi-jump-to-percentage (num)
  "Like Vim %."
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
  "Jump between items."
  :repeat nil
  (interactive "P")
  (cond
   ((and evilmi-may-jump-by-percentage num)
    (evilmi-jump-to-percentage num))
   (t
    (evilmi--operate-on-item num))))

;;;###autoload
(defun evilmi-version()
  (interactive)
  (message "2.3.3"))

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
  "Enable evil-matchit-mode in the current buffer."
  (evil-matchit-mode 1))

;;;###autoload
(defun turn-off-evil-matchit-mode ()
  "Disable evil-matchit-mode in the current buffer."
  (evil-matchit-mode -1))

;;;###autoload
(define-globalized-minor-mode global-evil-matchit-mode
  evil-matchit-mode turn-on-evil-matchit-mode
  "Global minor mode to emulate matchit.vim.")

;; initialize evilmi-plugins only once
(evilmi-init-plugins)

(provide 'evil-matchit)
;;; evil-matchit.el ends here
