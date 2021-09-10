;;; evil-matchit-tests.el ---  unit tests for evil-matchit -*- coding: utf-8 -*-

;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;; Code:

(require 'ert)
(require 'evil-matchit)

(setq evilmi-may-jump-by-percentage nil)
(setq evilmi-debug nil) ; debug

(defun evilmi-test-read-file (file)
  "Read FILE's content into current buffer."
  (let* ((files (directory-files-recursively default-directory file)))
    (when files
      (insert-file-contents (car files)))))

(ert-deftest evilmi-test-generic ()
  (let* ((str "123456")
         (jump-offset (+ 1 (length str))))
    (with-temp-buffer
      (insert (format "[%s]" str))
      (insert (format "{([%s])}" str))
      (insert "=BEG=\n{\nhello world\n}\n=END=")
      ;; test first segment
      (goto-char (point-min))
      (evilmi-jump-items)
      (should (eq (point) (+ (point-min) jump-offset)))
      ;; jump back
      (evilmi-jump-items)
      (should (eq (point) (point-min)))

      ;; test "{}"
      (goto-char (+ (point-min) 1 jump-offset))
      (evilmi-jump-items)
      (should (eq (following-char) ?}))
      (evilmi-jump-items)
      (should (eq (following-char) ?{))

      ;; test "()"
      (goto-char (+ (point-min) 2 jump-offset))
      (evilmi-jump-items)
      (should (eq (following-char) 41)) ; ?)
      (evilmi-jump-items)
      (should (eq (following-char) 40)) ; ?(

      ;; test deleting {}
      (goto-char (point-min))
      (search-forward "=BEG=")
      (forward-char)
      (let* ((pos (- (point) (length "=BEG=") 1)))
        (should (eq (following-char) ?{))
        (evilmi-delete-items)
        (should (string= (buffer-substring pos (point-max)) "=BEG=\n\n=END=")))

      (should (eq major-mode 'fundamental-mode)))))

(ert-deftest evilmi-test-javascript ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.js")
    (js-mode)

    (font-lock-ensure)

    ;; for javascript, jump from anywhere in function beginning
    (goto-char (+ 3 (point-min)))
    (evilmi-jump-items)
    (should (eq (following-char) ?}))

    ;; jump from start again
    (goto-char (point-min))
    (search-forward "{")
    (evilmi-jump-items)
    (should (eq (following-char) ?}))
    ;; jump back
    (evilmi-jump-items)
    (should (eq (following-char) ?{))

    (search-forward "'")
    (backward-char 1)
    (let* ((beg (point))
           end)
      (should (string= "'" (string (following-char))))
      (evilmi-jump-items)
      (setq end (point))
      (should (> end beg))
      (should (string= (buffer-substring-no-properties (1+ beg) end) "hello world"))
      (evilmi-jump-items)
      (should (eq (point) beg)))

    (search-forward "test1" (point-max) t)
    (backward-word)
    (should (string= (evilmi-sdk-curline) "const test1 = ["))
    (evilmi-jump-items)
    (should (string= (evilmi-sdk-curline) "];"))
    (evilmi-jump-items)
    (should (string= (evilmi-sdk-curline) "const test1 = ["))

    (search-forward "test2" (point-max) t)
    (backward-word)
    (should (string= (evilmi-sdk-curline) "const test2 = {"))
    (evilmi-jump-items)
    (should (string= (evilmi-sdk-curline) "};"))
    (evilmi-jump-items)
    (should (string= (evilmi-sdk-curline) "const test2 = {"))

    (search-forward "test3" (point-max) t)
    (backward-word)
    (should (string= (evilmi-sdk-curline) "const test3 = hello("))
    (evilmi-jump-items)
    (should (string= (evilmi-sdk-curline) ");"))
    (evilmi-jump-items)
    (should (string= (evilmi-sdk-curline) "const test3 = hello("))

    (should (eq major-mode 'js-mode))))

(ert-deftest evilmi-test-html ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.html")
    (html-mode)

    ;; jump from start again
    (goto-char (point-min))
    (search-forward "<head")
    ;; Please note it jumps to line feed
    (evilmi-jump-items)
    (goto-char (1- (point)))
    (should (eq (following-char) ?>))
    (should (string= (thing-at-point 'symbol) "/head"))

    ;; self closing tag
    (goto-char (point-min))
    (search-forward "<link")
    (evilmi-jump-items)
    (goto-char (1- (point)))
    (should (eq (following-char) ?>))
    (should (string= (thing-at-point 'symbol) "/"))
    (evilmi-jump-items)
    (should (eq (following-char) ?<))
    (forward-char)
    (should (string= (thing-at-point 'word) "link"))

    ;; tags in one line
    (goto-char (point-min))
    (search-forward "<p")
    (evilmi-jump-items)
    (goto-char (1- (point)))
    (should (eq (following-char) ?>))
    (should (string= (thing-at-point 'symbol) "/p"))

    (should (eq major-mode 'html-mode))))

(ert-deftest evilmi-test-c ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.c")
    (c-mode)

    ;; jump from start
    (goto-char (point-min))
    ;; test #ifdef
    (evilmi-jump-items)
    (should (string= "endif" (thing-at-point 'symbol)))
    ;; test #ifndef
    (forward-line -1)
    (evilmi-jump-items)
    (should (eq (point) (line-beginning-position)))
    (should (eq (following-char) ?#))
    (forward-char)
    (should (string= "ifndef" (thing-at-point 'symbol)))

    ;; jump from function begin to end
    (goto-char (point-min))
    (search-forward "static int");
    (evilmi-jump-items)
    (should (eq (following-char) ?}))
    (should (string= (evilmi-sdk-curline) "}"))
    ;; jump back
    (evilmi-jump-items)
    (should (eq (following-char) ?{))
    (should (string= (evilmi-sdk-curline) "{"))

    ;; jump in switch statement
    (goto-char (point-min))
    (search-forward "switch");
    (evilmi-jump-items)
    (should (eq (following-char) ?c))
    (should (string= (thing-at-point 'symbol) "case"))
    (should (string-match "case 'a'" (evilmi-sdk-curline)))
    ;; goto next case statement
    (evilmi-jump-items)
    (should (eq (following-char) ?c))
    (should (string= (thing-at-point 'symbol) "case"))
    (should (string-match "case 'b'" (evilmi-sdk-curline) ))
                                        ; jump back
    (evilmi-jump-items)
    (should (eq (following-char) ?{))
    (should (string-match "switch(c)" (evilmi-sdk-curline)))

    (should (eq major-mode 'c-mode))))

(ert-deftest evilmi-test-lua ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.lua")
    (lua-mode)

    ;; jump from start
    (goto-char (point-min))

    ;; test if else end
    (evilmi-jump-items)
    (should (string= "else" (thing-at-point 'symbol)))
    (evilmi-jump-items)
    (should (string= "end" (thing-at-point 'symbol)))
    (evilmi-jump-items)
    ;; no back to the beginning
    (should (string= "if" (thing-at-point 'symbol)))
    (should (eq (point) (point-min)))

    ;; test function
    (search-forward "local thread = coroutine.create")
    (evilmi-jump-items)
    (should (string= "end)" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "local thread = coroutine.create(function()" (string-trim (evilmi-sdk-curline))))
    ;; test while embedded in function
    (search-forward "while true do")
    (evilmi-jump-items)
    (should (string= "end" (thing-at-point 'symbol)))
    (evilmi-jump-items)
    (should (string= "while true do" (string-trim (evilmi-sdk-curline))))

    (should (eq major-mode 'lua-mode))))

(ert-deftest evilmi-test-diff ()
  (with-temp-buffer
    (evilmi-test-read-file "hello-git.diff")
    (diff-mode)

    ;; test git diff
    ;; jump to diff end
    (goto-char (point-min))
    (evilmi-jump-items)
    (should (string= "line2" (evilmi-sdk-curline)))
    ;; jump back to diff beginning
    (evilmi-jump-items)
    (should (string= "diff 1" (evilmi-sdk-curline)))
    (should (eq (point) (point-min)))
    ;; jump from second line of diff beginning
    (forward-line 1)
    (evilmi-jump-items)
    (should (string= "line2" (evilmi-sdk-curline)))
    ;; jump from third line of diff beginning
    (goto-char (point-min))
    (forward-line 2)
    (evilmi-jump-items)
    (should (string= "line2" (evilmi-sdk-curline)))
    ;; jump from fourth line of diff beginning
    (goto-char (point-min))
    (forward-line 3)
    (evilmi-jump-items)
    (should (string= "line2" (evilmi-sdk-curline)))

    ;; test GNU diff
    (erase-buffer)
    (evilmi-test-read-file "hello-gnu.diff")
    ;; jump to diff end
    (goto-char (point-min))
    (evilmi-jump-items)
    (should (string= "line2" (evilmi-sdk-curline)))
    ;; jump back to diff beginning
    (evilmi-jump-items)
    (should (string= "--- a/1.c" (evilmi-sdk-curline)))
    (should (eq (point) (point-min)))
    ;; jump from second line of diff beginning
    (forward-line 1)
    (evilmi-jump-items)
    (should (string= "line2" (evilmi-sdk-curline)))
    (should (eq major-mode 'diff-mode))))

(ert-deftest evilmi-test-fortran ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.f")
    (f90-mode)

    (goto-char (point-min))
    ;; jump to end tag
    (evilmi-jump-items)
    (should (string= "END PROGRAM" (evilmi-sdk-curline)))
    ;; jump back to open tags
    (evilmi-jump-items)
    (should (string= "PROGRAM cows" (evilmi-sdk-curline)))

    ;; lower case conditional statement
    (search-forward "if (x < x1)")
    (goto-char (line-beginning-position))
    (evilmi-jump-items)
    (should (string= "else" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "end if" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "if (x < x1) then" (evilmi-sdk-curline)))

    ;; upper case conditional statement
    (search-forward "IF (x < 50)")
    (goto-char (line-beginning-position))
    (evilmi-jump-items)
    (should (string= "ELSE IF (x < 60) THEN" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "ELSE IF (x < 70) THEN" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "ELSE" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "END IF" (evilmi-sdk-curline)))

    (should (eq major-mode 'f90-mode))))

(ert-deftest evilmi-test-verilog ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.v")
    (verilog-mode)

    (goto-char (point-min))
    (evilmi-jump-items)
    (should (string= "`else" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "`endif" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "`ifdef behavioral" (evilmi-sdk-curline)))

    (goto-char (point-min))
    (search-forward "always")
    (goto-char (line-beginning-position))
    (evilmi-jump-items)
    (should (string= "end" (thing-at-point 'symbol)))

    (should (eq major-mode 'verilog-mode))))

(ert-deftest evilmi-test-markdown ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.md")
    (markdown-mode)

    (goto-char (point-min))
    (forward-line 1)
    (evilmi-jump-items)
    (should (string= "```" (evilmi-sdk-curline)))
    (forward-line -1)
    (should (string= "Example 1" (evilmi-sdk-curline)))
    (forward-line 1)
    (evilmi-jump-items)
    (should (string= "```java" (evilmi-sdk-curline)))

    (goto-char (point-min))
    (search-forward "```c")
    (evilmi-jump-items)
    (should (string= "```" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "```c" (evilmi-sdk-curline)))

    (should (eq major-mode 'markdown-mode))))

(ert-deftest evilmi-test-emacs-lisp ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.el")
    (emacs-lisp-mode)
    (font-lock-ensure)

    (goto-char (point-min))
    (forward-char 2)
    (evilmi-jump-items)
    ;; still at the same line if there is no bracket at point
    (should (string= ";; test1" (evilmi-sdk-curline)))
    ;; test matched brackets
    (search-forward "{")
    (evilmi-jump-items)
    (should (string= ";; }}" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= ";; {{" (evilmi-sdk-curline)))

    (should (eq major-mode 'emacs-lisp-mode))))

(ert-deftest evilmi-test-ocaml ()
  (with-temp-buffer
    (require 'tuareg)
    (tuareg-mode)

    (defun prepare (&rest text)
      (progn
        (erase-buffer)
        (apply 'insert text)
        (goto-char (point-min))
        (font-lock-ensure)))

    (defun expect (text)
      (progn
        (insert "|")
        (should (string= text (thing-at-point 'line)))
        (delete-backward-char 1)))

    ;; if then
    ;; TODO: doesn't work if there's no whitespace before if
    (prepare " if foo then 1 else 2")

    (evilmi-jump-items)
    (expect " if foo |then 1 else 2")

    (evilmi-jump-items)
    (expect " |if foo then 1 else 2")

    ;; parentheses
    (prepare "let x = (1, 2) in 3")

    (evilmi-jump-items)
    (expect "let x = (1, 2|) in 3" )

    (evilmi-jump-items)
    (expect "let x = |(1, 2) in 3" )

    ;; struct end
    (prepare "module X = struct type t = int end")

    (evilmi-jump-items)
    (expect "module X = struct type t = int |end" )

    (evilmi-jump-items)
    (expect "module X = |struct type t = int end" )

    ;; TODO: begin end doesn't seem to be working
    (prepare "begin 1 end ;;")

    (evilmi-jump-items)
    (expect "|begin 1 end ;;")

    ;; match with
    (prepare "let _ = match x with _ -> ()")

    (evilmi-jump-items)
    (expect "let _ = match x |with _ -> ()" )

    (evilmi-jump-items)
    (expect "let _ = |match x with _ -> ()" )

    ;; let in
    (prepare
     "let () =\n"
     "  let x = foo in bar")

    (goto-char 10)
    (expect "|  let x = foo in bar" )

    (evilmi-jump-items)
    (expect "  let x = foo |in bar" )

    (evilmi-jump-items)
    (expect "  |let x = foo in bar" )

    (should (eq major-mode 'tuareg-mode))))

(ert-deftest evilmi-test-octave ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.m")
    (octave-mode)
    (font-lock-ensure)

    (goto-char (point-min))
    (evilmi-jump-items)
    ;; still at the same line if there is no bracket at point
    (should (string= "elseif a < 80" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "else" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "end" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "if a < 30" (evilmi-sdk-curline)))

    (should (eq major-mode 'octave-mode))))

(ert-deftest evilmi-test-simplejump-space ()
  "`evilmi-sdk-simple-jump' should skip spaces."
  (with-temp-buffer
    (insert "    {
    }")
    (goto-char (point-min))
    (evilmi-sdk-simple-jump)
    (should (= (char-after) ?}))
    (goto-line 2)
    (evilmi-sdk-simple-jump)
    (should (= (char-after) ?{))))

(ert-deftest evilmi-test-org ()
  (with-temp-buffer
    (evilmi-test-read-file "hello.org")
    (org-mode)
    (font-lock-ensure)

    (goto-char (point-min))
    (search-forward "#+begin_src")
    (evilmi-jump-items)
    (should (string= "#+end_src" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "#+begin_src javascript" (evilmi-sdk-curline)))

    (search-forward "#+begin_quote")
    (evilmi-jump-items)
    (should (string= "#+end_quote" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "#+begin_quote" (evilmi-sdk-curline)))

    ;; This is test string: "'hello world'"
    (search-forward "test quotes begin ")

    ;; test double quote
    (should (eq (following-char) 34))
    (let* ((start-pos (point)))
      (evilmi-jump-items)
      (should (eq (following-char) 34))
      (should (> (point) start-pos))
      ;; jump back
      (evilmi-jump-items)
      (should (eq (following-char) 34))
      (should (eq (point) start-pos))

      ;; test single quote
      (forward-char)
      (should (eq (following-char) 39))
      (setq start-pos (point))
      (evilmi-jump-items)
      (should (eq (following-char) 39))
      (should (> (point) start-pos))
      ;; jump back
      (evilmi-jump-items)
      (should (eq (following-char) 39))
      (should (eq (point) start-pos)))

    (should (eq major-mode 'org-mode))))

(ert-deftest evilmi-test-merge-conflict ()
  (with-temp-buffer
    (evilmi-test-read-file "conflict.js")
    (js-mode)

    (goto-char (point-min))
    (re-search-forward "^<<<<<<")
    (evilmi-jump-items)
    (should (string-match "^======" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string-match "^>>>>>>" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string-match "^<<<<<<" (evilmi-sdk-curline)))

    (should (eq major-mode 'js-mode))))

(ert-run-tests-batch-and-exit)
;;; evil-matchit-tests.el ends here
