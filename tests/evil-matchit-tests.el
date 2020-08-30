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

(require 'ert)
(require 'evil-matchit)

(setq evilmi-may-jump-by-percentage nil)
(setq evilmi-debug nil) ; debug

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
    (insert  "function hello() {\n  console.log('hello world');\n}")
    (js-mode)
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

    ;; jump between ends of string can't be tested.
    ;; because font face is not useable in batch mode

    (should (eq major-mode 'js-mode))))

(ert-deftest evilmi-test-html ()
  (with-temp-buffer
    (insert  "<html lang=\"en\">\n<head>\n<link rel=\"icon\" href=\"%PUBLIC_URL%/favicon.ico\" />\n</head>\n<body>\n<p>Hello world!</p>\n</body>\n</html>")
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
    (insert "#ifdef CONFIG_COMPAT\n#ifndef TEST1\nstruct mtip_s {\n  int v1;\n}\n#endif\n#endif\n"
            "static int fn1()\n{\nprintf(\"hello world\");\n}\nint a = 3;\n"
            "switch(c) {\ncase 'a':\nbreak;\ncase 'b':\nbreak;\n}\n")
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
    (insert "if configTable:FindFirstChild(configName) then\n"
            "    configs[configName] = configTable:FindFirstChild(configName).Value\n"
            "else\n"
            "    configs[configName] = defaultValue\n"
            "end\n"
            "\n"
            "local thread = coroutine.create(function()\n"
            "    while true do\n"
            "        wait()\n"
            "        if state then\n"
            "            display.Text = state.Name\n"
            "        end\n"
            "    end\n"
            "end)\n")
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
    (insert "diff 1\n"
            "index abed2b7\n"
            "--- a/1.c\n"
            "+++ b/1.c\n"
            "line1\n"
            "line2\n"
            "diff 2"
            "index abed2b7\n"
            "--- a/2.c\n"
            "+++ b/2.c\n"
            "line3\n"
            "line4")
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
    (insert "--- a/1.c\n"
            "+++ b/1.c\n"
            "line1\n"
            "line2\n"
            "--- a/2.c\n"
            "+++ b/2.c\n"
            "line3\n"
            "line4")
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
    (insert "PROGRAM cows\n"
            "IMPLICIT NONE\n"
            "INTEGER :: func_name\n"
            "PRINT *,func_name(2, 1.3)\n"
            "END PROGRAM\n")
    (f90-mode)

    (goto-char (point-min))
    ;; jump to end tag
    (evilmi-jump-items)
    (should (string= "END PROGRAM" (evilmi-sdk-curline)))
    ;; jump back to open tags
    (evilmi-jump-items)
    (should (string= "PROGRAM cows" (evilmi-sdk-curline)))

    ;; lower case conditional statement
    (erase-buffer)
    (insert "if (x < x1) then\n"
            "  print 1\n"
            "else\n"
            "  print 2\n"
            "end if\n")
    (goto-char (point-min))
    (evilmi-jump-items)
    (should (string= "else" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "end if" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "if (x < x1) then" (evilmi-sdk-curline)))

    ;; upper case conditional statement
    (erase-buffer)
    (insert "IF (x < 50) THEN\n"
            "   Grade = 'F'\n"
            "ELSE IF (x < 60) THEN\n"
            "   Grade = 'D'\n"
            "ELSE IF (x < 70) THEN\n"
            "   Grade = 'C'\n"
            "ELSE\n"
            "   Grade = 'A'\n"
            "END IF\n")
    (goto-char (point-min))
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
    (insert "`ifdef behavioral\n"
            " `include \"groupA_beh.v \";\n"
            " `include \"groupB_beh.v \";\n"
            " `include \"ctrl_beh.v \";\n"
            "`else\n"
            " `include \"groupA_synth.v \";\n"
            " `include \"groupB_ synth.v \";\n"
            " `include \"ctrl_ synth.v \";\n"
            "`endif\n"
            )
    (verilog-mode)

    (goto-char (point-min))
    (evilmi-jump-items)
    (should (string= "`else" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "`endif" (evilmi-sdk-curline)))
    (evilmi-jump-items)
    (should (string= "`ifdef behavioral" (evilmi-sdk-curline)))

    (erase-buffer)
    (insert "always @(WRITE or READ or STATUS) // test \n"
            " /* hello */"
            "  begin\n"
            "  out = 9;\n"
            "  end\n"
            " // more comment\n")
    (goto-char (point-min))
    (evilmi-jump-items)
    (should (string= "end" (thing-at-point 'symbol)))

    (should (eq major-mode 'verilog-mode))))

(ert-deftest evilmi-test-markdown ()
  (with-temp-buffer
    (insert "### test1\n"
            "```java\n"
            "Example 1\n"
            "```\n\n"
            "### test2\n"
            "```c\n"
            "Example 2\n"
            "```\n")
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
    (insert ";; test1\n"
            ";; {{\n"
            ";; java\n"
            ";; }}\n"
            "test\n")
    (emacs-lisp-mode)
    (when (fboundp 'font-lock-ensure)
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
      (should (string= ";; {{" (evilmi-sdk-curline))))

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

    (when (fboundp 'font-lock-ensure)
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
      (expect "  |let x = foo in bar" ))

    (should (eq major-mode 'tuareg-mode))))

(ert-run-tests-batch-and-exit)
;;; evil-matchit-tests.el ends here
