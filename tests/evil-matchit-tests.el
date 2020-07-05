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

(ert-run-tests-batch-and-exit)
;;; evil-matchit-tests.el ends here
