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
    (message "(point)=%s" (point))
    (should (string-match "switch(c)" (evilmi-sdk-curline) ))

    (should (eq major-mode 'c-mode))))
(ert-run-tests-batch-and-exit)
;;; evil-matchit-tests.el ends here
