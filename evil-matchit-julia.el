;;; evil-matchit-julia.el --- julia plugin of evil-matchit -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026 Chen Bin

;; Author: Chen Bin

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

;;; Code:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'evil-matchit-sdk)

(defvar evilmi-julia-match-tags
  '((("if") ("elseif" "else") ("end"))

    (("do" "begin" "quote" "function" "let" "macro" "while" "for" "module" "baremodule" "struct" "mutable") () ("end"))

    ;; try / catch / finally / end
    (("try") ("catch" "finally") ("end")))
  "Match tags for Julia language blocks.")

(defvar evilmi-julia-extract-keyword-howtos
  '(("^[ \t]*\\(if\\|mutable\\|struct\\|try\\|catch\\|finally\\|begin\\|quote\\|elseif\\|else\\|for\\|while\\|function\\|macro\\|module\\|baremodule\\|let\\)\\b" 1)
    ("^[ \t]*.*[ \t]\\(do\\)\\([ \t]*$\\|[ \t]\\)" 1)
    ("^[ \t]*\\(end\\)\\([ \t]*$\\|[ \t]*#\\)" 1))

  "Regex rules to extract Julia block keywords.")
;;;###autoload
(defun evilmi-julia-get-tag ()
  "Get tag at point."
  (let* ((tag (evilmi-sdk-get-tag evilmi-julia-match-tags evilmi-julia-extract-keyword-howtos)))
    (when (and evilmi-debug tag)
      (message "evilmi-julia-get-tag called. tag=%s" tag))
    tag))

;;;###autoload
(defun evilmi-julia-jump (info num)
  "Use INFO returned by `evilmi-julia-get-tag' and NUM to jump to matched tag."
  (evilmi-sdk-jump info num evilmi-julia-match-tags evilmi-julia-extract-keyword-howtos))

(provide 'evil-matchit-julia)
;;; evil-matchit-julia.el ends here