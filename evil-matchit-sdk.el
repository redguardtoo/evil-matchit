(require 'evil nil t)
(require 'subr-x)

(defvar evilmi-debug nil
  "Debug flag.")

(defvar evilmi-forward-chars (string-to-list "[{("))
(defvar evilmi-backward-chars (string-to-list "]})"))
(defvar evilmi-quote-chars (string-to-list "'\"/"))

(defvar evilmi-ignored-fonts
  '(web-mode-html-attr-value-face
    font-lock-string-face
    font-lock-doc-face
    font-lock-comment-face)
  "Text with ignored fonts has no string keyword.")

(defvar evilmi-sdk-extract-keyword-howtos
  '(("^[ \t]*\\([a-z]+\!?\\)\\( .*\\| *\\)$" 1)
    ("^.* \\(do\\) |[a-z0-9A-Z,|]+|$" 1))
  "The list of howto on extracting keyword from current line.
Each howto is actually a pair. The first element of pair is the regular
expression to match the current line. The second is the index of sub-matches
to extract the keyword which starts from one.  The sub-match is the match defined
between '\\(' and '\\)' in regular expression.")

(defun evilmi-sdk-keyword (info)
  "Get keyword from INFO."
  (nth 3 info))

(defun evilmi-sdk-get-char (position)
  "Get character at POSITION."
  (char-after position))

(defun evilmi-sdk-guess-jump-direction-of-quote (ch ff)
  "Guess jump direction by quote character CH and font face FF.
Return t if jump forward."
  (cond
   (ff
    (eq ff (get-text-property (+ 1 (point)) 'face)))

   (t
    (let* ((i 0)
           (cnt 0)
           (str (buffer-substring (point-min) (point)))
           (len (length str)))
      ;; count quote character
      (while (< i len)
        (when (eq ch (seq-elt str i))
          (setq cnt (1+ cnt)))
        (setq i (1+ i)))
      (eq (% cnt 2) 0)))))

(defun evilmi-sdk-jump-forward-p ()
  "Return: (forward-direction font-face-under-cursor character-under-cursor).
If font-face-under-cursor is NOT nil, the quoted string is being processed."
  (let* ((ch (following-char))
         ff ; font-face
         (rlt t))
    (cond
     ((memq ch evilmi-backward-chars)
      (setq rlt nil))

     ((and (memq ch evilmi-quote-chars))
      (setq ff (get-text-property (point) 'face))
      (setq rlt (evilmi-sdk-guess-jump-direction-of-quote ch ff))))

    (if evilmi-debug (message "evilmi-sdk-jump-forward-p => (%s %s %s)" rlt ff (string ch)))
    (list rlt ff ch)))

(defun evilmi-sdk-the-other-quote-char (font-face is-forward char)
  "The end character under cursor has different font from FONT-FACE."
  (let* (rlt
         got
         (delta (if is-forward 1 -1))
         (pos (+ delta (point)))
         (end (if is-forward (point-max) (point-min))))
    (while (not got)
      (cond
       ((or (= pos end)
            (and (= char (evilmi-sdk-get-char (- pos delta)))
                 (not (eq font-face (get-text-property pos 'face)))))
        (setq rlt (if is-forward pos (+ 1 pos)))
        (setq got t))
       (t
        (setq pos (+ delta pos)))))
    (if evilmi-debug (message "evilmi-sdk-the-other-quote-char called Return: %s" rlt))
    rlt))

(defun evilmi-sdk-comment-p (pos)
  "Check character at POS is comment by comparing font face."
  (cond
   ;; @see https://github.com/redguardtoo/evil-matchit/issues/92
   ((eq major-mode 'tuareg-mode)
    (evilmi-among-fonts-p pos '(font-lock-comment-face
                                font-lock-comment-delimiter-face
                                font-lock-doc-face)))
   (t
    (evilmi-among-fonts-p pos '(font-lock-comment-face
                                font-lock-comment-delimiter-face)))))

(defun evilmi-sdk-scan-sexps (is-forward character)
  "Get the position of matching tag with CHARACTER at point.
If IS-FORWARD is t, jump forward; or else jump backward."
  (if evilmi-debug (message "evilmi-sdk-scan-sexps called => (%s)" is-forward character))
  (let* ((start-pos (if is-forward (point) (+ 1 (point))))
         (arg (if is-forward 1 -1))
         (limit (if is-forward (point-max) (point-min)))
         (lvl 1)
         (dest-ch (cond
                     ;; {}
                     ((= character 123) 125)
                     ((= character 125) 123)
                     ;; ()
                     ((= character 40) 41)
                     ((= character 41) 40)
                     ;; []
                     ((= character 91) 93)
                     ((= character 93) 91)))
         (rlt start-pos))

    (cond
     ;; search another quote character
     ((memq character evilmi-quote-chars)
      (save-excursion
        (setq start-pos (if is-forward (1+ (point)) (1- (point))))
        (goto-char start-pos)
        (while (and (not (= start-pos limit))
                    (not (eq (following-char) character)))
          (goto-char (setq start-pos (+ start-pos arg))))
        (when (eq (following-char) character)
          (setq rlt (+ start-pos (if is-forward 1 0))))))

     ((evilmi-sdk-comment-p (point))
      ;; Matching tag in comment.
      ;; Use own algorithm instead of `scan-sexps'
      ;; because `scan-sexps' does not work in some major modes
      (save-excursion
        (setq start-pos (point))
        (while (and dest-ch (not (= start-pos limit)) (> lvl 0))
          (goto-char (setq start-pos (+ start-pos arg)))
          (when (evilmi-sdk-comment-p start-pos)
            (cond
             ((= (following-char) character)
              (setq lvl (1+ lvl)))
             ((= (following-char) dest-ch)
              (setq lvl (1- lvl))))))
        (when (= lvl 0)
          (setq rlt (+ start-pos (if is-forward 1 0))))))

     (t
      ;; jump inside code and ignore comments
      (let* ((parse-sexp-ignore-comments t))
        (setq rlt (scan-sexps start-pos arg)))))

    (when evilmi-debug
      (message "evilmi-sdk-scan-sexps => rlt=%s lvl=%s is-forward=%s" rlt lvl is-forward))
    rlt))

(defun evilmi-sdk-adjust-jumpto (is-forward rlt)
  ;; normal-state hack!
  (when (and (not (eq evil-state 'visual)) rlt is-forward)
    (setq rlt (1- rlt)))
  (if evilmi-debug (message "evilmi-sdk-adjust-jumpto => is-forward=%s rlt=%s" is-forward rlt))
  rlt)

;; @see http://emacs.stackexchange.com/questions/13222/a-elisp-function-to-jump-between-matched-pair
(defun evilmi-sdk-jumpto-where (ff is-forward ch)
  "Use font face FF, jump direction IS-FORWARD and character CH to jump."
  (if evilmi-debug (message "evilmi-sdk-jumpto-where => %s %s %s" ff is-forward ch))
  (let* ((dst (if ff (evilmi-sdk-the-other-quote-char ff is-forward ch)
                (evilmi-sdk-scan-sexps is-forward ch)))
         (rlt (evilmi-sdk-adjust-jumpto is-forward dst)))
    (if evilmi-debug (message "dst=%s rlt=%s" dst rlt))
    rlt))

(defun evilmi-sdk-tweak-selected-region (font-face jump-forward)
  "Tweak selected region using FONT-FACE and JUMP-FORWARD."
  ;; visual-state hack!
  (when (and jump-forward (eq evil-state 'visual) (not font-face))
    ;; if font-face is non-nil, I control the jump flow from character level,
    ;; so hack to workaround scan-sexps is NOT necessary
    (evil-backward-char)))

(defun evilmi-sdk-skip-whitespace ()
  "Skip whitespace characters at point."
  (let ((old (point)))
    (skip-syntax-forward " ")
    ;; If we move from a non-comment to before a comment,
    ;; `evilmi-sdk-jumpto-where' wont skip it:
    ;;
    ;; <point> /* comment */ {}
    ;;
    ;; Is skipped because we go back, but wouldn't be if we didn't (due to
    ;; checking for `evilmi-sdk-comment-p').
    (when (and (not (evilmi-sdk-comment-p old)) (evilmi-sdk-comment-p (point)))
      (goto-char old))))

(defun evilmi-sdk-simple-jump ()
  "Alternative for `evil-jump-item'."
  (if evilmi-debug (message "evilmi-sdk-simple-jump called (point)=%d" (point)))

  (evilmi-sdk-skip-whitespace)

  (let* ((tmp (evilmi-sdk-jump-forward-p))
         (jump-forward (car tmp))
         ;; if ff is not nil, it's jump between quotes
         ;; so we should not use (scan-sexps)
         (ff (nth 1 tmp))
         (ch (nth 2 tmp))
         (dst (evilmi-sdk-jumpto-where ff jump-forward ch)))
    (when dst
      (goto-char dst)
      (evilmi-sdk-tweak-selected-region ff jump-forward))))

(defun evilmi-sdk-strictly-type-p (crt orig)
  (or (evilmi-sdk-monogamy-p crt) (evilmi-sdk-monogamy-p orig)))

(defun evilmi-sdk-tags-matched-p (level orig-tag-info cur-tag-info match-tags)
  (let* (rlt
         (orig-keyword (evilmi-sdk-keyword orig-tag-info))
         (cur-keyword (evilmi-sdk-keyword cur-tag-info))
         (orig-row-idx (nth 0 orig-tag-info))
         (cur-row-idx (nth 0 cur-tag-info))
         (orig-type (nth 1 orig-tag-info))
         (cur-type (nth 1 cur-tag-info)))
    ;; handle function exit point
    (when (= 1 level)
      ;; end tag could be the same
      (if (and (evilmi-sdk-strictly-type-p cur-tag-info orig-tag-info)
               (not (evilmi-sdk-exactly-same-type-p cur-tag-info orig-tag-info)))
          ;; just pass
          (setq rlt nil)
        (cond
         ((and (< orig-type 2) (= cur-type 2))
          (setq rlt (evilmi-sdk-member cur-keyword (nth 2 (nth orig-row-idx match-tags)))))

         ((and (< cur-type 2) (= orig-type 2))
          (setq rlt (evilmi-sdk-member orig-keyword (nth 2 (nth cur-row-idx match-tags)))))

         (t
          (setq rlt (= (nth 0 orig-tag-info) (nth 0 cur-tag-info)))))))
    rlt))

;;;###autoload
(defun evilmi-sdk-curline ()
  "Get current line text."
  (let* ((inhibit-field-text-motion t))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

(defun evilmi-sdk-text-before-current-line ()
  "Text before current line."
  (buffer-substring-no-properties (point-min) (line-beginning-position)))

;;;###autoload
(defun evilmi-sdk-member (keyword keyword-list)
  "Check if KEYWORD exist in KEYWORD-LIST."
  (let* (rlt)
    (cond
     ((or (not keyword) (not keyword-list))
      (setq rlt nil))

     ((stringp keyword-list)
      (setq rlt (string-match (concat "^" keyword-list "$") keyword)))

     ((stringp (car keyword-list))
      (unless (setq rlt (string-match (concat "^" (car keyword-list) "$") keyword))
        (setq rlt (evilmi-sdk-member keyword (cdr keyword-list)))))

     ((listp (car keyword-list))
      (unless (setq rlt (evilmi-sdk-member keyword (car keyword-list)))
        (setq rlt (evilmi-sdk-member keyword (cdr keyword-list)))))

     (t
      ;; just ignore first element
      (setq rlt (evilmi-sdk-member keyword (cdr keyword-list)))))

    (when (and evilmi-debug rlt)
      (message "evilmi-sdk-member called => %s %s. rlt=%s" keyword keyword-list rlt))
    rlt))


;;;###autoload
(defun evilmi-sdk-get-tag-info (keyword match-tags)
  "Return (row column is-function-exit-point keyword).
The row and column mark the position in `evilmi-mylang-match-tags'
is-function-exit-point could be unknown status"
  (let* (rlt
         items
         item
         found
         (i 0)
         j)

    (while (and (< i (length match-tags)) (not found))
      (setq items (nth i match-tags))
      (setq j 0)
      (while (and (not found) (< j (length items)))
        (setq item (nth j items))
        (setq found (and (or (stringp item) (listp item))
                         (evilmi-sdk-member keyword item)))
        (unless found (setq j (1+ j))))
      (unless found (setq i (1+ i))))
    (when found
      ;; function exit point maybe?
      (if (nth 3 (nth i match-tags))
          (setq rlt (list i
                          j
                          (nth 3 (nth i match-tags))
                          keyword))
        (setq rlt (list i j nil keyword))))
    (if evilmi-debug (message "evilmi-sdk-get-tag-info called => %s %s. rlt=%s" keyword match-tags rlt))
    rlt))

(defun evilmi--sdk-check-keyword (keyword begin end)
  "KEYWORD has valid keyword font space between BEGIN and END."
  (let* (rlt)
    (save-excursion
      (goto-char begin)
      (while (search-forward keyword end t)
        (when (not (evilmi-among-fonts-p (point)
                                         evilmi-ignored-fonts))
          (setq rlt keyword))))
    rlt))

(defun evilmi--sdk-extract-keyword (cur-line match-tags howtos)
  "Extract keyword from CUR-LINE.  Keyword is defined in MATCH-TAGS.
Rule is looked up in HOWTOS."
  (let* (keyword howto (i 0))
    (while (and (not keyword) (< i (length howtos)))
      (setq howto (nth i howtos))
      (when (string-match (nth 0 howto) cur-line)
        ;; keyword should be trimmed because FORTRAN use "else if"
        (setq keyword (string-trim (match-string (nth 1 howto) cur-line)))
        ;; keep search keyword by using next howto (regex and match-string index)
        (unless (evilmi-sdk-member keyword match-tags) (setq keyword nil)))
      (setq i (1+ i)))

    (and keyword
         (evilmi--sdk-check-keyword keyword
                                    (line-beginning-position)
                                    (line-end-position)))))

(defun evilmi-sdk-monogamy-p (tag-info)
  (and (nth 2 tag-info) (string= (nth 2 tag-info) "MONOGAMY")))

(defun evilmi-sdk-exactly-same-type-p (crt orig)
  (eq (nth 0 crt) (nth 0 orig)))

(defun evilmi-sdk-same-type (crt orig)
  (when (and crt orig)
    ;; crt and orig should be at same row if either of them is monogamy
    (if (evilmi-sdk-strictly-type-p crt orig)
        (evilmi-sdk-exactly-same-type-p crt orig)
      t)))

;;;###autoload
(defun evilmi-sdk-get-tag (match-tags howtos)
  "Return '(start-point ((row column is-function-exit-point keyword))."
  (let* ((cur-line (evilmi-sdk-curline))
         (keyword (evilmi--sdk-extract-keyword cur-line match-tags howtos))
         (tag-info (if keyword (evilmi-sdk-get-tag-info keyword match-tags))))

    ;; since we mixed ruby and lua mode here
    ;; maybe we should be strict at the keyword
    (and tag-info
      ;; 0 - open tag; 1 - middle tag; 2 - close tag;
         (list (if (= 2 (nth 1 tag-info))
                   (line-end-position)
                 (line-beginning-position))
               tag-info))))

;;;###autoload
(defun evilmi-sdk-jump (rlt num match-tags howtos)
  "Use RLT, NUM, MATCH-TAGS and HOWTOS to jump.
Return nil if no matching tag found.  Please note (point) is changed
after calling this function."
  (let* ((orig-tag-info (nth 1 rlt))
         (orig-tag-type (nth 1 orig-tag-info))
         cur-tag-type
         cur-tag-info
         (level 1)
         (cur-line (evilmi-sdk-curline))
         keyword
         found
         ideal-dest)
    (if evilmi-debug (message "evilmi-sdk-jump called => rlt=%s (point)=%s" rlt (point)))

    (while (not found)
      (forward-line (if (= orig-tag-type 2) -1 1))
      (setq cur-line (evilmi-sdk-curline))
      (setq keyword (evilmi--sdk-extract-keyword cur-line match-tags howtos))
      (if evilmi-debug (message "keyword=%s cur-line=%s" keyword cur-line))

      (when keyword
        (setq cur-tag-info (evilmi-sdk-get-tag-info keyword match-tags))
        (when (evilmi-sdk-same-type cur-tag-info orig-tag-info)
          (setq cur-tag-type (nth 1 cur-tag-info))

          ;; key algorithm
          (cond
           ;; handle open tag
           ;; open (0) -> mid (1)  found when level is one else ignore
           ((and (= orig-tag-type 0) (= cur-tag-type 1))
            (when (evilmi-sdk-tags-matched-p level orig-tag-info cur-tag-info match-tags)
              (back-to-indentation)
              (setq ideal-dest (1- (line-beginning-position)))
              (setq found t)))

           ;; open (0) -> closed (2) found when level is zero, level--
           ((and (= orig-tag-type 0) (= cur-tag-type 2))
            (when (evilmi-sdk-tags-matched-p level orig-tag-info cur-tag-info match-tags)
              (goto-char (line-end-position))
              (setq ideal-dest (line-end-position))
              (setq found t))
            (setq level (1- level)))

           ;; open (0) -> open (0) level++
           ((and (= orig-tag-type 0) (= cur-tag-type 0))
            (setq level (1+ level)))

           ;; now handle mid tag
           ;; mid (1) -> mid (1) found if:
           ;;   1. level is one
           ;;   2. the open tag and middle tag are in the same row in evilmi-mylang-match-tags
           ;; else: just ignore
           ;; level is one means we are not in some embedded loop/conditional statements
           ((and (= orig-tag-type 1) (= cur-tag-type 1))

            (when (evilmi-sdk-tags-matched-p level orig-tag-info cur-tag-info match-tags)
              (back-to-indentation)
              (setq ideal-dest (1- (line-beginning-position)))
              (setq found t)))

           ;; mid (1) -> closed (2) found when level is zero, level --
           ((and (= orig-tag-type 1) (= cur-tag-type 2))
            (when (evilmi-sdk-tags-matched-p level orig-tag-info cur-tag-info match-tags)
              (goto-char (line-end-position))
              (setq ideal-dest (line-end-position))
              (setq found t))
            (setq level (1- level)))

           ;; mid (1) -> open (0) level++
           ((and (= orig-tag-type 1) (= cur-tag-type 0))
            (setq level (1+ level)))

           ;; now handle closed tag
           ;; closed (2) -> mid (1) ignore,impossible
           ((and (= orig-tag-type 2) (= cur-tag-type 1)))

           ;; closed (2) -> closed (2) level++
           ((and (= orig-tag-type 2) (= cur-tag-type 2))
            (setq level (1+ level)))

           ;; closed (2) -> open (0) found when level is zero, level--
           ((and (= orig-tag-type 2) (= cur-tag-type 0))
            (when (evilmi-sdk-tags-matched-p level orig-tag-info cur-tag-info match-tags)
              (setq ideal-dest (line-beginning-position))
              (back-to-indentation)
              (setq found t))
            (setq level (1- level)))

           (t (message "why here?")))))

      ;; we will stop at end or beginning of buffer anyway
      (if (or (= (line-end-position) (point-max))
              (= (line-beginning-position) (point-min)))
          (setq found t)))

    ideal-dest))


;;;###autoload
(defun evilmi-among-fonts-p (pos fonts)
  "If current font at POS is among FONTS."
  (let* ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
          (mapcar (lambda (f)
                    (member f fonts))
                  fontfaces))))

(defun evilmi-next-non-empty-line ()
  "Return next non-empty line content or nil."
  (let* ((b (line-beginning-position))
         (e (line-end-position))
         (cur-pos (point))
         (continue t)
         line
         rlt)
    (save-excursion
      (forward-line)
      (while (and continue (> (point) e))
        (setq line (evilmi-sdk-curline))
        (cond
         ((string-blank-p line)
          (setq b (line-beginning-position))
          (setq e (line-end-position))
          (forward-line))
         (t
          (setq continue nil)
          (setq rlt line)))))
    rlt))

(defun evilmi-sdk-count-matches (regexp str)
  "Count match of REGEXP in STR."
  (let* ((count 0)
         (start 0))
    (while (string-match regexp str start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun evilmi-sdk-n-lines (n)
  "Return content of N lines from current position."
  (unless (and n (> n 1)) (setq n 1))
  (let* (beg end str)
    (save-excursion
      (setq beg (line-beginning-position))
      (forward-line (1- n))
      (setq end (line-end-position)))
    (cons beg end)))

(provide 'evil-matchit-sdk)
