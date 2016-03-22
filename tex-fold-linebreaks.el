;;; tex-fold-linebreaks.el --- Fold away linebreaks with TeX-fold

;; Copyright (C) 2015 Anders Johansson

;; Author: Anders Johansson <mejlaandersj@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (auctex "11.86"))
;; Keywords: tex, wp
;; URL: http://github.com/andersjohansson/tex-fold-linebreaks

;;; Commentary:

;; Defines `tex-fold-linebreaks-mode', a minor-mode which hooks into
;; TeX-fold and folds away linebreaks after ".", "?", and "!" (or others).
;; This makes files using the convention of linebreak after each sentence
;; display in a nice way.
;;
;; It also rebinds the punctuation characters to insert a linebreak
;; if not in certain positions matched by
;; `tex-fold-linebreaks-non-sentence-punctuation' or preceded by a
;; prefix arg"

(require 'tex-fold)

(defcustom tex-fold-linebreaks-non-sentence-punctuation-regexp 
  "\\([0-9]\\|[[:space:]]\\([[:alpha:]]\\|e\\.g\\|i\\.e\\)?\\)$"
  "Regexp for text before punctuation which doesn't end a sentence.

Should match constructs after which a period (or punctuation
defined in `tex-fold-linebreaks-sentence-end-punctuation') would
normally not imply the end of a sentence. If this matches before
an inserted punctuation character
`tex-fold-linebreaks-insert-punctuation' avoids inserting an
extra line break.

By default matches numbers, spaces, spaces followed by single
letters, and the abbreviations e.g. and i.e."
  :type 'regexp
  :group 'TeX-fold)

(defcustom tex-fold-linebreaks-sentence-end-punctuation
  '(("." . "⁎")
    ("?" . "❓")
    ("!" . "❗"))
  "Punctuation characters viewed as ending a sentence.
An alist of the characters (can be strings) that should be viewed
as ending a sentence. Car of each element is the character (a
string) and cdr a character that will be used for to replace
it (with an overlay, not written to any file) when folded. By
default these are decorative unicode-variants of .?! to make them
stand out."
  :type '(alist :key-type string :value-type string)
  :group 'TeX-fold)

(defcustom tex-fold-linebreaks-rebind-characters t
  "Whether punctuation characters should insert a linebreak.

If non-nil the characters defined in
`tex-fold-linebreaks-end-sentence-punctuation' is rebound to add
a linebreak in `tex-fold-linebreaks-mode'"
  :type '(choice (const :tag "Enable" t) (const :tag "Disable" nil))
  :group 'TeX-fold)


;;;###autoload
(define-minor-mode tex-fold-linebreaks-mode
  "Minor mode for folding linebreaks when using `TeX-fold-mode',

When enabled `TeX-fold-region' and commands calling it
additionally fold all linebreaks after the characters defined in
`tex-fold-linebreaks-sentence-end-punctuation' making files using
the convention of a linebreak after each sentence display nicely.

It also rebinds the punctuation characters to insert a linebreak
if not in certain positions matched by
`tex-fold-linebreaks-non-sentence-punctuation' or preceded by a
prefix arg"

  ;; we hack minor-mode-map-alist instead
  ;;  :keymap 'tex-fold-linebreaks-mode-map
  :lighter "tfl"
  (if tex-fold-linebreaks-mode
      (progn
        (when tex-fold-linebreaks-rebind-characters
          (tex-fold-linebreaks--redefine-keymap))
        (advice-add 'TeX-fold-region :before
                    #'tex-fold-linebreaks--fold-region-linebreaks))
    (advice-remove 'TeX-fold-region
                   #'tex-fold-linebreaks--fold-region-linebreaks)))

(defun tex-fold-linebreaks--redefine-keymap ()
  "Updates the `tex-fold-linebreaks-mode' map in
  `minor-mode-map-alist' based on
  `tex-fold-linebreaks-sentence-end-punctuation'"
  (setq minor-mode-map-alist
        (assq-delete-all tex-fold-linebreaks-mode minor-mode-map-alist))
  (add-to-list
   'minor-mode-map-alist
   (cons 'tex-fold-linebreaks-mode
         (easy-mmode-define-keymap
          (mapcar
           (lambda (c) (when (= 1 (length (car c)))
                    (cons (kbd (car c)) 'tex-fold-linebreaks-insert-punctuation)))
           tex-fold-linebreaks-sentence-end-punctuation)))))



(defun tex-fold-linebreaks--search-forward-linebreak-start (&optional limit)
  "Search forward for a linebreak start from point till LIMIT.
If LIMIT is omitted, search till the end of the buffer."
  (let ((limit (or limit (point-max)))
        (regexp (tex-fold-linebreaks--sentence-end-regexp)))
    (when (TeX-re-search-forward-unescaped regexp limit t)
      (match-beginning 0))))

(defun tex-fold-linebreaks--sentence-end-regexp ()
  "Return regexp matching end of line preceded by any of the
strings in `tex-fold-linebreaks-sentence-end-punctuation'."
  (concat "\\("
          (regexp-opt
           (mapcar (lambda (c) (car c)) tex-fold-linebreaks-sentence-end-punctuation) t)
          "\n?\\)\n"))

(defun tex-fold-linebreaks--fold-region-linebreaks (start end)
  "Fold all line breaks after sentence end characters defined in
  `tex-fold-linebreaks-sentence-end-punctuation' in region from
  START to END"
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let (beg)
      (while (setq beg (tex-fold-linebreaks--search-forward-linebreak-start end))
        (goto-char beg)
        (let* ((ms1 (match-string 1))
               (ms2 (match-string 2))
               (r1 (cdr (assoc ms1 tex-fold-linebreaks-sentence-end-punctuation)))
               (rep (concat
                     r1
                     (if (string= ms1 ms2)
                         " " ;normal sentence end
                       "\n\t")))) ; we had \n\n, represent it as indent
          (goto-char (match-end 0))
          ;; Hide the whole region if we are not at some specific places
          (unless (looking-at-p "\\\\\\(begin\\|end\\|item\\)")
            (TeX-fold-hide-item (TeX-fold-make-overlay beg (point) 'comment rep))))))))

;; TODO, is it more reasonable to use syntax tables and check for word endings?
(defun tex-fold-linebreaks-insert-punctuation (&optional arg)
  "Insert linebreak along with calling character.
Bound to characters defined in
`tex-fold-linebreaks-sentence-end-punctuation' in
`tex-fold-linebreaks-mode'. Linebreak is inserted if point is not
preceded by `tex-fold-linebreaks-non-sentence-punctuation-regexp'
or given prefix ARG."
  (interactive "P")
  (let ((dontbreak
         (or arg (looking-back tex-fold-linebreaks-non-sentence-punctuation-regexp 7))))
	(insert (this-command-keys))
	(unless dontbreak (TeX-newline))))


(provide 'tex-fold-linebreaks)

;;; tex-fold-linebreaks.el ends here
