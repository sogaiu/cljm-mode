;;; cljm-unknown.el --- unknown -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun cljm-char-at-point ()
  "Return the char at point or nil if at buffer end."
  (when (not (= (point) (point-max)))
    (buffer-substring-no-properties (point) (1+ (point)))))

(defun cljm-char-before-point ()
  "Return the char before point or nil if at buffer beginning."
  (when (not (= (point) (point-min)))
    (buffer-substring-no-properties (point) (1- (point)))))

(defun cljm-toggle-keyword-string ()
  "Convert the string or keyword at point to keyword or string."
  (interactive)
  (let ((original-point (point)))
    (while (and (> (point) 1)
                (not (equal "\"" (buffer-substring-no-properties
                                  (point) (+ 1 (point)))))
                (not (equal ":" (buffer-substring-no-properties
                                 (point) (+ 1 (point))))))
      (backward-char))
    (cond
     ((equal 1 (point))
      (error "Beginning of file reached, this was probably a mistake"))
     ((equal "\"" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert ":" (substring (cljm-delete-and-extract-sexp) 1 -1)))
     ((equal ":" (buffer-substring-no-properties (point) (+ 1 (point))))
      (insert "\"" (substring (cljm-delete-and-extract-sexp) 1) "\"")))
    (goto-char original-point)))

(defconst cljm-def-type-and-name-regex
  (concat "(\\(?:\\(?:\\sw\\|\\s_\\)+/\\)?"
          ;; Declaration
          "\\(def\\(?:\\sw\\|\\s_\\)*\\)\\>"
          ;; Any whitespace
          "[ \r\n\t]*"
          ;; Possibly type or metadata
          "\\(?:#?^\\(?:{[^}]*}\\|\\(?:\\sw\\|\\s_\\)+\\)[ \r\n\t]*\\)*"
          ;; Symbol name
          "\\(\\(?:\\sw\\|\\s_\\)+\\)"))

(defun cljm-find-def ()
  "Find the var declaration macro and symbol name of the current form.
Returns a list pair, e.g. (\"defn\" \"abc\") or (\"deftest\" \"some-test\")."
  (save-excursion
    (unless (looking-at cljm-def-type-and-name-regex)
      (beginning-of-defun))
    (when (search-forward-regexp cljm-def-type-and-name-regex nil t)
      (list (match-string-no-properties 1)
            (match-string-no-properties 2)))))

(provide 'cljm-unknown)

;;; cljm-unknown.el ends here

