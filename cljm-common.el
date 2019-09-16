;;; cljm-common.el --- common -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun cljm--point-after (&rest actions)
  "Return POINT after performing ACTIONS.

An action is either the symbol of a function or a two element
list of (fn args) to pass to `apply''"
  (save-excursion
    (dolist (fn-and-args actions)
      (let ((f (if (listp fn-and-args) (car fn-and-args) fn-and-args))
            (args (if (listp fn-and-args) (cdr fn-and-args) nil)))
        (apply f args)))
    (point)))

(defvar cljm--let-regexp
  "\(\\(when-let\\|if-let\\|let\\)\\(\\s-*\\|\\[\\)"
  "Regexp matching let like expressions, i.e. \"let\", \"when-let\", \"if-let\".

The first match-group is the let expression.

The second match-group is the whitespace or the opening square
bracket if no whitespace between the let expression and the
bracket.")

(defun cljm--goto-let ()
  "Go to the beginning of the nearest let form."
  (when (cljm--in-string-p)
    (while (or (not (looking-at "("))
               (cljm--in-string-p))
      (backward-char)))
  (ignore-errors
    (while (not (looking-at cljm--let-regexp))
      (backward-up-list)))
  (looking-at cljm--let-regexp))

(defun cljm--replace-sexp-with-binding (bound-name init-expr)
  "Replace a binding with its bound name in the let form.

BOUND-NAME is the name (left-hand side) of a binding.

INIT-EXPR is the value (right-hand side) of a binding."
  (save-excursion
    (while (re-search-forward
            (cljm--sexp-regexp init-expr)
            (cljm--point-after 'cljm--goto-let 'forward-sexp)
            t)
      (replace-match (concat "\\1" bound-name "\\2")))))

(defun cljm--replace-sexps-with-bindings (bindings)
  "Replace bindings with their respective bound names in the let form.

BINDINGS is the list of bound names and init expressions."
  (let ((bound-name (pop bindings))
        (init-expr (pop bindings)))
    (when bound-name
      (cljm--replace-sexp-with-binding bound-name init-expr)
      (cljm--replace-sexps-with-bindings bindings))))

(defun cljm--replace-sexps-with-bindings-and-indent ()
  "Replace sexps with bindings."
  (cljm--replace-sexps-with-bindings
   (cljm--read-let-bindings))
  (cljm-indent-region
   (cljm--point-after 'cljm--goto-let)
   (cljm--point-after 'cljm--goto-let 'forward-sexp)))

(provide 'cljm-common)

;;; cljm-common.el ends here
