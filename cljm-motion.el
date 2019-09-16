;;; cljm-motion.el --- cljm motion code -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'newcomment)
(require 'subr-x)

(defun cljm--looking-at-non-logical-sexp ()
  "Return non-nil if text after point is \"non-logical\" sexp.
\"Non-logical\" sexp are ^metadata and #reader.macros."
  (comment-normalize-vars)
  (comment-forward (point-max))
  (looking-at-p "\\^\\|#:?:?[[:alpha:]]"))

(defun cljm-forward-logical-sexp (&optional n)
  "Move forward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (cljm-backward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        (while (cljm--looking-at-non-logical-sexp)
          (forward-sexp 1))
        ;; The actual sexp
        (forward-sexp 1)
        (skip-chars-forward ",")
        (setq n (1- n))))))

(defun cljm-backward-logical-sexp (&optional n)
  "Move backward N logical sexps.
This will skip over sexps that don't represent objects, so that ^hints and
#reader.macros are considered part of the following sexp."
  (interactive "p")
  (unless n (setq n 1))
  (if (< n 0)
      (cljm-forward-logical-sexp (- n))
    (let ((forward-sexp-function nil))
      (while (> n 0)
        ;; The actual sexp
        (backward-sexp 1)
        ;; Non-logical sexps.
        (while (and (not (bobp))
                    (ignore-errors
                      (save-excursion
                        (backward-sexp 1)
                        (cljm--looking-at-non-logical-sexp))))
          (backward-sexp 1))
        (setq n (1- n))))))

(defcustom cljm-toplevel-inside-comment-form nil
  "Eval top level forms inside comment forms instead of the comment form itself.
Experimental.  Function `cider-defun-at-point' is used extensively so if we
change this heuristic it needs to be bullet-proof and desired.  While
testing, give an easy way to turn this new behavior off."
  :type 'boolean
  :safe #'booleanp)

(defun cljm-find-first (pred coll)
  "Find first element of COLL for which PRED return truthy."
  (let ((found)
        (haystack coll))
    (while (and (not found)
                haystack)
      (if (funcall pred (car haystack))
          (setq found (car haystack))
        (setq haystack (cdr haystack))))
    found))

(defun cljm-sexp-starts-until-position (position)
  "Return the starting points for forms before POSITION.
Positions are in descending order to aide in finding the first starting
position before the current position."
  (save-excursion
    (let (sexp-positions)
      (condition-case nil
          (while (< (point) position)
            (cljm-forward-logical-sexp 1)
            (cljm-backward-logical-sexp 1)
            (push (point) sexp-positions)
            (cljm-forward-logical-sexp 1))
        (scan-error nil))
      sexp-positions)))

(defun cljm-top-level-form-p (first-form)
  "Return truthy if the first form matches FIRST-FORM."
  (condition-case nil
      (save-excursion
        (beginning-of-defun)
        (forward-char 1)
        (cljm-forward-logical-sexp 1)
        (cljm-backward-logical-sexp 1)
        (looking-at-p first-form))
    (scan-error nil)
    (end-of-buffer nil)))

(defun cljm-beginning-of-defun-function (&optional n)
  "Go to top level form.
Set as `beginning-of-defun-function' so that these generic
operators can be used.  Given a positive N it will do it that
many times."
  (let ((beginning-of-defun-function nil))
    (if (and cljm-toplevel-inside-comment-form
             (cljm-top-level-form-p "comment"))
        (condition-case nil
            (save-match-data
              (let ((original-position (point))
                    cljm-comment-end)
                (beginning-of-defun)
                (end-of-defun)
                (setq cljm-comment-end (point))
                (beginning-of-defun)
                (forward-char 1)          ;; skip paren to start at comment
                (cljm-forward-logical-sexp) ;; skip the comment form itself
                (if-let
                    ((sexp-start
                      (cljm-find-first (lambda (beg-pos)
                                         (< beg-pos original-position))
                                       (cljm-sexp-starts-until-position
                                        cljm-comment-end))))
                    (progn (goto-char sexp-start) t)
                  (beginning-of-defun n))))
          (scan-error (beginning-of-defun n)))
      (beginning-of-defun n))))

(provide 'cljm-motion)

;;; cljm-motion.el ends here
