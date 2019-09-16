;;; cljm-refactor.el --- refactor -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cljm-cache)
(require 'cljm-common)
(require 'cljm-indent)
(require 'cljm-motion)
(require 'cljm-project)

(defcustom cljm-refactor-map-prefix (kbd "C-c C-r")
  "Clojure refactor keymap prefix."
  :type 'string)

(defvar cljm-refactor-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-t") #'cljm-thread)
    (define-key map (kbd "t") #'cljm-thread)
    (define-key map (kbd "C-u") #'cljm-unwind)
    (define-key map (kbd "u") #'cljm-unwind)
    (define-key map (kbd "C-f") #'cljm-thread-first-all)
    (define-key map (kbd "f") #'cljm-thread-first-all)
    (define-key map (kbd "C-l") #'cljm-thread-last-all)
    (define-key map (kbd "l") #'cljm-thread-last-all)
    (define-key map (kbd "C-p") #'cljm-cycle-privacy)
    (define-key map (kbd "p") #'cljm-cycle-privacy)
    (define-key map (kbd "C-(") #'cljm-convert-collection-to-list)
    (define-key map (kbd "(") #'cljm-convert-collection-to-list)
    (define-key map (kbd "C-'") #'cljm-convert-collection-to-quoted-list)
    (define-key map (kbd "'") #'cljm-convert-collection-to-quoted-list)
    (define-key map (kbd "C-{") #'cljm-convert-collection-to-map)
    (define-key map (kbd "{") #'cljm-convert-collection-to-map)
    (define-key map (kbd "C-[") #'cljm-convert-collection-to-vector)
    (define-key map (kbd "[") #'cljm-convert-collection-to-vector)
    (define-key map (kbd "C-#") #'cljm-convert-collection-to-set)
    (define-key map (kbd "#") #'cljm-convert-collection-to-set)
    (define-key map (kbd "C-i") #'cljm-cycle-if)
    (define-key map (kbd "i") #'cljm-cycle-if)
    (define-key map (kbd "C-w") #'cljm-cycle-when)
    (define-key map (kbd "w") #'cljm-cycle-when)
    (define-key map (kbd "C-o") #'cljm-cycle-not)
    (define-key map (kbd "o") #'cljm-cycle-not)
    (define-key map (kbd "n i") #'cljm-insert-ns-form)
    (define-key map (kbd "n h") #'cljm-insert-ns-form-at-point)
    (define-key map (kbd "n u") #'cljm-update-ns)
    (define-key map (kbd "n s") #'cljm-sort-ns)
    (define-key map (kbd "n r") #'cljm-rename-ns-alias)
    (define-key map (kbd "s i") #'cljm-introduce-let)
    (define-key map (kbd "s m") #'cljm-move-to-let)
    (define-key map (kbd "s f") #'cljm-let-forward-slurp-sexp)
    (define-key map (kbd "s b") #'cljm-let-backward-slurp-sexp)
    (define-key map (kbd "C-a") #'cljm-add-arity)
    (define-key map (kbd "a") #'cljm-add-arity)
    map)
  "Keymap for Clojure refactoring commands.")

(fset 'cljm-refactor-map cljm-refactor-map)

(defvar-local cljm-expected-ns-function nil
  "The function used to determine the expected namespace of a file.
`cljm-mode' ships a basic function named `cljm-expected-ns'
that does basic heuristics to figure this out.
CIDER provides a more complex version which does classpath analysis.")

;;; ns manipulation
(defun cljm-expected-ns (&optional path)
  "Return the namespace matching PATH.

PATH is expected to be an absolute file path.

If PATH is nil, use the path to the file backing the current buffer."
  (let* ((path (or path (file-truename (buffer-file-name))))
         (relative (cljm-project-relative-path path))
         (sans-file-type (substring relative 0
                                    (- (length (file-name-extension path t)))))
         (sans-file-sep (mapconcat 'identity
                                   (cdr (split-string sans-file-type "/")) "."))
         (sans-underscores (replace-regexp-in-string "_" "-" sans-file-sep)))
    ;; Drop prefix from ns for projects with structure src/{clj,cljs,cljc}
    (replace-regexp-in-string "\\`clj[scx]?\\." "" sans-underscores)))

(defun cljm-insert-ns-form-at-point ()
  "Insert a namespace form at point."
  (interactive)
  (insert (format "(ns %s)" (funcall cljm-expected-ns-function))))

(defun cljm-insert-ns-form ()
  "Insert a namespace form at the beginning of the buffer."
  (interactive)
  (widen)
  (goto-char (point-min))
  (cljm-insert-ns-form-at-point))

(defconst cljm-namespace-name-regex
  (rx line-start
      "("
      (zero-or-one (group (regexp "clojure.core/")))
      (zero-or-one (submatch "in-"))
      "ns"
      (zero-or-one "+")
      (one-or-more (any whitespace "\n"))
      (zero-or-more (or (submatch (zero-or-one "#")
                                  "^{"
                                  (zero-or-more (not (any "}")))
                                  "}")
                        (zero-or-more "^:"
                                      (one-or-more (not (any whitespace)))))
                    (one-or-more (any whitespace "\n")))
      (zero-or-one (any ":'")) ;; (in-ns 'foo) or (ns+ :user)
      (group (one-or-more (not (any "()\"" whitespace))) symbol-end)))

(defun cljm--in-string-p ()
  "Check whether the point is currently in a string."
  (nth 3 (syntax-ppss)))

(defun cljm--in-comment-p ()
  "Check whether the point is currently in a comment."
  (nth 4 (syntax-ppss)))

(defun cljm--find-ns-in-direction (direction)
  "Return the nearest namespace in a specific DIRECTION.
DIRECTION is `forward' or `backward'."
  (let ((candidate)
        (fn (if (eq direction 'forward)
                #'search-forward-regexp
              #'search-backward-regexp)))
    (while (and (not candidate)
                (funcall fn cljm-namespace-name-regex nil t))
      (unless (or (cljm--in-string-p) (cljm--in-comment-p))
        (setq candidate (match-string-no-properties 4))))
    candidate))

(defun cljm-find-ns ()
  "Return the namespace of the current Clojure buffer.
Return the namespace closest to point and above it.  If there are
no namespaces above point, return the first one in the buffer.

The results will be cached if `cljm-cache-ns' is set to t."
  (if (and cljm-cache-ns cljm-cached-ns)
      cljm-cached-ns
    (let ((ns (save-excursion
                (save-restriction
                  (widen)

                  ;; Move to top-level to avoid searching from inside ns
                  (ignore-errors (while t (up-list nil t t)))

                  (or (cljm--find-ns-in-direction 'backward)
                      (cljm--find-ns-in-direction 'forward))))))
      (setq cljm-cached-ns ns)
      ns)))

(defun cljm-update-ns ()
  "Update the namespace of the current buffer.
Useful if a file has been renamed."
  (interactive)
  (let ((nsname (funcall cljm-expected-ns-function)))
    (when nsname
      (save-excursion
        (save-match-data
          (if (cljm-find-ns)
              (progn
                (replace-match nsname nil nil nil 4)
                (message "ns form updated to `%s'" nsname)
                (setq cljm-cached-ns nsname))
            (user-error "Can't find ns form")))))))

(defun cljm--sort-following-sexps ()
  "Sort sexps between point and end of current sexp.
Comments at the start of a line are considered part of the
following sexp.  Comments at the end of a line (after some other
content) are considered part of the preceding sexp."
  ;; Here we're after the :require/:import symbol.
  (save-restriction
    (narrow-to-region (point) (save-excursion
                                (up-list)
                                (1- (point))))
    (skip-chars-forward "\r\n[:blank:]")
    (sort-subr nil
               (lambda () (skip-chars-forward "\r\n[:blank:]"))
               ;; Move to end of current top-level thing.
               (lambda ()
                 (condition-case nil
                     (while t (up-list))
                   (scan-error nil))
                 ;; We could be inside a symbol instead of a sexp.
                 (unless (looking-at "\\s-\\|$")
                   (cljm-forward-logical-sexp))
                 ;; move past comments at the end of the line.
                 (search-forward-regexp "$"))
               ;; Move to start of ns name.
               (lambda ()
                 (comment-forward)
                 (skip-chars-forward "[:blank:]\n\r[(")
                 (cljm-forward-logical-sexp)
                 (forward-sexp -1)
                 nil)
               ;; Move to end of ns name.
               (lambda ()
                 (cljm-forward-logical-sexp)))
    (goto-char (point-max))
    ;; Does the last line now end in a comment?
    (when (nth 4 (parse-partial-sexp (point-min) (point)))
      (insert "\n"))))

(defun cljm-sort-ns ()
  "Internally sort each sexp inside the ns form."
  (interactive)
  (comment-normalize-vars)
  (if (cljm-find-ns)
      (save-excursion
        (goto-char (match-beginning 0))
        (redisplay)
        (let ((beg (point))
              (ns))
          (forward-sexp 1)
          (setq ns (buffer-substring beg (point)))
          (forward-char -1)
          (while (progn (forward-sexp -1)
                        (looking-at "(:[a-z]"))
            (save-excursion
              (forward-char 1)
              (forward-sexp 1)
              (cljm--sort-following-sexps)))
          (goto-char beg)
          (if (looking-at (regexp-quote ns))
              (message "ns form is already sorted")
            (sleep-for 0.1)
            (redisplay)
            (message "ns form has been sorted")
            (sleep-for 0.1))))
    (user-error "Can't find ns form")))

;;; Threading macros related
(defcustom cljm-thread-all-but-last nil
  "Non-nil means do not thread the last expression.
This means that `cljm-thread-first-all' and
`cljm-thread-last-all' not thread the deepest sexp inside the
current sexp."
  :safe #'booleanp
  :type 'boolean)

(defun cljm--maybe-unjoin-line ()
  "Undo a `join-line' done by a threading command."
  (when (get-text-property (point) 'cljm-thread-line-joined)
    (remove-text-properties (point) (1+ (point))
                            '(cljm-thread-line-joined t))
    (insert "\n")))

(defun cljm-delete-and-extract-sexp ()
  "Delete the surrounding sexp and return it."
  (let ((begin (point)))
    (forward-sexp)
    (let ((result (buffer-substring begin (point))))
      (delete-region begin (point))
      result)))

(defun cljm--ensure-parens-around-function-names ()
  "Insert parens around function names if necessary."
  (cljm--looking-at-non-logical-sexp)
  (unless (looking-at "(")
    (insert-parentheses 1)
    (backward-up-list)))

(defun cljm--unwind-last ()
  "Unwind a thread last macro once.

Point must be between the opening paren and the ->> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (cljm-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (cljm--ensure-parens-around-function-names)
      (let* ((sexp-beg-line (line-number-at-pos))
             (sexp-end-line (progn (forward-sexp)
                                   (line-number-at-pos)))
             (multiline-sexp-p (not (= sexp-beg-line sexp-end-line))))
        (down-list -1)
        (if multiline-sexp-p
            (insert "\n")
          ;; `cljm--maybe-unjoin-line' only works when unwinding sexps that were
          ;; threaded in the same Emacs session, but it also catches cases that
          ;; `multiline-sexp-p' doesn't.
          (cljm--maybe-unjoin-line))
        (insert contents))))
  (forward-char))

(defun cljm--unwind-first ()
  "Unwind a thread first macro once.

Point must be between the opening paren and the -> symbol."
  (forward-sexp)
  (save-excursion
    (let ((contents (cljm-delete-and-extract-sexp)))
      (when (looking-at " *\n")
        (join-line 'following))
      (cljm--ensure-parens-around-function-names)
      (down-list)
      (forward-sexp)
      (insert contents)
      (forward-sexp -1)
      (cljm--maybe-unjoin-line)))
  (forward-char))

(defun cljm--pop-out-of-threading ()
  "Raise a sexp up a level to unwind a threading form."
  (save-excursion
    (down-list 2)
    (backward-up-list)
    (raise-sexp)))

(defun cljm--nothing-more-to-unwind ()
  "Return non-nil if a threaded form cannot be unwound further."
  (save-excursion
    (let ((beg (point)))
      (forward-sexp)
      (down-list -1)
      (backward-sexp 2) ;; the last sexp, the threading macro
      (when (looking-back "(\\s-*" (line-beginning-position))
        (backward-up-list)) ;; and the paren
      (= beg (point)))))

(defun cljm--fix-sexp-whitespace (&optional move-out)
  "Fix whitespace after unwinding a threading form.

Optional argument MOVE-OUT, if non-nil, means moves up a list
before fixing whitespace."
  (save-excursion
    (when move-out (backward-up-list))
    (let ((sexp (bounds-of-thing-at-point 'sexp)))
      (cljm-indent-region (car sexp) (cdr sexp))
      (delete-trailing-whitespace (car sexp) (cdr sexp)))))

;;;###autoload
(defun cljm-unwind (&optional n)
  "Unwind thread at point or above point by N levels.
With universal argument \\[universal-argument], fully unwind thread."
  (interactive "P")
  (setq n (cond ((equal n '(4)) 999)
                (n) (1)))
  (save-excursion
    (let ((limit (save-excursion
                   (beginning-of-defun)
                   (point))))
      (ignore-errors
        (when (looking-at "(")
          (forward-char 1)
          (forward-sexp 1)))
      (while (> n 0)
        (search-backward-regexp "([^-]*->" limit)
        (if (cljm--nothing-more-to-unwind)
            (progn (cljm--pop-out-of-threading)
                   (cljm--fix-sexp-whitespace)
                   (setq n 0)) ;; break out of loop
          (down-list)
          (cond
           ((looking-at "[^-]*->\\_>")  (cljm--unwind-first))
           ((looking-at "[^-]*->>\\_>") (cljm--unwind-last)))
          (cljm--fix-sexp-whitespace 'move-out)
          (setq n (1- n)))))))

;;;###autoload
(defun cljm-unwind-all ()
  "Fully unwind thread at point or above point."
  (interactive)
  (cljm-unwind '(4)))

(defun cljm--remove-superfluous-parens ()
  "Remove extra parens from a form."
  (when (looking-at "([^ )]+)")
    (delete-pair)))

(defun cljm--thread-first ()
  "Thread a nested sexp using ->."
  (down-list)
  (forward-symbol 1)
  (unless (looking-at ")")
    (let ((contents (cljm-delete-and-extract-sexp)))
      (backward-up-list)
      (just-one-space 0)
      (save-excursion
        (insert contents "\n")
        (cljm--remove-superfluous-parens))
      (when (looking-at "\\s-*\n")
        (join-line 'following)
        (forward-char 1)
        (put-text-property (point) (1+ (point))
                           'cljm-thread-line-joined t))
      t)))

(defun cljm--thread-last ()
  "Thread a nested sexp using ->>."
  (forward-sexp 2)
  (down-list -1)
  (backward-sexp)
  (unless (eq (char-before) ?\()
    (let ((contents (cljm-delete-and-extract-sexp)))
      (just-one-space 0)
      (backward-up-list)
      (insert contents "\n")
      (cljm--remove-superfluous-parens)
      ;; cljr #255 Fix dangling parens
      (forward-sexp)
      (when (looking-back "^\\s-*\\()+\\)\\s-*" (line-beginning-position))
        (let ((pos (match-beginning 1)))
          (put-text-property pos (1+ pos) 'cljm-thread-line-joined t))
        (join-line))
      t)))

(defun cljm--threadable-p ()
  "Return non-nil if a form can be threaded."
  (save-excursion
    (forward-symbol 1)
    (looking-at "[\n\r\t ]*(")))

;;;###autoload
(defun cljm-thread ()
  "Thread by one more level an existing threading macro."
  (interactive)
  (ignore-errors
    (when (looking-at "(")
      (forward-char 1)
      (forward-sexp 1)))
  (search-backward-regexp "([^-]*->")
  (down-list)
  (when (cljm--threadable-p)
    (prog1 (cond
            ((looking-at "[^-]*->\\_>")  (cljm--thread-first))
            ((looking-at "[^-]*->>\\_>") (cljm--thread-last)))
      (cljm--fix-sexp-whitespace 'move-out))))

(defun cljm--thread-all (first-or-last-thread but-last)
  "Fully thread the form at point.

FIRST-OR-LAST-THREAD is \"->\" or \"->>\".

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `cljm-thread-all-but-last'."
  (save-excursion
    (insert-parentheses 1)
    (insert first-or-last-thread))
  (while (save-excursion (cljm-thread)))
  (when (or but-last cljm-thread-all-but-last)
    (cljm-unwind)))

;;;###autoload
(defun cljm-thread-first-all (but-last)
  "Fully thread the form at point using ->.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `cljm-thread-all-but-last'."
  (interactive "P")
  (cljm--thread-all "-> " but-last))

;;;###autoload
(defun cljm-thread-last-all (but-last)
  "Fully thread the form at point using ->>.

When BUT-LAST is non-nil, the last expression is not threaded.
Default value is `cljm-thread-all-but-last'."
  (interactive "P")
  (cljm--thread-all "->> " but-last))

;;; Cycling stuff

(defcustom cljm-use-metadata-for-privacy nil
  "If nil, `cljm-cycle-privacy' will use (defn- f []).
If t, it will use (defn ^:private f [])."
  :safe #'booleanp
  :type 'boolean)

;;;###autoload
(defun cljm-cycle-privacy ()
  "Make public the current private def, or vice-versa.
See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-privacy"
  (interactive)
  (save-excursion
    (ignore-errors (forward-char 7))
    (search-backward-regexp "(defn?\\(-\\| ^:private\\)?\\_>")
    (if (match-string 1)
        (replace-match "" nil nil nil 1)
      (goto-char (match-end 0))
      (insert (if (or cljm-use-metadata-for-privacy
                      (equal (match-string 0) "(def"))
                  " ^:private"
                "-")))))

(defun cljm--convert-collection (coll-open coll-close)
  "Convert the collection at (point) by unwrapping it an wrapping it between COLL-OPEN and COLL-CLOSE."
  (save-excursion
    (while (and
            (not (bobp))
            (not (looking-at "(\\|{\\|\\[")))
      (backward-char))
    (when (or (eq ?\# (char-before))
              (eq ?\' (char-before)))
      (delete-char -1))
    (when (and (bobp)
               (not (memq (char-after) '(?\{ ?\( ?\[))))
      (user-error "Beginning of file reached, collection is not found"))
    (insert coll-open (substring (cljm-delete-and-extract-sexp) 1 -1)
            coll-close)))

;;;###autoload
(defun cljm-convert-collection-to-list ()
  "Convert collection at (point) to list."
  (interactive)
  (cljm--convert-collection "(" ")"))

;;;###autoload
(defun cljm-convert-collection-to-quoted-list ()
  "Convert collection at (point) to quoted list."
  (interactive)
  (cljm--convert-collection "'(" ")"))

;;;###autoload
(defun cljm-convert-collection-to-map ()
  "Convert collection at (point) to map."
  (interactive)
  (cljm--convert-collection "{" "}"))

;;;###autoload
(defun cljm-convert-collection-to-vector ()
  "Convert collection at (point) to vector."
  (interactive)
  (cljm--convert-collection "[" "]"))

;;;###autoload
(defun cljm-convert-collection-to-set ()
  "Convert collection at (point) to set."
  (interactive)
  (cljm--convert-collection "#{" "}"))

(defun cljm--goto-if ()
  "Find the first surrounding if or if-not expression."
  (when (cljm--in-string-p)
    (while (or (not (looking-at "("))
               (cljm--in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((if \\)\\|\\((if-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No if or if-not found")))))

;;;###autoload
(defun cljm-cycle-if ()
  "Change a surrounding if to if-not, or vice-versa.

See: https://github.com/clojure-emacs/clj-refactor.el/wiki/cljr-cycle-if"
  (interactive)
  (save-excursion
    (cljm--goto-if)
    (cond
     ((looking-at "(if-not")
      (forward-char 3)
      (delete-char 4)
      (forward-sexp 2)
      (transpose-sexps 1))
     ((looking-at "(if")
      (forward-char 3)
      (insert "-not")
      (forward-sexp 2)
      (transpose-sexps 1)))))

;; TODO: Remove code duplication with `cljm--goto-if'.
(defun cljm--goto-when ()
  "Find the first surrounding when or when-not expression."
  (when (cljm--in-string-p)
    (while (or (not (looking-at "("))
               (cljm--in-string-p))
      (backward-char)))
  (while (not (looking-at "\\((when \\)\\|\\((when-not \\)"))
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "No when or when-not found")))))

;;;###autoload
(defun cljm-cycle-when ()
  "Change a surrounding when to when-not, or vice-versa."
  (interactive)
  (save-excursion
    (cljm--goto-when)
    (cond
     ((looking-at "(when-not")
      (forward-char 9)
      (delete-char -4))
     ((looking-at "(when")
      (forward-char 5)
      (insert "-not")))))

(defun cljm-cycle-not ()
  "Add or remove a not form around the current form."
  (interactive)
  (save-excursion
    (condition-case nil
        (backward-up-list)
      (scan-error (user-error "`cljm-cycle-not' must be invoked inside a list")))
    (if (looking-back "(not " nil)
        (progn
          (delete-char -5)
          (forward-sexp)
          (delete-char 1))
      (insert "(not ")
      (forward-sexp)
      (insert ")"))))

(defun cljm--inside-let-binding-p ()
  "Return non-nil if point is inside a let binding."
  (ignore-errors
    (save-excursion
      (let ((pos (point)))
        (cljm--goto-let)
        (re-search-forward "\\[")
        (if (< pos (point))
            nil
          (forward-sexp)
          (up-list)
          (< pos (point)))))))

(defun cljm--beginning-of-current-let-binding ()
  "Move before the bound name of the current binding.
Assume that point is in the binding form of a let."
  (let ((current-point (point)))
    (cljm--goto-let)
    (search-forward "[")
    (forward-char)
    (while (> current-point (point))
      (forward-sexp))
    (backward-sexp 2)))

(defun cljm--previous-line ()
  "Keep the column position while go the previous line."
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col)))

(defun cljm--prepare-to-insert-new-let-binding ()
  "Move to right place in the let form to insert a new binding and indent."
  (if (cljm--inside-let-binding-p)
      (progn
        (cljm--beginning-of-current-let-binding)
        (newline-and-indent)
        (cljm--previous-line)
        (indent-for-tab-command))
    (cljm--goto-let)
    (search-forward "[")
    (backward-up-list)
    (forward-sexp)
    (down-list -1)
    (backward-char)
    (if (looking-at "\\[\\s-*\\]")
        (forward-char)
      (forward-char)
      (newline-and-indent))))

(defun cljm--introduce-let-internal (name &optional n)
  "Create a let form, binding the form at point with NAME.

Optional numeric argument N, if non-nil, introduces the let N
lists up."
  (if (numberp n)
      (let ((init-expr-sexp (cljm-delete-and-extract-sexp)))
        (insert name)
        (ignore-errors (backward-up-list n))
        (insert "(let" (cljm-delete-and-extract-sexp) ")")
        (backward-sexp)
        (down-list)
        (forward-sexp)
        (insert " [" name " " init-expr-sexp "]\n")
        (cljm--replace-sexps-with-bindings-and-indent))
    (insert "[ " (cljm-delete-and-extract-sexp) "]")
    (backward-sexp)
    (insert "(let " (cljm-delete-and-extract-sexp) ")")
    (backward-sexp)
    (down-list 2)
    (insert name)
    (forward-sexp)
    (up-list)
    (newline-and-indent)
    (insert name)))

(defun cljm--move-to-let-internal (name)
  "Bind the form at point to NAME in the nearest let."
  (if (not (save-excursion (cljm--goto-let)))
      (cljm--introduce-let-internal name)
    (let ((contents (cljm-delete-and-extract-sexp)))
      (insert name)
      (cljm--prepare-to-insert-new-let-binding)
      (insert contents)
      (backward-sexp)
      (insert " ")
      (backward-char)
      (insert name)
      (cljm--replace-sexps-with-bindings-and-indent))))

(defun cljm--let-backward-slurp-sexp-internal ()
  "Slurp the s-expression before the let form into the let form."
  (cljm--goto-let)
  (backward-sexp)
  (let ((sexp (string-trim (cljm-delete-and-extract-sexp))))
    (delete-blank-lines)
    (down-list)
    (forward-sexp 2)
    (newline-and-indent)
    (insert sexp)
    (cljm--replace-sexps-with-bindings-and-indent)))

(defun cljm--rename-ns-alias-internal (current-alias new-alias)
  "Rename a namespace alias CURRENT-ALIAS to NEW-ALIAS."
  (cljm--find-ns-in-direction 'backward)
  (let ((rgx (concat ":as +" current-alias))
        (bound (save-excursion (forward-list 1) (point))))
    (when (search-forward-regexp rgx bound t)
      (replace-match (concat ":as " new-alias))
      (save-excursion
        (while (re-search-forward (concat current-alias "/") nil t)
          (when (not (nth 3 (syntax-ppss)))
            (replace-match (concat new-alias "/")))))
      (save-excursion
        (while (re-search-forward (concat "#::" current-alias "{") nil t)
          (replace-match (concat "#::" new-alias "{"))))
      (message "Successfully renamed alias '%s' to '%s'"
               current-alias new-alias))))

;;;###autoload
(defun cljm-let-backward-slurp-sexp (&optional n)
  "Slurp the s-expression before the let form into the let form.
With a numeric prefix argument slurp the previous N s-expressions
into the let form."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ n)
      (save-excursion (cljm--let-backward-slurp-sexp-internal)))))

(defun cljm--let-forward-slurp-sexp-internal ()
  "Slurp the next s-expression after the let form into the let form."
  (cljm--goto-let)
  (forward-sexp)
  (let ((sexp (string-trim (cljm-delete-and-extract-sexp))))
    (down-list -1)
    (newline-and-indent)
    (insert sexp)
    (cljm--replace-sexps-with-bindings-and-indent)))

;;;###autoload
(defun cljm-let-forward-slurp-sexp (&optional n)
  "Slurp the next s-expression after the let form into the let form.
With a numeric prefix argument slurp the next N s-expressions
into the let form."
  (interactive "p")
  (unless n (setq n 1))
  (dotimes (_ n)
    (save-excursion (cljm--let-forward-slurp-sexp-internal))))

;;;###autoload
(defun cljm-introduce-let (&optional n)
  "Create a let form, binding the form at point.
With a numeric prefix argument the let is introduced N lists up."
  (interactive "P")
  (cljm--introduce-let-internal (read-from-minibuffer
                                 "Name of bound symbol: ") n))

;;;###autoload
(defun cljm-move-to-let ()
  "Move the form at point to a binding in the nearest let."
  (interactive)
  (cljm--move-to-let-internal (read-from-minibuffer "Name of bound symbol: ")))

;;;###autoload
(defun cljm-rename-ns-alias ()
  "Rename a namespace alias."
  (interactive)
  (let ((current-alias (read-from-minibuffer "Current alias: ")))
    (save-excursion
      (cljm--find-ns-in-direction 'backward)
      (let ((rgx (concat ":as +" current-alias))
            (bound (save-excursion (forward-list 1) (point))))
        (if (save-excursion (search-forward-regexp rgx bound t))
            (let ((new-alias (read-from-minibuffer "New alias: ")))
              (cljm--rename-ns-alias-internal current-alias new-alias))
          (message "Cannot find namespace alias: '%s'" current-alias))))))

(defun cljm--add-arity-defprotocol-internal ()
  "Add an arity to a signature inside a defprotocol.

Assumes cursor is at beginning of signature."
  (re-search-forward "\\[")
  (save-excursion (insert "] [")))

(defun cljm--add-arity-reify-internal ()
  "Add an arity to a function inside a reify.

Assumes cursor is at beginning of function."
  (re-search-forward "\\(\\w+ \\)")
  (insert "[")
  (save-excursion (insert "])\n(" (match-string 0))))

(defun cljm--add-arity-internal ()
  "Add an arity to a function.

Assumes cursor is at beginning of function."
  (let ((beg-line (what-line))
        (end (save-excursion (forward-sexp)
                             (point))))
    (down-list 2)
    (when (looking-back "{" 1) ;; skip metadata if present
      (up-list)
      (down-list))
    (cond
     ((looking-back "(" 1) ;; multi-arity fn
      (insert "[")
      (save-excursion (insert "])\n(")))
     ((looking-back "\\[" 1)  ;; single-arity fn
      (let* ((same-line (string= beg-line (what-line)))
             (new-arity-text (concat (when same-line "\n") "([")))
        (save-excursion
          (goto-char end)
          (insert ")"))

        (re-search-backward " +\\[")
        (replace-match new-arity-text)
        (save-excursion (insert "])\n([")))))))

;;;###autoload
(defun cljm-add-arity ()
  "Add an arity to a function."
  (interactive)
  (let ((original-pos (point))
        (n 0))
    (while (not (looking-at-p "(\\(defn\\|letfn\\|fn\\|defmacro\\|defmethod\\|defprotocol\\|reify\\|proxy\\)"))
      (setq n (1+ n))
      (backward-up-list 1 t))
    (let ((beg (point))
          (end-marker (make-marker))
          (end (save-excursion (forward-sexp)
                               (point)))
          (jump-up (lambda (x)
                     (goto-char original-pos)
                     (backward-up-list x t))))
      (set-marker end-marker end)
      (cond
       ((looking-at-p "(\\(defn\\|fn\\|defmethod\\|defmacro\\)")
        (cljm--add-arity-internal))
       ((looking-at-p "(letfn")
        (funcall jump-up (- n 2))
        (cljm--add-arity-internal))
       ((looking-at-p "(proxy")
        (funcall jump-up (- n 1))
        (cljm--add-arity-internal))
       ((looking-at-p "(defprotocol")
        (funcall jump-up (- n 1))
        (cljm--add-arity-defprotocol-internal))
       ((looking-at-p "(reify")
        (funcall jump-up (- n 1))
        (cljm--add-arity-reify-internal)))
      (indent-region beg end-marker))))

(provide 'cljm-refactor)

;;; cljm-refactor.el ends here
