;;; cljm-project.el --- project support -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cljm-cache)

(require 'cl-lib)
(require 'project)

;; project.el integration
(cl-defmethod project-roots ((project (head clojure)))
  (list (cdr project)))

(defun cljm-project-relative-path (path)
  "Denormalize PATH by making it relative to the project root."
  (file-relative-name path (cljm-project-dir)))

(defcustom cljm-build-tool-files
  '("project.clj"      ; Leiningen
    "build.boot"       ; Boot
    "build.gradle"     ; Gradle
    "build.gradle.kts" ; Gradle
    "deps.edn"         ; Clojure CLI (a.k.a. tools.deps)
    "shadow-cljs.edn"  ; shadow-cljs
    )
  "A list of files, which identify a Clojure project's root.
Out-of-the box `cljm-mode' understands lein, boot, gradle,
 shadow-cljs and tools.deps."
  :type '(repeat string)
  :safe (lambda (value)
          (and (listp value)
               (cl-every 'stringp value))))

(defcustom cljm-project-root-function #'cljm-project-root-path
  "Function to locate clojure project root directory."
  :type 'function
  :risky t)

(defun cljm-project-dir (&optional dir-name)
  "Return the absolute path to the project's root directory.

Call is delegated down to `cljm-project-root-function' with
optional DIR-NAME as argument.

When `cljm-cache-project-dir' is t the results of the command
are cached in a buffer local variable (`cljm-cached-project-dir')."
  (let ((project-dir (or cljm-cached-project-dir
                         (funcall cljm-project-root-function dir-name))))
    (when (and cljm-cache-project-dir
               (derived-mode-p 'cljm-mode)
               (not cljm-cached-project-dir))
      (setq cljm-cached-project-dir project-dir))
    project-dir))

(defun cljm-current-project (&optional dir-name)
  "Return the current project as a cons cell usable by project.el.

Call is delegated down to `cljm-project-dir' with
optional DIR-NAME as argument."
  (let ((project-dir (cljm-project-dir dir-name)))
    (if project-dir
        (cons 'clojure project-dir)
      nil)))

(defun cljm-project-root-path (&optional dir-name)
  "Return the absolute path to the project's root directory.

Use `default-directory' if DIR-NAME is nil.
Return nil if not inside a project."
  (let* ((dir-name (or dir-name default-directory))
         (choices (delq nil
                        (mapcar (lambda (fname)
                                  (locate-dominating-file dir-name fname))
                                cljm-build-tool-files))))
    (when (> (length choices) 0)
      (car (sort choices #'file-in-directory-p)))))

(provide 'cljm-project)

;;; cljm-project.el ends here
