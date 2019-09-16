;;; cljm-cache.el --- cache -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defcustom cljm-cache-ns nil
  "Whether to cache the results of `cljm-find-ns'.

Note that this won't work well in buffers with multiple namespace
declarations (which rarely occur in practice) and you'll
have to invalidate this manually after changing the ns for
a buffer.  If you update the ns using `cljm-update-ns'
the cached value will be updated automatically."
  :type 'boolean
  :safe #'booleanp)

(defvar-local cljm-cached-ns nil
  "A buffer ns cache used to speed up ns-related operations.")

(defcustom cljm-cache-project-dir t
  "Whether to cache the results of `cljm-project-dir'."
  :type 'boolean
  :safe #'booleanp)

(defvar-local cljm-cached-project-dir nil
  "A project dir cache used to speed up related operations.")

(defun cljm-show-cache ()
  "Display cached values if present.
Useful for debugging."
  (interactive)
  (message "Cached Project: %s, Cached Namespace: %s"
           cljm-cached-project-dir cljm-cached-ns))

(defun cljm-clear-cache ()
  "Clear all buffer-local cached values.

Normally you'd need to do this very infrequently - e.g.
after renaming the root folder of project or after
renaming a namespace."
  (interactive)
  (setq cljm-cached-project-dir nil
        cljm-cached-ns nil)
  (message "Buffer-local cljm-mode cache cleared"))

(provide 'cljm-cache)

;;; cljm-cache.el ends here
