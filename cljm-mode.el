;;; cljm-mode.el --- Major mode for Clojure code -*- lexical-binding: t; -*-

;; Stripped down and broken up clojure-mode.el

;; Original copyright and author information for clojure-mode.el:
;;
;; Copyright © 2007-2019 Jeffrey Chu, Lennart Staflin, Phil Hagelberg
;; Copyright © 2013-2019 Bozhidar Batsov, Artur Malabarba
;;
;; Authors: Jeffrey Chu <jochu0@gmail.com>
;;       Lennart Staflin <lenst@lysator.liu.se>
;;       Phil Hagelberg <technomancy@gmail.com>
;;       Bozhidar Batsov <bozhidar@batsov.com>
;;       Artur Malabarba <bruce.connor.am@gmail.com>

;; Changes follow

;; URL: https://github.com/sogaiu/cljm-mode
;; Keywords: languages clojure clojurescript lisp
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides font-lock, indentation, navigation
;; Clojure programming language (http://clojure.org).

;; Here are some example configurations:

;;   ;; require or autoload paredit-mode
;;   (add-hook 'cljm-mode-hook #'paredit-mode)

;;   ;; require or autoload smartparens
;;   (add-hook 'cljm-mode-hook #'smartparens-strict-mode)

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; What appears to be left:

;; paragraph filling
;;
;; syntax highlighting via font-lock
;;
;; indentation / alignment
;;
;; paredit compatibility
;;
;; project root detection / project.el integration
;;
;; navigation to misc online docs
;;
;; syntax table
;;
;; outline
;;
;; sexp motion
;;
;; wrong major mode checking
;;
;; imenu
;;
;; misc functions supporting above code

;;; Code:

(require 'cljm-align)
(require 'cljm-docstring)
(require 'cljm-fill)
(require 'cljm-font-lock)
(require 'cljm-indent)
(require 'cljm-paredit)
(require 'cljm-project)
(require 'cljm-refs)
(require 'cljm-verify)

(require 'cl-lib)
(require 'imenu)
(require 'newcomment)
(require 'subr-x)
(require 'lisp-mnt)
(require 'project)

(declare-function lisp-fill-paragraph  "lisp-mode" (&optional justify))

(defgroup cljm nil
  "Major mode for editing Clojure code."
  :prefix "cljm-"
  :group 'languages
  :link '(url-link :tag "GitHub" "https://github.com/sogaiu/cljm-mode")
  :link '(emacs-commentary-link :tag "Commentary" "cljm-mode"))

(defconst cljm-mode-version
  (eval-when-compile
    (lm-version (or load-file-name buffer-file-name)))
  "The current version of `cljm-mode'.")

(defvar cljm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    map)
  "Keymap for Clojure mode.")

(defvar cljm-mode-syntax-table
  (let ((table (copy-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?? "_ p" table) ; ? is a prefix outside symbols
    (modify-syntax-entry ?# "_ p" table) ; # is allowed inside keywords (#399)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    table)
  "Syntax table for Clojure mode.
Inherits from `emacs-lisp-mode-syntax-table'.")

(defun cljm-mode-variables ()
  "Set up initial buffer-local variables for Clojure mode."
  (add-to-list 'imenu-generic-expression '(nil cljm-match-next-def 0))
  (setq-local indent-tabs-mode nil)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local outline-regexp ";;;\\(;* [^ \t\n]\\)\\|(")
  (setq-local outline-level 'lisp-outline-level)
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-use-syntax t)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local fill-paragraph-function #'cljm-fill-paragraph)
  (setq-local adaptive-fill-function #'cljm-adaptive-fill-function)
  (setq-local normal-auto-fill-function #'cljm-auto-fill-function)
  (setq-local comment-start-skip
              "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (setq-local indent-line-function #'cljm-indent-line)
  (setq-local indent-region-function #'cljm-indent-region)
  (setq-local lisp-indent-function #'cljm-indent-function)
  (setq-local lisp-doc-string-elt-property 'cljm-doc-string-elt)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local open-paren-in-column-0-is-defun-start nil))

;;;###autoload
(define-derived-mode cljm-mode prog-mode "Clojure"
  "Major mode for editing Clojure code.

\\{cljm-mode-map}"
  (cljm-mode-variables)
  (cljm-font-lock-setup)
  (add-hook 'paredit-mode-hook #'cljm-paredit-setup)
  ;; `electric-layout-post-self-insert-function' prevents indentation in strings
  ;; and comments, force indentation of non-inlined docstrings:
  (add-hook 'electric-indent-functions
            (lambda (_char)
              (if (and (cljm-in-docstring-p)
                       ;; make sure we're not dealing with an inline docstring
                       ;; e.g. (def foo "inline docstring" bar)
                       (save-excursion
                         (beginning-of-line-text)
                         (eq (get-text-property (point) 'face)
                             'font-lock-doc-face)))
                  'do-indent)))
  ;; integration with project.el
  (add-hook 'project-find-functions #'cljm-current-project))

;;;###autoload
(define-derived-mode cljms-mode cljm-mode "ClojureScript"
  "Major mode for editing ClojureScript code.

\\{clojurescript-mode-map}"
  (font-lock-add-keywords nil cljms-font-lock-keywords))

;;;###autoload
(define-derived-mode cljmc-mode cljm-mode "Clojure with Reader Conditionals"
  "Major mode for editing Clojure code with reader conditionals.

\\{cljmc-mode-map}")

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist
               '("\\.\\(clj\\|dtm\\|edn\\)\\'" . cljm-mode))
  (add-to-list 'auto-mode-alist '("\\.cljc\\'" . cljmc-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . cljms-mode))
  ;; boot build scripts are Clojure source files
  (add-to-list 'auto-mode-alist '("\\(?:build\\|profile\\)\\.boot\\'" .
                                  cljm-mode)))

(provide 'cljm-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cljm-mode.el ends here
