;;; firefox-history-search.el --- Search through firefox history -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: August 25, 2021
;; Modified: August 25, 2021
;; Version: 0.0.1
;; Keywords: broswer, matching
;; Homepage: https://github.com/savnkk/firefox-history-search
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Search through firefox history
;;

(require 'firefox-history)
(require 'sexp-string)

;;; vars:
(defvar firefox-history-search-predicates
  '((or  :name or
         :transform
         ((`(or . ,clauses) (--> clauses
                                 (cl-reduce (lambda (x y) (concat x " or " y)) (mapcar #'rec it))
                                 (concat "(" it ")")))))
    (and :name and
         :transform
         ((`(and . ,clauses) (--> clauses
                                  (cl-reduce (lambda (x y) (concat x " and " y)) (mapcar #'rec it))
                                  (concat "(" it ")")))))
    (url :name url :docstring "Return non-nil if current heading has one or more of TAGS (a list of strings).\nTests both inherited and local tags." :args (&rest titles)
         :transform
         ((`(url . ,rest)
           (--> rest
                (mapcar (apply-partially #'format "url like '%%%%%s%%%%'") it)
                (cl-reduce (lambda (x y) (concat x " or " y)) it)
                (concat "(" it ")")))))
    (regexp :name regexp  :docstring "Return non-nil if current heading has one or more of TAGS (a list of strings).\nTests both inherited and local tags." :args (&rest regexp)
            :transform
            ((`(regexp . ,rest)
              (--> rest
                   (mapcar (lambda (x) (concat (format "title like '%%%% %s%%%%'" x)
                                               " or "
                                               (format "description like '%%%% %s%%%%'" x))) it)
                   (cl-reduce (lambda (x y) (concat x " or " y)) it)
                   (concat "(" it ")")))))))

(defvar firefox-history-search-default-predicate-boolean 'and)
(defvar firefox-history-search-default-predicate 'regexp)
;;; Code:

(declare-function firefox-history-search--query-string-to-sexp "ext:firefox-history-search" (query) t)
(declare-function firefox-history-search--transform-query "ext:firefox-history-search" (query) t)
(fset 'firefox-history-search--query-string-to-sexp (sexp-string--define-query-string-to-sexp-fn "firefox-history-search"))
(fset 'firefox-history-search--transform-query (sexp-string--define-transform-query-fn "firefox-history-search" :transform))

(defun firefox-history-search-match (str)
  "Create where... type query for firefox history from STR.
Expect STR to be clause that fits between `where' and `order' SQL constructs."
  (concat "where "
          str
          " order by visit_date desc limit 20"))

(defun firefox-history-search-interactive-query (query)
  "Convert QUERY to firefox-history compatable query.
Two supported syntax.
`(url: ) or (regex:query2,query3 query4)
or
`(and (url query1 query2) (regex query3))
look at `firefox-history-search--query-string-to-sexp' and `firefox-history-search--transform-query' for more info."
  (--> query
       (if (listp it)
           it
         (firefox-history-search--query-string-to-sexp it))
       (firefox-history-search--transform-query it)
       (firefox-history-search-match it)))

(defun firefox-history-search (str)
  "Search STR in firefox history."
  (interactive (list (->> (completing-read "query:#" 'nil)
                          (firefox-history-search-interactive-query))))
  (--> (mapcar #'firefox-history-item
               (firefox-history "--query" str))
       (mapcar (lambda (x) (cons (firefox-history-lister-view x) x)) it)
       (consult--read it
       :lookup #'consult--lookup-cdr
       ;; :add-history
       ;; (when-let (thing (thing-at-point 'symbol))
       ;; (consult--async-split-initial thing))
       :require-match t
       :category 'firefox-history-search
       ;; :category 'consult-grep
       ;; :group #'consult--grep-group
       ;; :history '(:input consult--grep-history)
       :sort nil)
       (firefox-history-new-buffer (list it) "Search results")))

(defun firefox-history-search-consult--command-args (cmd)
  "Split command arguments and append to CMD."
  (setq cmd (split-string-and-unquote cmd))
  (lambda (input)
    (save-match-data
      (setq  input (condition-case nil (firefox-history-search-interactive-query input) (error "")))
      (unless (string-blank-p input)
        (mapcan (lambda (x)
                  (list (replace-regexp-in-string "ARG" input x 'fixedcase 'literal)))
                cmd)))))

(defmacro firefox-history-search-consult--async-command (cmd &rest args)
  "Asynchronous CMD pipeline.

ARGS is a list of `make-process' properties and transforms."
  (declare (indent 1))
  `(thread-first (consult--async-sink)
     (consult--async-refresh-timer)
     ,@(seq-take-while (lambda (x) (not (keywordp x))) args)
     (consult--async-process
      (firefox-history-search-consult--command-args ,cmd)
      ,@(seq-drop-while (lambda (x) (not (keywordp x))) args))
     (consult--async-throttle)
     (consult--async-split)))

(defmacro firefox-history-search-consult--async-transform (async &rest transform)
  "Use FUN to TRANSFORM candidates of ASYNC."
  (let ((async-var (make-symbol "async"))
        (action-var (make-symbol "action")))
    `(let ((,async-var ,async))
       (lambda (,action-var)
         (funcall ,async-var (if (consp ,action-var) (,@transform ,action-var) ,action-var))))))

(cl-defun firefox-history-search-consult (&optional (prompt "Firefox-history") (initial ""))
  "Search through firefox-history.
PROMPT is the prompt string.
INITIAL is initial input."
  (interactive)
  (let* (;(default-directory firefox-history-location)
         (read-process-output-max (max read-process-output-max (* 1024 1024))))
    (firefox-history-sync-temp-database)
    (--> (consult--read
          (firefox-history-search-consult--async-command (concat (firefox-history-main-cmd-string) " " "--elisp" " " "--stable" " " "--query" " " "ARG")
            (firefox-history-search-consult--async-transform (lambda (lines)
                                                               (--> (read (apply 'concat lines))
                                                                    (mapcar (lambda (entry) (let ((item (firefox-history-item entry)))
                                                                                              (cons (firefox-history-lister-view item) item))) it)))))
          :prompt prompt
          :initial (consult--async-split-initial initial)
          :lookup #'consult--lookup-cdr
          ;; :add-history
          ;; (when-let (thing (thing-at-point 'symbol))
          ;; (consult--async-split-initial thing))
          :require-match t
          :category 'firefox-history-search
          ;; :category 'consult-grep
          ;; :group #'consult--grep-group
          ;; :history '(:input consult--grep-history)
          :sort nil)
         (firefox-history-new-buffer (list it) "Results"))))

(provide 'firefox-history-search)
;;; firefox-history-search.el ends here
