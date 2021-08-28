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
;;; Code:

(defun firefox-history-search--def-query-string-to-sexp-fn (predicates)
  "Define function `firefox-history-search--query-string-to-sexp' according to PREDICATES.
Builds the PEG expression using PREDICATES (which should be the
value of `firefox-history-search-predicates').
Borrowed from `org-ql'."
  (let* ((names (--map (symbol-name (plist-get (cdr it) :name))
                       predicates))
         (aliases (->> predicates
                       (--map (plist-get (cdr it) :aliases))
                       -non-nil
                       -flatten
                       (-map #'symbol-name)))
         (predicate-names (->> (append names aliases)
                               -uniq
                               ;; Sort the keywords longest-first to work around what seems to be an
                               ;; obscure bug in `peg': when one keyword is a substring of another,
                               ;; and the shorter one is listed first, the shorter one fails to match.
                               (-sort (-on #'> #'length))))
         (pexs
          `((query sum (opt eol))
            (sum value  (* (or (and _ "and" _ value `(a b -- (list 'and a b)))
                               (and _ "or" _ value `(a b -- (list 'or a b)))
                               (and _ value `(a b -- (list firefox-history-search-default-predicate-boolean a b))))))
            (value
             (or term
                 (and "(" (opt _) sum (opt _) ")" (opt _))))
            (term (or (and negation (list positive-term)
                           ;; This is a bit confusing, but it seems to work.  There's probably a better way.
                           `(pred -- (list 'not (car pred))))
                      positive-term))
            (positive-term (or (and predicate-with-args `(pred args -- (cons (intern pred) args)))
                               (and predicate-without-args `(pred -- (list (intern pred))))
                               (and plain-string `(s -- (list 'regexp s)))))
            (plain-string (or quoted-arg unquoted-arg))
            (predicate-with-args (substring predicate) ":" args)
            (predicate-without-args (substring predicate) ":")
            (predicate (or ,@predicate-names))
            (args (list (+ (and (or keyword-arg quoted-arg unquoted-arg) (opt separator)))))
            (keyword-arg (and keyword "=" `(kw -- (intern (concat ":" kw)))))
            (keyword (substring (+ (not (or brackets separator "=" "\"" (syntax-class whitespace))) (any))))
            (quoted-arg "\"" (substring (+ (not (or separator "\"")) (any))) "\"")
            (unquoted-arg (substring (+ (not (or brackets separator "\"" (syntax-class whitespace))) (any))))
            (negation "!")
            (separator "," )
            (operator (or "and" "or"))
            (brackets (or "(" ")"))
            (_ (* [" \t"]))
                                        ;(_ (+ (syntax-class whitespace)))
            (eol (or  "\n" "\r\n" "\r"))))
         (closure (lambda (input &optional boolean)
                    "Return query parsed from plain query string INPUT.
  Multiple predicate-names are combined with BOOLEAN (default: `and')."
                    ;; HACK: Silence unused lexical variable warnings.
                    (ignore predicates predicate-names names aliases)
                    (unless (s-blank-str? input)
                      (let* ((firefox-history-search-default-predicate-boolean (or boolean firefox-history-search-default-predicate-boolean))
                             (parsed-sexp
                              (with-temp-buffer
                                (insert input)
                                (goto-char (point-min))
                                ;; Copied from `peg-parse'.  There is no function in `peg' that
                                ;; returns a matcher function--every entry point is a macro,
                                ;; which means that, since we define our PEG rules at runtime when
                                ;; predicate-names are defined, we either have to use `eval', or we
                                ;; have to borrow some code.  It ends up that we only have to
                                ;; borrow this `with-peg-rules' call, which isn't too bad.
                                (eval `(with-peg-rules ,pexs
                                         (peg-run (peg ,(caar pexs)) #'peg-signal-failure))))))
                        (pcase parsed-sexp
                          (`(,_) (car parsed-sexp))
                          (_ nil)))))))
    (fset 'firefox-history-search--query-string-to-sexp closure)))

(defun firefox-history-search--define-transform-query-fn (predicates)
  "Define function `firefox-history-search--transform-query' for PREDICATES.
PREDICATES should be the value of `firefox-history-search-predicates'.
Borrowed from `org-ql'."
  (let ((transformer-patterns (->> predicates
                                   (--map (plist-get (cdr it) :transform))
                                   (-flatten-n 1))))
    (fset 'firefox-history-search--transform-query
          (byte-compile
           `(lambda (query)
              "Return transformed form of QUERY expression.
This function is defined by calling
`firefox-history-search--define-transform-query-fn', which uses transformr forms
defined in `firefox-history-search-predicates' by calling `firefox-history-search-defpred'."
              (cl-labels ((rec (element)
                               (pcase element
                                 ,@transformer-patterns
                                 (_ (error "element didn't match transformer:%S" element)))))
                (rec query)))))))

(firefox-history-search--define-transform-query-fn (reverse firefox-history-search-predicates))
(firefox-history-search--def-query-string-to-sexp-fn (reverse firefox-history-search-predicates))

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
               (firefox-history "--stable" "--query" str))
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
    (--> (consult--read
          (firefox-history-search-consult--async-command (concat firefox-history-location " " "--elisp" " " "--stable" " " "--query" " " "ARG")
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
