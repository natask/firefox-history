;;; firefox-history.el --- Utility to delve into firefox database. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Natnael Kahssay
;;
;; Author: Natnael Kahssay <https://github.com/natask>
;; Maintainer: Natnael Kahssay <thisnkk@gmail.com>
;; Created: July 05, 2021
;; Modified: July 05, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/savnkk/firefox-history
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
(require 'helm)
(require 'dash)
(require 'org-protocol)

;;; vars:
(defcustom firefox-history-location  (concat (file-name-directory (locate-library "firefox-history")) "firefox-history")
  "Location of `firefox-history' script."
  :type '(string)
:group 'firefox-history)
(defcustom firefox-database-location   "~/.mozilla/firefox/ynulz4el.dev-edition-default/places.sqlite"
  "Location of the firefox database."
  :type '(string)
  :group 'firefox-history)
(defcustom firefox-database-temp-extension   "bak"
  "Extension for temporary firefox database."
  :type '(string)
  :group 'firefox-history)
(defcustom firefox-history-depth   10
  "Depth of history query."
  :type '(integer)
  :group 'firefox-history)

;;; Code:
(defun firefox-history (&rest args)
  "Call `firefox-history' with arguments ARGS."
  (let ((args (-reduce (lambda (x y) (concat x " " y)) args)))
    (read
    (shell-command-to-string (concat firefox-history-location " " args)))))


(defun firefox-history-check-if-visited (url &optional fn)
  "Check if URL has been visited in the past by querying firefox database.
Call FN if URL has been found."
  (when-let ((visit-dates (mapcar (lambda (x) (--> x
                                              (car x)
                                              (/ it 1000000)
                                              (seconds-to-time it)
                                              (format-time-string "[%Y-%m-%d %a %H:%M:%S]" it)
                                              (cons (concat it " " (cdr x)) x)))
                              (firefox-history url "--elisp" "--visit"))))
    (if fn (funcall fn))
    (helm :sources (helm-build-sync-source "select container"
                     :candidates (reverse visit-dates) :filtered-candidate-transformer))))

(defun firefox-history--org-protocol (info)
  "Process an org-protocol://firefox-history?ref= style url with INFO.

  It checks, opens, searchs or creates a note with the given ref.

When check is available in url, no matter what it is set to, just check if file exists, if not don't open anything or create org file.

    javascript:location.href = 'org-protocol://firefox-history?ref=' + \\
          encodeURIComponent(location.href)"
  (let* ((ref (car (split-string (plist-get info :ref) "[&#]")))
         (fn (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame)))))
    (firefox-history-check-if-visited ref fn)))

(push '("firefox-history"  :protocol "firefox-history"   :function firefox-history--org-protocol)
      org-protocol-protocol-alist)

(provide 'firefox-history)
;;; firefox-history.el ends here
