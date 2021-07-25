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
(require 'org-roam-url)

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
(defcustom firefox-history-url-max-depth   3
  "The max depth of url examined by `org-roam-url'."
  :type '(integer)
  :group 'firefox-history)
(defcustom firefox-history-stop-on-first-result 't
  "Stops after there is a result or depth which ever one comes first."
  :type '(boolean)
  :group 'firefox-history)

;;; Code:
(defun firefox-history (&rest args)
  "Call `firefox-history' with arguments ARGS."
  (let ((args (-reduce-from (lambda (x y) (concat x " " "\"" y "\"")) "" args)))
    (read
     (shell-command-to-string (concat firefox-history-location " " args)))))


(defun firefox-history-check-if-visited (url)
  "Check if URL has been visited in the past by querying firefox database."
  (mapcar (lambda (x) (--> x
                           (car x)
                           (/ it 1000000)
                           (seconds-to-time it)
                           (format-time-string "[%Y-%m-%d %a %H:%M:%S]" it)
                           (cons (concat it " " (cadr x)) x)))
          (firefox-history "--elisp" "--visit" url)))


(defun firefox-history-check-if-visited-progressively (url &optional fn)
  "Check if URL has been visited in the past by querying firefox database progressively.
   Call FN if URL has been found."
  (let ((urls (let ((org-roam-url-max-depth firefox-history-url-max-depth)) (org-roam-url--progressive-urls url)))
        (visited-dates '()))
    (dolist (progressive-url urls)
      (if (not (and visited-dates firefox-history-stop-on-first-result))
          (setq visited-dates (append visited-dates (firefox-history-check-if-visited progressive-url)))))
    (when visited-dates
        (if fn (funcall fn))
      (helm :sources (helm-build-sync-source "firefox history"
                       :candidates (reverse visited-dates) :filtered-candidate-transformer)))))

(defun firefox-history--org-protocol (info)
  "Process an org-protocol://firefox-history?ref= style url with INFO.

  It checks, opens, searchs or creates a note with the given ref.

When check is available in url, no matter what it is set to, just check if file exists, if not don't open anything or create org file.

    javascript:location.href = 'org-protocol://firefox-history?ref=' + \\
          encodeURIComponent(location.href)"
  (let* ((ref (plist-get info :ref))
         (fn (lambda () (x-focus-frame nil) (raise-frame) (select-frame-set-input-focus (selected-frame)))))
    (firefox-history-check-if-visited-progressively ref fn)))

(push '("firefox-history"  :protocol "firefox-history"   :function firefox-history--org-protocol)
      org-protocol-protocol-alist)

(provide 'firefox-history)
;;; firefox-history.el ends here
