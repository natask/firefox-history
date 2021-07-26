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
(require 'lister)
(require 'cl)

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

;;; mode:
(defvar firefox-history-mode-map
  (let ((map (make-sparse-keymap)))
    ;; inherit standard key bindings:
    ;; FIXME Remove reference to deprecated lister-keys-mode-map
    ;; once we dependend on lister>0.7.1
    (set-keymap-parent map (if (fboundp 'lister-keys-mode-map)
                               lister-keys-mode-map
                             lister-mode-map))
    map))

(define-derived-mode firefox-history-mode
  lister-mode "Firefox-history"
  "Major mode for browsing firefox history."
  ;; Setup lister first since it deletes all local vars:
  (lister-setup	(current-buffer) #'list
                        nil
                        (concat "firefox history Version " (firefox-history-version)))
  ;; --- Now add delve specific stuff:
  ;; do not mark searches:
  ;; (setq-local lister-local-marking-predicate #'delve-zettel-p)
  )

;;; Code:
(defun firefox-history (&rest args)
  "Call `firefox-history' with arguments ARGS."
  (let ((args (-reduce-from (lambda (x y) (concat x " " "\"" y "\"")) "" args)))
    (read
     (shell-command-to-string (concat firefox-history-location " " args)))))

(defun firefox-history-version ()
  "Get `firefox-history' version."
  (string-trim (shell-command-to-string (concat firefox-history-location " " "--version"))))

(defun firefox-history-check-if-visited (url)
  "Check if URL has been visited in the past by querying firefox database."
  (mapcar (lambda (x) (--> x
                           (car x)
                           (/ it 1000000)
                           (seconds-to-time it)
                           (format-time-string "[%Y-%m-%d %a %H:%M:%S]" it)
                           (cons (concat it " " (cadr x)) x)))
          (firefox-history "--elisp" "--visit" url)))

(defun firefox-history-new-buffer (items &optional heading buffer-name)
  "List firefox history ITEMS in a new buffer.

HEADING has to be a list item (a list of strings) which will be
used as a heading for the list. As special case, if HEADING is
nil, no heading will be displayed, and if HEADING is a string,
implictly convert it into a valid item.

The new buffer name will be created by using
`delve-buffer-name-format' with the value of BUFFER-NAME."
  (let* ((buf (generate-new-buffer (or buffer-name "*FIREFOX-HISTORY*"))))
    (with-current-buffer buf
      (firefox-history-mode)
      (lister-set-list buf items)
                                        ;(setq-local delve-local-initial-list items)
                                        ;(lister-set-header buf heading)
      (lister-goto buf :first)
      (lister-highlight-mode))
    (switch-to-buffer buf)
    buf))

(defun firefox-history-check-if-visited-progressively (url &optional fn)
  "Check if URL has been visited in the past by querying firefox database progressively.
   Call FN if URL has been found."
  (let ((urls (let ((org-roam-url-max-depth firefox-history-url-max-depth)) (org-roam-url--progressive-urls url)))
        (visited-dates '()))
    (cl-dolist (progressive-url urls)
      (if (and visited-dates firefox-history-stop-on-first-result)
          (cl-return visited-dates)
        (setq visited-dates (append visited-dates (firefox-history-check-if-visited progressive-url)))))
    (when visited-dates
      (if fn (funcall fn))
      (-some--> (helm :sources (helm-build-sync-source "firefox history"
                                 :candidates (reverse visited-dates) :filtered-candidate-transformer))
        (car it)
        (firefox-history "--elisp" "--chrono" "--time" it)
        (firefox-history-new-buffer it)
        ))))

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
