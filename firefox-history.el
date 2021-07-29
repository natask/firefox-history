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
(require 'delve)
(require 'cl)
(require 'cl-lib)

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

(defcustom firefox-history-include-url-title 'nil
  "Show the title of url. Expensive to include url titles. Necessary to make https call."
  :type '(boolean)
  :group 'firefox-history)

(defvar firefox-history-url-title-plist 'nil
  "Plist mapping url to title.")
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

(evil-define-key* '(normal insert) firefox-history-mode-map
  (kbd "<return>") #'firefox-history-expand-toggle-sublist
  (kbd "<tab>") #'firefox-history-expand-toggle-sublist
  (kbd "b") #'firefox-history-item-backtrace
  (kbd "c") #'firefox-history-item-chrono
  (kbd "v") #'firefox-history-item-visit
  (kbd "yt") #'firefox-history-item-yank-time
  (kbd "yy") #'firefox-history-item-yank-url
  (kbd "yu") #'firefox-history-item-yank-url
  (kbd "yo") #'firefox-history-item-yank-url-org
  (kbd "yd") #'firefox-history-item-yank-url-title
  (kbd "r") #'firefox-history-reverse-buffer
  (kbd "i") #'firefox-history-reverse-buffer)

(define-derived-mode firefox-history-mode
  lister-mode "Firefox-history"
  "Major mode for browsing firefox history."
  ;; Setup lister first since it deletes all local vars:
  (lister-setup	(current-buffer) #'firefox-history-lister-view
                        nil
                        (concat "firefox history Version " (firefox-history-version)))
  ;; --- Now add delve specific stuff:
  ;; do not mark searches:
  ;; (setq-local lister-local-marking-predicate #'delve-zettel-p)
  )

;;; datatypes:

;;; * Root object, for displaying additional info

(cl-defstruct (firefox-history-item (:constructor firefox-history-make-item))
  time
  timestamp
  url
  title
  head)
;; -- presenting a zettel object:

(defvar firefox-history-pp-scheme
  '((firefox-history-pp:timestamp   (:set-face firefox-history-timestamp-face))
    (firefox-history-pp:title       (:set-face firefox-history-title-face))
    (firefox-history-pp:focus-title (:set-face firefox-history-focus-title-face)))
  "Pretty printing scheme for displaying firefox history item.
See `firefox-history-pp-line' for possible values.")

;;; lister functions:
(defun firefox-history-reverse-buffer (buf)
  "Refresh all items in BUF."
  (interactive (list (current-buffer)))
  (lister-reorder-this-level buf (point) 'reverse))

;; Act on the item at point

(defun firefox-history-item-chrono (buf pos)
  "Visualize chronology of item in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-chrono (firefox-history-item-time data)))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-item-backtrace (buf pos)
  "Visualize acktrace of item in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-backtrace-new-buffer (firefox-history-item-time data)))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-yank-evil (item)
  "Yank ITEM using evil. Message yanked string."
  (evil-set-register ?* item)
  (message "%s" item))

(defun firefox-history-item-yank-time (buf pos)
  "Yank time of item in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-yank-evil (number-to-string (firefox-history-item-time data))))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-item-yank-url (buf pos)
  "Yank url of item in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-yank-evil (firefox-history-item-url data)))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-item-yank-url-org (buf pos)
  "Yank url of item in org format in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-yank-evil (concat "[[" (firefox-history-item-url data) "][" (firefox-history-query-url-title data) "]]")))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-query-url-title (data)
  "Get url title of DATA. Query web if necessary."
  (if (string= (firefox-history-item-title data)
               (firefox-history-item-url data))
      (firefox-history-get-url-title (firefox-history-item-url data))
    (firefox-history-item-title data)))

(defun firefox-history-item-yank-url-title (buf pos)
  "Yank url title of item in org format in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-yank-evil (firefox-history-query-url-title data)))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-get-url-title (url)
  "Get title for URL through wget and sed shell call."
  (let ((shell-string (concat "xidel "
                              "\"" url "\""
                              " -s --extract //title --error-handling=500| head -1")))
    (string-trim (shell-command-to-string shell-string))))

(defun firefox-history-item-visit (buf pos)
  "Visit dates of item in BUF at point POS."
  (interactive (list (current-buffer) (point)))
  (unless (lister-item-p buf pos)
    (user-error "No item to visit"))
  (let ((data (lister-get-data buf pos)))
    (pcase data
      ((pred firefox-history-item-p) (firefox-history-check-if-visited-progressively (firefox-history-item-url data)))
      (_                     (error "Cannot visit this item")))))

(defun firefox-history-expand-toggle-sublist ()
  "Close or open the item's sublist at point."
  (interactive)
  (let* ((buf (current-buffer))
	 (pos (point)))
    (if (lister-sublist-below-p buf pos)
	(lister-remove-sublist-below buf pos)
      (firefox-history-expand-and-insert buf pos))))

(defun firefox-history-expand-and-insert (buf pos)
  "Determine expansion operators and insert results for item at POS.
Determine the expansion operator for the item at the indicated
position, collect the results and insert them as sublist.

BUF must be a valid lister buffer populated with delve items. POS
can be an integer or the symbol `:point'."
  (interactive (list (current-buffer) (point)))
  (let* ((position (pcase pos
		     ((and (pred integerp) pos) pos)
		     (:point (with-current-buffer buf (point)))
		     (_ (error "Invalid value for POS: %s" pos))))
	 (item     (lister-get-data buf position))
	 (sublist  (firefox-history-backtrace (firefox-history-item-time item))))
    (if sublist
	(with-temp-message "Inserting expansion results..."
	  (lister-insert-sublist-below buf position sublist))
      (user-error "No expansion found"))))

;;; Code:
(defun firefox-history (&rest args)
  "Call `firefox-history' with arguments ARGS."
  (let ((args (-reduce-from (lambda (x y) (concat x " " "\"" y "\"")) "" args)))
    (--> (shell-command-to-string (concat firefox-history-location " " args))
     (split-string it "\n" 't)
     (mapcar #'read it)
     (if (length> it 1)
       (progn
        ;; NOTE: check if the list is in fact a url-title plist.
       (setq firefox-history-url-title-plist (car (last it)))
       it)
     (setq firefox-history-url-title-plist 'nil)
     it)
     (car it))))

(defun firefox-history-elisp-title (&rest args)
  "Call `firefox-history' with arguments ARGS.
Queries for an elisp compatable result.
Queries for url title if `firefox-history-include-url-title'."
  (let ((args (-reduce-from (lambda (x y) (concat x " " "\"" y "\"")) "" args)))
    (--> (shell-command-to-string (concat firefox-history-location " " "--elisp" " " (if firefox-history-include-url-title "--title" "") " " args))
         (split-string it "\n" 't)
         (mapcar #'read it)
         (if (length> it 1)
             (progn
               ;; NOTE: check if the list is in fact a url-title plist.
               (setq firefox-history-url-title-plist (car (last it)))
               it)
           (setq firefox-history-url-title-plist 'nil)
           it)
         (car it))))

(defun firefox-history-version ()
  "Get `firefox-history' version."
  (string-trim (shell-command-to-string (concat firefox-history-location " " "--version"))))

(defun firefox-history-visit (url)
  "Get `firefox-history' visit dates of URL."
  (mapcar #'firefox-history-item
    (firefox-history-elisp-title "--visit" url)))

(defun firefox-history-chrono (visit-date)
  "Get `firefox-history' chronology of website visited on VISIT-DATE."
  (let ((visit-date (cond
                      ((stringp visit-date) visit-date)
                      ((numberp visit-date) (number-to-string visit-date)))))
  (-some--> (firefox-history-elisp-title "--chrono" "--time" visit-date)
  (firefox-history-parse-chronology it)
  (firefox-history-new-buffer it "Chronology"))))

(defun firefox-history-backtrace (visit-date)
  "Get `firefox-history' backtrace of website visited on VISIT-DATE."
  (let ((visit-date (cond
                     ((stringp visit-date) visit-date)
                     ((numberp visit-date) (number-to-string visit-date)))))
    (-some--> (firefox-history-elisp-title "--backtrace" "--time" visit-date)
      (firefox-history-parse-backtrace it))))

(defun firefox-history-backtrace-new-buffer (visit-date)
  "Get `firefox-history' ."
  (when-let ((backtr (firefox-history-backtrace visit-date)))
      (firefox-history-new-buffer backtr "Backtrace")))

(defun firefox-history-lister-view (entry)
  "Convert ENTRY to lister viewable string."
  (let* ((time (number-to-string (firefox-history-item-time entry)))
                                 (url (firefox-history-item-url entry))
                                 (timestamp (firefox-history-item-timestamp entry))
                                 (title (firefox-history-item-title entry))
                                 (head (firefox-history-item-head entry)))
                                (concat  (propertize timestamp 'face 'org-clock-overlay) " " (propertize title 'face (if head 'doom-dashboard-menu-title 'org-document-title)))))

(defun firefox-history-item (item &optional head)
  "Create firefox history item from ITEM.
HEAD signifies the search target."
  (let* ((time (car item))
         (url (cadr item))
         (timestamp (->> time
                         firefox-history-unix-time-to-seconds
                         firefox-history-unix-time-to-timestamp))
         (title (if firefox-history-url-title-plist
                    (lax-plist-get firefox-history-url-title-plist url)
                  url)))
    (firefox-history-make-item
     :time time
     :timestamp timestamp
     :url url
     :title title
     :head head)))

(defun  firefox-history-parse-chronology (chrono)
  "Receives a nested property list CHRONO of with `time' literal, under it `item', `left' and `right'.
Parses CHRONO for `lister' consumption."
  (cl-loop for entry in chrono
           collect (let*  ((chrono-of-item (-flatten-n 2 (cdr entry)))
                           (main-url (list (firefox-history-item (lax-plist-get chrono-of-item "item") 't)))
                           (left (mapcar #'firefox-history-item (lax-plist-get chrono-of-item "left")))
                           (right (mapcar #'firefox-history-item (lax-plist-get chrono-of-item "right"))))
                     (append left main-url right))))

(defun  firefox-history-parse-backtrace (backtr)
  "Receives a nested property list BACKTR of with `time' literal, under it `item' and `backtrace'.
Parses BACKTR for `lister' consumption."
  (cl-loop for entry in backtr
           collect (let*  ((item (-flatten-n 2 (cdr entry)))
                           (main-url (list (firefox-history-item (lax-plist-get item "item") 't)))
                           (backtrace-of-item (mapcar #'firefox-history-item (lax-plist-get item "backtrace"))))
                     (append main-url backtrace-of-item))))

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
      (lister-set-header buf heading)
      (lister-goto buf :first)
      (lister-highlight-mode))
    (switch-to-buffer buf)
    buf))

(defun firefox-history-unix-time-to-seconds (time)
  "Convert firefox unixtime in microseconds to unix time in seconds."
  (--> time
       (/ it 1000000)))

(defun firefox-history-unix-time-to-timestamp (time)
  (--> time
       (seconds-to-time it)
       (format-time-string "[%Y-%m-%d %a %H:%M:%S]" it)))

(defun firefox-history-check-if-visited-progressively (url &optional fn)
  "Check if URL has been visited in the past by querying firefox database progressively.
   Call FN if URL has been found."
  (let ((urls (let ((org-roam-url-max-depth firefox-history-url-max-depth)) (org-roam-url--progressive-urls url)))
        (visited-dates '()))
    (cl-dolist (progressive-url urls)
      (setq visited-dates (append visited-dates (firefox-history-visit progressive-url)))
      (when (and visited-dates firefox-history-stop-on-first-result)
        (cl-return visited-dates)))
    (when visited-dates
      (if fn (funcall fn))
     (firefox-history-new-buffer visited-dates "Visited dates"))))

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
