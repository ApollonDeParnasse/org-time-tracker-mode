;;; org-time-tracker-mode.el --- Time tracking with org-tables  -*- lexical-binding: t; -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: org
;; Package-Requires: ((emacs "30") (org "9.7") (dash "2.20.0") (s "1.13.1"))
;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Tracking

;;; Code:

(require 'time-date)
(require 'gv)
(require 'map)
(require 'org)
(require 'org-table)
(require 'dash)
(require 's)

(defgroup org-time-tracker-mode nil
  "Options for time-tracker-mode."
  :tag "org-time-tracker-mode"
  :group 'org)

(defcustom ott-cell-options-alist '(("le sommeil" . (:background "DimGray"))
				    ("la programmation" . (:background "LemonChiffon"))
				    ("le temps d'arrêt" . (:background "LightCoral"))
				    ("l'écriture" . (:background "MediumOrchid"))
				    ("la méditation" . (:background "PaleTurquoise"))
				    ("le temps perdu" . (:background "Cyan"))
				    ("la musculation" . (:background "RosyBrown"))
				    ("le travail" . (:background "DarkOliveGreen"))
				    ("la lecture" . (:background "DeepPink"))
				    ("le temps en famille" . (:background "LightSteelBlue"))
				    ("les jeux vidéos" . (:background "LightGoldenrodYellow"))
				    ("la socialisation" . (:background "CadetBlue1"))
				    ("les tâches" . (:background "MediumPurple1"))
				    ("la carrière" . (:background "DeepSkyBlue"))
				    ("les rendez-vous" . (:background "magenta4")))
  "Category-Color pairs."
  :group 'org-time-tracker-mode
  :type 'plist)

(defvar ott-cell-options-keys
  (map-keys ott-cell-options-alist))

(defalias 'ott--pad-zeros (-partial #'s-pad-left 2 "0"))
(defalias 'ott--format-pad (-compose #'ott--pad-zeros (lambda (x) (format "%s" x))))
(defalias 'ott--seq-map-string-to-number (-partial #'seq-map #'string-to-number))
(defalias 'ott--split-on-dashes (-partial #'s-split "-"))
(defalias 'ott--surround-text-with-brackets (lambda (x) (format "<%s>" x)))
(defalias 'ott--interpose-hlines (-partial #'-interpose 'hline))
(defalias 'ott--basic-tbl (-rpartial #'orgtbl-to-orgtbl '()))

(defalias 'ott--2- (-rpartial #'- 2))
(defalias 'ott--get-periods-per-hour (-partial #'/ 60))
(defalias 'ott--get-total-periods (-compose #'1+ (-partial #'* 24) #'ott--get-periods-per-hour))
(defalias 'ott--convert-index-into-hour-string (lambda (step index) (funcall (-compose (-rpartial #'mod 24) (-partial #'floor index) #'ott--get-periods-per-hour) step)))
(defalias 'ott--convert-index-into-minute-string (lambda (step index) (* (mod index (ott--get-periods-per-hour step)) step)))
(defalias 'ott--convert-index-into-hour-and-minute-strings (-compose (-partial #'seq-map #'ott--format-pad) (-juxt #'ott--convert-index-into-hour-string #'ott--convert-index-into-minute-string)))
(defalias 'ott--partition-in-steps-two (-partial #'-partition-in-steps 2 1))
(defalias 'ott--join-on-dashes (-rpartial #'string-join "-"))
(defalias 'ott--seq-map-join-on-dashes (-partial #'seq-map #'ott--join-on-dashes))

(defun ott--time-unfolder (step index)
  (unless (eql index (ott--get-total-periods step))
    (-let* (((hour minute) (ott--convert-index-into-hour-and-minute-strings step index))
	  (time (concat hour ":" minute)))
      (cons time (1+ index)))))

(defun ott--create-list-of-time-periods (step)
  (-unfold (-partial #'ott--time-unfolder step) 0))

(defalias 'ott--create-list-of-time-headers (-compose #'ott--seq-map-join-on-dashes #'ott--partition-in-steps-two #'ott--create-list-of-time-periods))

(defun ott--days-in-year (year)
  (if (date-leap-year-p year)
      366
    365))



(defalias 'ott--identity-or-zero (-orfn #'identity (cl-constantly 0)))
(defalias 'ott--seq-replace-nil-with-zero (-partial #'seq-map #'ott--identity-or-zero))
(defalias 'ott--org-time-formatter (-partial #'format-time-string "%Y-%m-%d %a"))
(defalias 'ott--slice-0-6 (-rpartial #'-slice 0 6))
(defalias 'ott--get-iso-date-for-day-number (-compose #'ott--surround-text-with-brackets #'ott--org-time-formatter #'encode-time #'ott--seq-replace-nil-with-zero #'ott--slice-0-6 #'date-ordinal-to-time))
(defalias 'ott--last-day-index-for-year (-compose (-partial #'+ 1) #'ott--days-in-year))

(defun ott--date-unfolder (year day-number)
  (unless (equal day-number (ott--last-day-index-for-year year))
    (cons (ott--get-iso-date-for-day-number year day-number) (1+ day-number))))

(defun ott--create-list-of-dates-for-year (year)
  (-unfold (-partial #'ott--date-unfolder year) 1))

(defun ott--create-list-of-empty-rows-with-headers (total-columns row-headers)
  (funcall (-compose (-partial #'-zip-with #'cons row-headers) (-partial #'-partition total-columns) (-rpartial #'make-list "") (-rpartial #'* total-columns) #'length) row-headers))


(defun ott--create-time-table-for-year (step year)
  (-let* (((column-headers total-periods) (funcall (-compose (-juxt #'identity #'seq-length) #'ott--create-list-of-time-headers) step))
	(columns (append (list "") column-headers))
	(rows (funcall (-compose (-partial #'ott--create-list-of-empty-rows-with-headers total-periods) #'ott--create-list-of-dates-for-year) year)))
    (append (list columns) rows)))

(defun ott-create-time-table-file-for-year (file-name step year)
  (let* ((table (funcall (-compose #'ott--basic-tbl #'ott--interpose-hlines #'ott--create-time-table-for-year) step year)))
    (with-work-buffer
      (insert table)
      (org-table-align)
      (write-file file-name))))

(defmacro ott--incf-mod (place max &optional delta)
  "Increment generalized variable PLACE by DELTA (default to 1) mod MAX.

The DELTA mod MAX is added to PLACE, and then stored in PLACE.
Return the incremented value of PLACE.

For more information about generalized variables, see Info node
`(elisp) Generalized Variables'."
  (declare (debug (gv-place &optional form)))
  (gv-letplace (getter setter) place
    (funcall setter `(mod (+ ,getter ,(or delta 1)) ,max))))

(defun ott--nth-mod (n list &optional delta)
  (nth (mod (+ n (or delta 1)) (seq-length list)) list))

(cl-defun ott--create-cell-options-closure (direction-func options)
  (let ((ht (make-hash-table :size (seq-length options) :test #'equal))
	(options-length (1+ (seq-length options))))
    (seq-do-indexed (lambda (value key)
		      (puthash (1+ key) value ht)
		      (puthash value (1+ key) ht))
		    options)
    (puthash 0 "" ht)
    (puthash "" 0 ht)
    (lambda (current-val) (let* ((current-index (map-elt ht current-val))
				 (next-index (funcall direction-func current-index options-length)))
			    (gethash next-index ht)))))

(defalias 'ott--org-table-get-field-clean (lambda () (org-trim (substring-no-properties (org-table-get-field)))))
(defun ott--create-cell-value-toggler (cycler)
  (lambda ()
    (interactive)
    (org-table-check-inside-data-field nil t)
    (let* ((current-val (ott--org-table-get-field-clean))
	 (next-value (funcall cycler current-val)))
    (org-table-get-field nil next-value))))

(defalias 'ott--backwards-cell-options-closure (ott--create-cell-options-closure (lambda (x max) (mod (1- x) max)) ott-cell-options-keys))
(defalias 'ott--forward-cell-options-closure (ott--create-cell-options-closure (lambda (x max) (mod (1+ x) max)) ott-cell-options-keys))
(defalias 'ott-toggle-backwards-cell-value (ott--create-cell-value-toggler #'ott--backwards-cell-options-closure))
(defalias 'ott-toggle-forward-cell-value (ott--create-cell-value-toggler #'ott--forward-cell-options-closure))


(defvar-keymap org-time-tracker-mode-map
  :doc "Keymap for `org-time-tracker-mode'."
  "C-c n" #'ott-toggle-forward-cell-value
  "C-c k" #'ott-toggle-backwards-cell-value)

;; consider using undo-auto-amalgamate for the repeat map
(defvar-keymap org-time-tracker-mode-repeat-map
  :doc "Keymap for `org-time-tracker-mode' when repeat-mode is activated."
  :repeat (:exit (org-table-align))
  "n" #'ott-toggle-forward-cell-value
  "k" #'ott-toggle-backwards-cell-value
  "a" #'org-table-align)


(defun org-time-tracker-mode-defun-base (ott-cell-options)
  "Setup org-time-tracker-mode."
  (setq-local cell-options-alist ott-cell-options))

;;;###autoload
(define-minor-mode org-time-tracker-mode
  "Time tracking mode for `org-mode'."
  :interactive 't
  :lighter " org-time-tracker-mode"
  (org-time-tracker-mode-defun-base ott-cell-options-alist))

(provide 'org-time-tracker-mode)
;;; org-time-tracker-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ott-" . "org-time-tracker-mode-"))
;; End:
