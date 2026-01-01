;;; org-time-tracker-mode.el --- Time tracking with org-tables -*- lexical-binding: t; -*-

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

(defcustom cell-options-alist '(("le sommeil" . (:background "grey"))
				("les jeux vidéos" . (:background "yellow"))
				("le temps d'arrêt" . (:background "orange"))
				("le temps en famille" . (:background "pink"))
				("la socialisation" . (:background "green"))
				("la musculation" . (:background "burgundy")) 
				("le travail" . (:background "white"))
				("la programmation" . (:background "blue"))  				
				("l'écriture" . (:background "silver"))
				("les rendez-vous" . (:background "red"))
				("le temp perdu" . (:background "pale yellow"))
				("les tâches" . (:background "dark blue")))
  "Category-Color pairs"
  :group 'org-time-tracker-mode
  :type 'plist)

(defvar cell-options-keys
  (map-keys cell-options-alist))

(defalias 'pad-zeros (-partial #'s-pad-left 2 "0"))
(defalias 'format-pad (-compose #'pad-zeros (lambda (x) (format "%s" x))))
(defalias 'seq-map-string-to-number (-partial #'seq-map #'string-to-number))
(defalias 'split-on-dashes (-partial #'s-split "-"))
(defalias 'surround-text-with-brackets (lambda (x) (format "<%s>" x)))
(defalias '-interpose-hlines (-partial #'-interpose 'hline))
(defalias 'basic-tbl (-rpartial #'orgtbl-to-orgtbl '()))

(defun org-test-table-alignment-asserter (function expected test-value)
  (should
   (equal expected
    (org-test-with-temp-text test-value
			     (funcall function)
			     (buffer-string)))))

(defalias '2- (-rpartial #'- 2))
(defalias 'get-periods-per-hour (-partial #'/ 60))
(defalias 'get-total-periods (-compose #'1+ (-partial #'* 24) #'get-periods-per-hour))  
(defalias 'convert-index-into-hour-string (lambda (step index) (funcall (-compose (-rpartial #'mod 24) (-partial #'floor index) #'get-periods-per-hour) step)))
(defalias 'convert-index-into-minute-string (lambda (step index) (* (mod index (get-periods-per-hour step)) step)))
(defalias 'convert-index-into-hour-and-minute-strings (-compose (-partial #'seq-map #'format-pad) (-juxt #'convert-index-into-hour-string #'convert-index-into-minute-string)))
(defalias '-partition-in-steps-two (-partial #'-partition-in-steps 2 1))
(defalias 'join-on-dashes (-rpartial #'string-join "-"))
(defalias 'seq-map-join-on-dashes (-partial #'seq-map #'join-on-dashes))

(defun time-unfolder (step index)
  (unless (eql index (get-total-periods step))
    (-let* (((hour minute) (convert-index-into-hour-and-minute-strings step index))
	    (time (concat hour ":" minute)))
      (cons time (1+ index)))))

(defun create-list-of-time-periods (step)
  (-unfold (-partial #'time-unfolder step) 0))

(defalias 'create-list-of-time-headers (-compose #'seq-map-join-on-dashes #'-partition-in-steps-two #'create-list-of-time-periods))

(defun days-in-year (year)
  (if (date-leap-year-p year)
      366
    365))



(defalias 'identity-or-zero (-orfn #'identity (cl-constantly 0)))
(defalias 'seq-replace-nil-with-zero (-partial #'seq-map #'identity-or-zero))
(defalias 'org-time-formatter (-partial #'format-time-string "%Y-%m-%d %a"))
(defalias '-slice-0-6 (-rpartial #'-slice 0 6))  
(defalias 'get-iso-date-for-day-number (-compose #'surround-text-with-brackets #'org-time-formatter #'encode-time #'seq-replace-nil-with-zero #'-slice-0-6 #'date-ordinal-to-time))  
(defalias 'last-day-index-for-year (-compose (-partial #'+ 1) #'days-in-year))

(defun date-unfolder (year day-number)
  (unless (eql day-number (last-day-index-for-year year))
    (cons (get-iso-date-for-day-number year day-number) (1+ day-number))))

(defun create-list-of-dates-for-year (year)
  (-unfold (-partial #'date-unfolder year) 1))

(defun create-list-of-empty-rows-with-headers (total-columns row-headers)
  (funcall (-compose (-partial #'-zip-with #'cons row-headers) (-partial #'-partition total-columns) (-rpartial #'make-list "") (-rpartial #'* total-columns) #'length) row-headers))


(defun create-time-table-for-year (step year)
  (-let* (((column-headers total-periods) (funcall (-compose (-juxt #'identity #'seq-length) #'create-list-of-time-headers) step))
	  (columns (append (list "") column-headers))
	  (rows (funcall (-compose (-partial #'create-list-of-empty-rows-with-headers total-periods) #'create-list-of-dates-for-year) year)))
    (append (list columns) rows)))

(defun create-time-table-file-for-year (file-name step year)
  (let* ((table (funcall (-compose #'basic-tbl #'-interpose-hlines #'create-time-table-for-year) step year)))
    (with-work-buffer
      (insert table)
      (org-table-align)
      (write-file file-name))))

(create-time-table-file-for-year "~/org/2026.orgtbl" 4 2026)

(defmacro ott-incf-mod (place max &optional delta)
  "Increment generalized variable PLACE by DELTA (default to 1) mod MAX.

The DELTA mod MAX is added to PLACE, and then stored in PLACE.
Return the incremented value of PLACE.

For more information about generalized variables, see Info node
`(elisp) Generalized Variables'."
  (declare (debug (gv-place &optional form)))
  (gv-letplace (getter setter) place
    (funcall setter `(mod (+ ,getter ,(or delta 1)) ,max))))

(let ((x 0))
  (ott-incf-mod x 4))

(defun ott-nth-mod (n list &optional delta)
  (nth (mod (+ n (or delta 1)) (seq-length list)) list))

(defun ott-create-cell-options-closure (options)
  (let ((ht (make-hash-table :size (seq-length options) :test #'equal))
	(options-length (1+ (seq-length options))))
    (seq-do-indexed (lambda (value key)
            (puthash (1+ key) value ht)
	    (puthash value (1+ key) ht))
          options)
  (puthash 0 "" ht)
  (puthash "" 0 ht)
  (lambda (current-val) (let ((current-index (map-elt ht current-val)))
			  (ott-incf-mod current-index options-length)
			  (gethash current-index ht)))))

;;(fmakunbound 'ott-create-cell-value-toggler)
(defalias 'org-table-get-field-clean (lambda () (org-trim (substring-no-properties (org-table-get-field)))))
;; this was slow, nows its not?
(defun ott-create-cell-value-toggler (cycler)
  (lambda ()
    (interactive)
    (org-table-check-inside-data-field nil t) 
    (let* ((current-val (org-table-get-field-clean))
	   (next-value (funcall cycler current-val)))      
    (org-table-get-field nil next-value))))

(defalias 'seq-fmakunbound (-partial #'seq-do #'fmakunbound))
(defalias 'seq-makunbound (-partial #'seq-do #'makunbound))
;;(seq-fmakunbound [ott-cell-options-closure ott-toggle-cell-value])
;;(seq-makunbound [org-time-tracker-mode org-time-tracker-mode-map])
(defalias 'ott-cell-options-closure (ott-create-cell-options-closure cell-options-keys))
(defalias 'ott-toggle-cell-value (ott-create-cell-value-toggler #'ott-cell-options-closure))

(defun ott-toggle ()
  (interactive)
    (print (org-table-get-field nil "slow")))

(defvar-keymap org-time-tracker-mode-map 
  :doc "Keymap for `org-time-tracker-mode."
  :repeat t
  "C-c i" #'ott-toggle-cell-value)


;; hashmap-with-numbers for chosen options + check current cell-value + plus repeat for toggle
(define-minor-mode org-time-tracker-mode
  "Time tracking mode"
  :lighter " org-time-tracker-mode")

(defsubst org-table-goto-line-column (line column)
  (org-table-goto-line line)
  (org-table-goto-column column))

(defsubst org-no-properties-and-trim (s &optional restricted)
    "Remove all text properties from string S and then trim the result.
    When RESTRICTED is non-nil, only remove the properties listed
     in `org-rm-props'."
    (if restricted (remove-text-properties 0 (length s) org-rm-props s)
      (set-text-properties 0 (length s) nil s))
    (org-trim s))

    (defsubst org-table-goto-char-and-get-column (char)
      (save-excursion
        (goto-char char)
        (org-table-check-inside-data-field nil t)
        (org-table-current-column)))

    (defsubst org-table-goto-char-and-get-line (char)
      (save-excursion
        (goto-char char)
        (org-table-check-inside-data-field nil t)
        (org-table-current-line)))

    ;;(fmakunbound 'org-table-get-fill-direction)
    ;;(fmakunbound 'org-table-autofill)
    (defsubst org-table-get-fill-direction (beg end)
      "Helper function for org-table-autofill.
      Use the BEG and END of selection to determine
      the direction of fill."
      (pcase (- beg end) 
        ((pred zerop) #'(lambda (x y) x))
        ((pred plusp) #'-)
        ((pred minusp) #'+)))

;; incrementing + prefix arguments are next
(defun org-table-autofill (beg end)
  "Copy the value of the first cell in a selection to all of the other cells.
This works in any direction: left, right, down right or up left.
With a prefix argument, if the field is a number, a timestamp,
or is either prefixed or suffixed with a number, it will be incremented while copying"
      (interactive (list (mark) (point)))  		
      (let* ((beg-col (org-table-goto-char-and-get-column beg))
    	 (end-col (org-table-goto-char-and-get-column end))
    	 (beg-row (org-table-goto-char-and-get-line beg))
    	 (end-row (org-table-goto-char-and-get-line end))
    	 (auto-fill-value (org-table-get beg-row beg-col))
    	 (columns (1+ (abs (- beg-col end-col))))
    	 (rows (1+ (abs (- beg-row end-row))))
    	 (left-or-right? (org-table-get-fill-direction beg-col end-col))
    	 (up-or-down? (org-table-get-fill-direction beg-row end-row)))
        (dotimes (row rows)
          (let ((line (funcall up-or-down? beg-row row)))
    	(dotimes (column columns)
    	  (org-table-put line (funcall left-or-right? beg-col column) auto-fill-value))))))

(provide 'org-time-tracker-mode)
;;; org-time-tracker-mode.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ott-" . "org-time-tracker-mode"))
;; End:
