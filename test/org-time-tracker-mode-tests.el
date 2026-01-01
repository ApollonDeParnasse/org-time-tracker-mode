;;; org-time-tracker-tests.el --- Time tracking with org-tables -*- lexical-binding: t; -*-

;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: time

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

;; Track your time

;;; Code:

(require 'dash)
(require 'org-table)
(require 's)
(require 'org-time-tracker)

(defconst MINUTES-IN-A-DAY
  (* 24 60))

(defconst HOURS-IN-A-DAY
  (* 24 60))

(defconst MINUTES-IN-A-HOUR
  (* 24 60))

(defconst TEST-STEPS
  (list 3 5 10 15 20))


(defalias 'get-random-test-step (-partial #'seq-random-elt TEST-STEPS))
(defalias 'get-random-day-number (-partial #'seq-random-elt TEST-STEPS))
(defalias 'get-random-year (-partial #'random-nat-number-in-range (list 2000 3000)))

(ert-deftest-n-times time-unfolder 100
  (-let* ((test-step (get-random-test-step))
	  (test-max-index (/ MINUTES-IN-A-DAY test-step))
	  (test-periods (-iota test-max-index 0 test-step))
	  (test-index (random-nat-number-in-range-from-zero test-max-index))
	  ((actual-result actual-hour-and-minute)
	   (funcall (-compose (-juxt #'identity (-partial #'s-split ":")) #'car #'time-unfolder) test-step test-index))
	  ((actual-hour actual-minute) (seq-map-string-to-number actual-hour-and-minute)))
    (should (length= actual-result 5))
    (should (eql (mod actual-hour HOURS-IN-A-DAY) actual-hour))
    (should (zerop (mod actual-minute test-step)))))

(ert-deftest-n-times create-list-of-time-periods 100
  (-let* ((test-step (get-random-test-step))
	 ((expected-length expected-max-time-index) (funcall (-compose (-juxt #'identity #'2-) #'get-total-periods) test-step))
	 ((actual-result actual-first actual-last) (funcall (-compose (-juxt #'identity #'seq-first (-partial #'nth expected-max-time-index)) #'create-list-of-time-periods) test-step)))
    (should (eql (seq-length actual-result) expected-length))
    (should (string< actual-first actual-last))))

(ert-deftest-n-times create-list-of-time-headers 100
  (-let* ((test-step (get-random-test-step))
	 (expected-length (thread-last test-step (get-total-periods) (1-)))
	 ((actual-result (actual-random-start-time actual-random-end-time)) (funcall (-compose (-juxt #'identity (-compose #'split-on-dashes #'seq-take-one-random-value-from-seq)) #'create-list-of-time-headers) test-step)))
    (should (eql (seq-length actual-result) expected-length))
    (should (not (string= actual-random-start-time actual-random-end-time)))))

(ert-deftest-n-times date-unfolder 100
  (-let* ((test-day-number (get-random-day-number))
	  (test-year (get-random-year))
	  ((actual-date (actual-year actual-month actual-day))
	   (funcall (-compose (-juxt #'identity #'split-on-dashes) #'car #'date-unfolder) test-year test-day-number)))
    (should (length= actual-date 16))
    (should (equal actual-year (format "<%s" test-year)))
    (should (between-one-and-? 12 (string-to-number actual-month)))
    (should (between-one-and-? 31 (string-to-number actual-day)))))

(ert-deftest-n-times create-list-of-dates-for-year 100
  (-let* ((test-year (get-random-year))
	 (expected-length (days-in-year test-year))
	 ((actual-result actual-first actual-last) (funcall (-compose (-juxt #'identity #'seq-first #'seq-last) #'create-list-of-dates-for-year) test-year))
	 (expected-string-start (format "<%s" (number-to-string test-year))))
    (should (eql (seq-length actual-result) expected-length))
    (should (string< actual-first actual-last))
    (should (s-starts-with-p expected-string-start actual-first))
    (should (s-starts-with-p expected-string-start actual-last))))

(ert-deftest-n-times create-time-table-for-year 100
  (-let* ((test-year (get-random-year))
	 (expected-table-length (thread-last test-year (days-in-year) (1+)))  	 
	 (test-step (get-random-test-step))
	 (expected-row-length (get-total-periods test-step))
	 ((actual-time-table actual-random-row) (funcall (-compose (-juxt #'identity #'seq-take-one-random-value-from-seq) #'create-time-table-for-year) test-step test-year)))      
    (should (length= actual-time-table expected-table-length))
    (should (length= actual-random-row expected-row-length))))

(ert-deftest-n-times create-time-table-file-for-year 0
  (let* ((test-year (get-random-year))
	  (test-step 5)
	  (test-file-name (file-name-with-extension "earl" ".orgtbl"))
	  (test-file (file-name-concat "/tmp" test-file-name)))
    (create-time-table-file-for-year test-file test-step test-year)
    (with-temp-buffer
      (insert-file-contents test-file)
      (should (org-at-table-p)))))

(ert-deftest-n-times ott-incf-mod 0
  (-let* (((test-place test-delta) (generate-test-list-of-nat-numbers :min-length 2 :max-length 2))
	 (test-max (random-nat-number-in-range (list test-delta (+ test-place test-delta)))))
    (ott-incf-mod test-place test-max test-delta)
    (should (funcall (between-zero-and-? test-max) test-place))))

(ert-deftest-n-times ott-nth-mod 0
  (-let* ((test-list (generate-test-data))
	  (test-nth (random-nat-number-in-range (list 0 (seq-length test-list))))
	  (actual-val (ott-nth-mod test-nth test-list)))
    (should (member actual-val test-list))))

(ert-deftest-n-times ott-create-cell-options-closure 0
  (-let* ((test-options (generate-test-list-of-strings))
	 (test-closure (ott-create-cell-options-closure test-options))
	 (expected-closure-list (cons "" test-options))
	 (test-n (random-nat-number-in-range (list 0 (1- (length expected-closure-list)))))
	 ((test-current-val expected-val) (funcall (-juxt #'nth #'ott-nth-mod) test-n expected-closure-list)))
    (should (equal (funcall test-closure test-current-val) expected-val))))

(ert-deftest-n-times ott-create-toggle-cell-value 0
  (-let* (((test-list &as test-row-count test-column-count) (-times-no-args 2 #'random-nat-number-in-range-10))
	  ((test-row-number test-column-number) (seq-map (lambda (val) (random-nat-number-in-range (list 1 val))) test-list))
	  (test-options (generate-test-list-of-strings))
	  (test-current-value (seq-take-one-random-value-from-seq test-options))
	  (test-closure (ott-create-cell-options-closure test-options))
	  (test-ott-toggle-cell-value (-partial #'ott-create-cell-value-toggler test-closure)))
    (should
     (ott-with-buffer-with-test-table (list (cl-constantly test-current-value) test-row-count test-column-count)
       (org-table-goto-line test-row-number)
       (org-table-goto-column test-column-number)
       (funcall test-ott-toggle-cell-value)
       (member (org-table-get test-row-number test-column-number) test-options)))))

(ert-deftest-n-times org-table-autofill-string-right 0
  (-let* ((test-row-count (random-nat-number-in-range (list 2 10)))
	  (test-row-number (random-nat-number-in-range (list 1 test-row-count)))
	  (test-beg-column-number (random-nat-number-in-range (list 1 10)))
	  (test-end-column-number (random-nat-number-in-range (list (1+ test-beg-column-number) (+ 10 test-beg-column-number))))
	  (test-column-count (random-nat-number-in-range (list (1+ test-end-column-number) (+ 10 test-end-column-number))))
	  (test-cell-value "10"))
    (should
     (equal (ott-with-buffer-with-test-table (list (cl-constantly "1") test-row-count test-column-count)  				     
				   (org-table-goto-line-column test-row-number test-beg-column-number)
				   (org-table-get-field nil test-cell-value)
				   (setq test-beg (point))
				   (org-table-goto-column test-end-column-number)
				   (setq test-end (point))
				   (org-table-autofill test-beg test-end)
				   (org-table-align)
				   (org-no-properties-and-trim (org-table-get-field (random-nat-number-in-range (list test-beg-column-number test-end-column-number)))))
	    test-cell-value))))

(ert-deftest-n-times org-table-autofill-string-left 0
  (-let* ((test-row-count (random-nat-number-in-range (list 2 10)))
	  (test-row-number (random-nat-number-in-range (list 1 test-row-count)))
	  (test-end-column-number (random-nat-number-in-range (list 1 10)))
	  (test-beg-column-number (random-nat-number-in-range (list (1+ test-end-column-number) (+ 10 test-end-column-number))))
	  (test-column-count (random-nat-number-in-range (list (1+ test-beg-column-number) (+ 10 test-beg-column-number))))
	  (test-cell-value (generate-test-string)))
    (should
     (equal (ott-with-buffer-with-test-table (list (cl-constantly "1") test-row-count test-column-count)  				     
				   (org-table-goto-line-column test-row-number test-beg-column-number)
				   (org-table-get-field nil test-cell-value)
				   (org-table-align)
				   (setq test-beg (point))
				   (org-table-goto-column test-end-column-number)
				   (setq test-end (point))
				   (org-table-autofill test-beg test-end)
				   (org-table-align)
				   (org-no-properties-and-trim (org-table-get-field (random-nat-number-in-range (list test-end-column-number (1- test-beg-column-number))))))
	    test-cell-value))))

(ert-deftest-n-times org-table-autofill-string-down-right 0
  (-let* ((test-beg-column-number (random-nat-number-in-range (list 1 10)))
	  (test-end-column-number (random-nat-number-in-range (list (1+ test-beg-column-number) (+ 10 test-beg-column-number))))
	  (test-column-count (random-nat-number-in-range (list (1+ test-end-column-number) (+ 10 test-end-column-number))))  	  
	  (test-beg-row-number (random-nat-number-in-range (list 1 10)))
	  (test-end-row-number (random-nat-number-in-range (list (1+ test-beg-row-number) (+ 10 test-beg-row-number))))
	  (test-row-count (random-nat-number-in-range (list (1+ test-end-row-number) (+ 10 test-end-row-number))))
	  (test-cell-value (generate-test-string)))
    (should
     (equal (ott-with-buffer-with-test-table (list (cl-constantly "1") test-row-count test-column-count)  				     
				   (org-table-goto-line-column test-beg-row-number test-beg-column-number)
				   (org-table-get-field nil test-cell-value)
				   (setq test-beg (point))
				   (org-table-goto-line-column test-end-row-number test-end-column-number)
				   (setq test-end (point))
				   (org-table-autofill test-beg test-end)
				   (org-table-align)
				   (org-no-properties-and-trim (org-table-get (random-nat-number-in-range (list (1+ test-beg-row-number) test-end-row-number))
							       (random-nat-number-in-range (list (1+ test-beg-column-number) test-end-column-number)))))
	    test-cell-value))))

(ert-deftest-n-times org-table-autofill-string-up-left 0
  (-let* ((test-end-row-number (random-nat-number-in-range (list 1 10)))
	  (test-beg-row-number (random-nat-number-in-range (list (1+ test-end-row-number) (+ 10 test-end-row-number))))
	  (test-row-count (random-nat-number-in-range (list (1+ test-beg-row-number) (+ 10 test-beg-row-number))))  	  
	  (test-end-column-number (random-nat-number-in-range (list 1 10)))
	  (test-beg-column-number (random-nat-number-in-range (list (1+ test-end-column-number) (+ 10 test-end-column-number))))
	  (test-column-count (random-nat-number-in-range (list (1+ test-beg-column-number) (+ 10 test-beg-column-number))))
	  (test-cell-value (generate-test-string)))
    (should
     (equal (ott-with-buffer-with-test-table (list (cl-constantly "1") test-row-count test-column-count)  				     
				   (org-table-goto-line-column test-beg-row-number test-beg-column-number)
				   (org-table-get-field nil test-cell-value)
				   (org-table-align)
				   (setq test-beg (point))
				   (org-table-goto-line-column test-end-row-number test-end-column-number)
				   (setq test-end (point))
				   (org-table-autofill test-beg test-end)
				   (org-table-align)
				   (org-no-properties-and-trim (org-table-get (random-nat-number-in-range (list test-end-row-number (1- test-beg-row-number)))
							       (random-nat-number-in-range (list test-end-column-number (1- test-beg-column-number))))))
	    test-cell-value))))

(provide 'org-time-tracker-tests)
;;; org-time-tracker-tests.el ends here
