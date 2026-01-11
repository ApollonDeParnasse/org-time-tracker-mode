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
(require 'org-time-tracker-mode)
(load "~/.config/emacs/lib/generate/generate.el")

(defconst MINUTES-IN-A-DAY
  (* 24 60))

(defconst HOURS-IN-A-DAY
  (* 24 60))

(defconst MINUTES-IN-A-HOUR
  (* 24 60))

(defconst TEST-STEPS
  (list 3 5 10 15 20))


(defalias 'ott--get-random-test-step (-partial #'seq-random-elt TEST-STEPS))
(defalias 'ott--get-random-day-number (-partial #'seq-random-elt TEST-STEPS))
(defalias 'ott--get-random-year (-partial #'generate-random-nat-number-in-range (list 2000 3000)))

(generate-ert-deftest-n-times ott--time-unfolder ()
  (-let* ((test-step (ott--get-random-test-step))
	  (test-max-index (/ MINUTES-IN-A-DAY test-step))
	  (test-periods (-iota test-max-index 0 test-step))
	  (test-index (generate--random-nat-number-in-range-from-zero test-max-index))
	  ((actual-result actual-hour-and-minute)
	   (funcall (-compose (-juxt #'identity (-partial #'s-split ":")) #'car #'ott--time-unfolder) test-step test-index))
	  ((actual-hour actual-minute) (generate--seq-map-string-to-number actual-hour-and-minute)))
    (should (length= actual-result 5))
    (should (eql (mod actual-hour HOURS-IN-A-DAY) actual-hour))
    (should (zerop (mod actual-minute test-step)))))

(generate-ert-deftest-n-times ott--create-list-of-time-periods ()
  (-let* ((test-step (ott--get-random-test-step))
	 ((expected-length expected-max-time-index) (funcall (-compose (-juxt #'identity #'ott--2-) #'ott--get-total-periods) test-step))
	 ((actual-result actual-first actual-last) (funcall (-compose (-juxt #'identity #'seq-first (-partial #'nth expected-max-time-index)) #'ott--create-list-of-time-periods) test-step)))
    (should (eql (seq-length actual-result) expected-length))
    (should (string< actual-first actual-last))))

(generate-ert-deftest-n-times ott--create-list-of-time-headers ()
  (-let* ((test-step (ott--get-random-test-step))
	 (expected-length (thread-last test-step (ott--get-total-periods) (1-)))
	 ((actual-result (actual-random-start-time actual-random-end-time)) (funcall (-compose (-juxt #'identity (-compose #'ott--split-on-dashes #'generate-seq-take-random-value-from-seq)) #'ott--create-list-of-time-headers) test-step)))
    (should (eql (seq-length actual-result) expected-length))
    (should (not (string= actual-random-start-time actual-random-end-time)))))

(generate-ert-deftest-n-times ott--date-unfolder ()
  (-let* ((test-day-number (ott--get-random-day-number))
	  (test-year (ott--get-random-year))
	  ((actual-date (actual-year actual-month actual-day))
	   (funcall (-compose (-juxt #'identity #'ott--split-on-dashes) #'car #'ott--date-unfolder) test-year test-day-number)))
    (should (length= actual-date 16))
    (should (equal actual-year (format "<%s" test-year)))
    (should (generate--between-1-and-p 12 (string-to-number actual-month)))
    (should (generate--between-1-and-p 31 (string-to-number actual-day)))))

(generate-ert-deftest-n-times ott--create-list-of-dates-for-year ()
  (-let* ((test-year (ott--get-random-year))
	 (expected-length (ott--days-in-year test-year))
	 ((actual-result actual-first actual-last) (funcall (-compose (-juxt #'identity #'-first-item #'-last-item) #'ott--create-list-of-dates-for-year) test-year))
	 (expected-string-start (format "<%s" (number-to-string test-year))))
    (should (eql (seq-length actual-result) expected-length))
    (should (string< actual-first actual-last))
    (should (s-starts-with-p expected-string-start actual-first))
    (should (s-starts-with-p expected-string-start actual-last))))

(generate-ert-deftest-n-times ott-create-time-table-for-year ()
  (-let* ((test-year (ott--get-random-year))
	 (expected-table-length (thread-last test-year (ott--days-in-year) (1+)))  	 
	 (test-step (ott--get-random-test-step))
	 (expected-row-length (ott--get-total-periods test-step))
	 ((actual-time-table actual-random-row) (funcall (-compose (-juxt #'identity #'generate-seq-take-random-value-from-seq) #'ott--create-time-table-for-year) test-step test-year)))      
    (should (length= actual-time-table expected-table-length))
    (should (length= actual-random-row expected-row-length))))

(generate-ert-deftest-n-times ott-create-time-table-file-for-year ()
  :num-runs 0
  (let* ((test-year (ott--get-random-year))
	  (test-step 5)
	  (test-file-name (file-name-with-extension "earl" ".orgtbl"))
	  (test-file (file-name-concat "/tmp" test-file-name)))
    (ott-create-time-table-file-for-year test-file test-step test-year)
    (with-temp-buffer
      (insert-file-contents test-file)
      (should (org-at-table-p)))))

(generate-ert-deftest-n-times ott--incf-mod ()
  (-let* (((test-place test-delta) (generate-list-of-nat-numbers :exact-length 2))
	 (test-max (generate-random-nat-number-in-range (list test-delta (+ test-place test-delta)))))
    (ott--incf-mod test-place test-max test-delta)
    (should (funcall (generate--between-0-and-p test-max) test-place))))

(generate-ert-deftest-n-times ott--nth-mod ()
  (-let* ((test-list (generate-data))
	  (test-nth (generate-random-nat-number-in-range (list 0 (seq-length test-list))))
	  (actual-val (ott--nth-mod test-nth test-list)))
    (should (member actual-val test-list))))

(generate-ert-deftest-n-times ott--create-cell-options-closure-forward ()
  (-let* ((test-options (generate-random-list-of-words))
	  (test-func (lambda (x max) (mod (1+ x) max)))
	  (test-closure (ott--create-cell-options-closure test-func test-options))
	  (expected-closure-list (cons "" test-options))
	  (test-n (generate-random-nat-number-in-range (list 0 (1- (length expected-closure-list)))))
	  (test-current-val (nth test-n test-options))
	  (expected-val (ott--nth-mod test-n test-options)))
    (should (equal (funcall test-closure test-current-val) expected-val))))

(generate-ert-deftest-n-times ott--create-cell-value-toggler ()
  (-let* (((test-list &as test-row-count test-column-count) (generate--times-no-args 2 #'generate--random-nat-number-in-range-10))
	  ((test-row-number test-column-number) (seq-map (lambda (val) (generate-random-nat-number-in-range (list 1 val))) test-list))
	  (test-options (generate-random-list-of-words))
	  (test-current-value (generate-seq-take-random-value-from-seq test-options))
	  (test-func (list (lambda (x max) (mod (1+ x) max))))
	  (test-closure (ott--create-cell-options-closure test-func test-options))
	  (test-ott--toggle-cell-value (-partial #'ott--create-cell-value-toggler test-closure)))
    (should
     (generate-with-buffer-with-org-table-without-hlines (list (cl-constantly test-current-value) test-row-count test-column-count)
       (org-table-goto-line test-row-number)
       (org-table-goto-column test-column-number)
       (funcall test-ott--toggle-cell-value)
       (member (org-table-get test-row-number test-column-number) test-options)))))

;; Local Variables:
;; read-symbol-shorthands: (("ott-" . "org-time-tracker-mode-"))
;; End:
