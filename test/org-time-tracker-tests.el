(require 'dash)

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

(defmacro ert-deftest-n-times (name runs body)
  (declare (indent 2))    
  (let ((fun-sym (gensym "test")))
    `(ert-deftest ,name ()
       (let ((,fun-sym (lambda (x) (progn
				     ,body 1))))  			 
	(-dotimes ,runs ,fun-sym)))))

(defconst MINUTES-IN-A-DAY
  (* 24 60))

(defconst HOURS-IN-A-DAY
  (* 24 60))

(defconst MINUTES-IN-A-HOUR
  (* 24 60))

(defconst TEST-STEPS
  (list 3 5 10 15 20))

(defalias 'get-random-test-step (partial #'seq-random-elt TEST-STEPS))

(with-temp-buffer
  (insert (orgtbl-to-orgtbl (list (make-list 5 "e")) (list)))
  (goto-char (org-table-begin))
  (org-table-get-field (org-table-current-column)))

(ert-deftest-n-times time-unfolder 100
  (-let* ((test-step (get-random-test-step))
	  (test-max-index (/ MINUTES-IN-A-DAY test-step))
	  (test-periods (-iota test-max-index 0 test-step))
	  ((actual-result actual-hour-and-minute)
	   (funcall (-compose (-juxt #'identity (-partial #'split ":")) #'time-unfolder) test-step test-index))
	  ((actual-hour actual-minute) (seq-map #'string-to-number actual-hour-and-minute)))
    (should (length= actual-result 4))
    (should (eql (mod actual-hour HOURS-IN-A-DAY) actual-hour))
    (should (zerop (mod actual-minute test-step)))))

(ert-deftest-n-times columns-creator 100
  (let* ((test-step (get-random-test-step))
	 ((actual-result actual-random-value)
	  (funcall (-compose (-juxt #'identity #'seq-random-elt ) #'columns-creator) test-step))
	 ((actual-result-car actual-result-cdr) actual-result))
    (should (length= actual-result 4))
    (should (zerop (mod actual-hour HOURS-IN-A-DAY)))
    (should (zerop (mod actual-hour )))))

(provide 'org-time-tracker-tests)
;;; org-time-tracker-tests.el ends here
