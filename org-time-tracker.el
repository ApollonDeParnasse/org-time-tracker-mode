(require 'dash)
(require 's)

;;; org-time-tracker.el --- Time tracking with org-tables -*- lexical-binding: t; -*-

  ;; Author: Earl Chase
;; Maintainer: Earl Chase
;; Version: 0.0
;; Keywords: testing

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

;; This is a quickcheck clone.

;;; Code:

(defgroup org-time-tracker-mode nil
  "Options for time-tracker-mode."
  :tag "org-time-tracker-mode"
  :group 'org)

(defcustom categories-alist '(("le sommeil" . (:background "red")))
  "Category-Color pairs"
  :group 'org-time-tracker-mode
  :type 'plist)

;; challenge is to break up this code and make it shift left and right
(defun my/org-cycle-status ()
    "Cycle through custom status values in the current table column."
    (interactive)
    (let* ((current (org-table-get (org-table-current-line) (org-table-current-column)))
           (statuses '("TODO" "TO_BE_SENT" "SENT" "TO_BE_PAID" "PAID"))
           (next (or (cadr (member current statuses)) (car statuses))))
      (org-table-put (org-table-current-line) (org-table-current-column) next t))
    )



;; challenge is to break up this code and make it shift left and right
(defun my/org-cycle-status ()
    "Cycle through custom status values in the current table column."
    (interactive)
    (let* ((current (org-table-get (org-table-current-line) (org-table-current-column)))
           (statuses '("TODO" "TO_BE_SENT" "SENT" "TO_BE_PAID" "PAID"))
           (next (or (cadr (member current statuses)) (car statuses))))
     (org-table-put (org-table-current-line) (org-table-current-column) next t)))

(define-minor-mode org-time-tracker-mode
  "time-tracker-mode"
  :lighter " tracking..."
  (unless (eq major-mode 'org-mode)
    (user-error "Cannot turn org time tracker mode outside org-mode buffers"))
  (when org-time-tracker-mode
    (add-hook 'org-shiftright-hook 'my/org-cycle-status)))
