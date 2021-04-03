;;; test-my-package.el --- tests for my-package.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  kga

;; Author: kga(require 'ert) <kga@Thinkpad-OpenSUSE>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'ert)
(require 'my-ert)

(ert-delete-all-tests)

(if (featurep 'my-package)
    (unload-feature 'my-package t))
(require 'my-package "./my-package.el")


;;;; Copy lines
(ert-deftest test-my-copy-line/empty nil
  :tags '(my-copy-line)
  (should (equal ""
                 (my-ert-temp-buffer
                  ""
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (call-interactively 'my-copy-line)
                    (car kill-ring))
                  ))))


(ert-deftest test-my-copy-line/one-of-one-lines nil
  :tags '(my-copy-line)
  (should (equal "foobar"
                 (my-ert-temp-buffer
                  "foobar"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (call-interactively 'my-copy-line)
                    (car kill-ring))
                  ))))


(ert-deftest test-my-copy-line/two-of-two-lines nil
  :tags '(my-copy-line)
  (should (equal "foo"
                 (my-ert-temp-buffer
                  "foo
bar"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (call-interactively 'my-copy-line)
                    (car kill-ring))
                  ))))


(ert-deftest test-my-copy-line/first-of-two-lines-active-region nil
  :tags '(my-copy-line)
  (should (equal "foo"
                 (my-ert-temp-buffer
                  "foo
bar"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (call-interactively 'set-mark-command)
                    (end-of-line)
                    (call-interactively 'my-copy-line)
                    (car kill-ring))
                  ))))


(ert-deftest test-my-copy-line/first-of-two-lines-inactive-region nil
  :tags '(my-copy-line)
  (should (equal "foo"
                 (my-ert-temp-buffer
                  "foo
bar"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (call-interactively 'my-copy-line)
                    (car kill-ring))
                  ))))


(ert-deftest test-my-copy-line/rest-of-line-inactive-region nil
  :tags '(my-copy-line)
  (should (equal "oo"
                 (my-ert-temp-buffer
                  "foo"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (goto-char 2)
                    (call-interactively 'my-copy-line)
                    (car kill-ring))
                  ))))


(ert-deftest test-my-copy-line/point-moves-next-line-inactive-region nil
  :tags '(my-copy-line)
  (should (equal '("oo" 5)
                 (my-ert-temp-buffer
                  "foo
" ; Point should be here (character number 5)
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (goto-char 2)
                    (call-interactively 'my-copy-line)
                    `(,(car kill-ring) ,(point)))
                  ))))


(ert-deftest test-my-copy-line/point-moves-next-line-active-region nil
  :tags '(my-copy-line)
  (should (equal '("foo" 5)
                 (my-ert-temp-buffer
                  "foo
"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (call-interactively 'set-mark-command)
                    (end-of-line)
                    (call-interactively 'my-copy-line)
                    `(,(car kill-ring) ,(point))
                    )))))


(ert-deftest test-my-copy-line/prefix-nlines nil
  :tags '(my-copy-line)
  (should (equal "foo
bar"
                 (my-ert-temp-buffer
                  "foo
bar
eggs
spam"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (let ((current-prefix-arg 2))
                      (call-interactively 'my-copy-line))
                    (car kill-ring))))
          ))


(ert-deftest test-my-copy-line/prefix-nlines-reverse-1 nil
  :tags '(my-copy-line)
  (should (equal "spa"
                 (my-ert-temp-buffer
                  "foo
bar
eggs
spam"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (goto-char (- (point-max) 1)) ; start at spa|m
                    (let ((current-prefix-arg -1))
                      (call-interactively 'my-copy-line))
                    (car kill-ring))))
          ))


(ert-deftest test-my-copy-line/prefix-nlines-reverse-2 nil
  :tags '(my-copy-line)
  (should (equal "eggs
spa"
                 (my-ert-temp-buffer
                  "foo
bar
eggs
spam"
                  (lambda nil
                    (make-local-variable 'kill-ring)
                    (goto-char (- (point-max) 1)) ; start at spa|m
                    (let ((current-prefix-arg -2))
                      (call-interactively 'my-copy-line))
                    (car kill-ring))))
          ))


;; BUG not sure how to simulate command for appending purposes
;; (ert-deftest test-my-copy-line/reverse-prepend nil
;;   :tags '(my-copy-line)
;;   (should (equal "eggs
;; spa"
;;                  (my-ert-temp-buffer
;;                   "foo
;; bar
;; eggs
;; spam"
;;                   (lambda nil
;;                     (make-local-variable 'kill-ring)
;;                     (goto-char (- (point-max) 1)) ; start at spa|m
;;                     (let ((current-prefix-arg -2))
;;                       (call-interactively 'my-copy-line))
;;                     (car kill-ring))))
;;           ))


;; BUG not sure how to simulate command for appending purposes
;; (ert-deftest test-my-copy-line/copy-append-inactive-region nil
;;   :tags '(my-copy-line)
;;   (should (equal "foo
;; bar"
;;                  (my-ert-temp-buffer
;;                   "foo
;; bar
;; "
;;                   (lambda nil
;;                     (make-local-variable 'kill-ring)
;;                     ;; Call `my-copy-line' twice
;;                     (ert-simulate-command '(my-copy-line))
;;                     (ert-simulate-command '(my-copy-line))
;;                     (car kill-ring)
;;                     )))))





;;;; Blank lines
;;;;; Blank lines above
(ert-deftest test-my-package-get-blank-lines-above/empty nil
  (should (equal 0 (my-ert-temp-buffer
                    ""
                    'my-package-get-blank-lines-above))))


(ert-deftest test-my-package-get-blank-lines-above/not-empty nil
  (should (equal 0 (my-ert-temp-buffer
                    "foobar"
                    'my-package-get-blank-lines-above))))


(ert-deftest test-my-package-get-blank-lines-above/one-above nil
  (should (equal 1 (my-ert-temp-buffer
                    "
foobar"
                    (lambda nil
                      (forward-line)
                      (my-package-get-blank-lines-above))))))


(ert-deftest test-my-package-get-blank-lines-above/two-lines nil
  (should (equal 1 (my-ert-temp-buffer
                    "
foo

bar
"
                    (lambda nil
                      (forward-line 3)
                      (my-package-get-blank-lines-above))))))


(ert-deftest test-my-package-get-blank-lines-above/org-mode-headlines nil
  (should (equal 2 (my-ert-temp-buffer
                    "
** asdkpjasdlkjsad
:PROPERTIES:
:ID:       3a3b2554-15bb-40aa-9eae-ee97dbd23779
:END:


[[id:5967c236-64d2-45ad-8a05-f1301919466b][other]] ; Point is here
* alksdjasdlk
"
                    (lambda nil
                      (forward-line 7)
                      (end-of-line)
                      (my-package-get-blank-lines-above))))))


;;;;; Blank lines below
(ert-deftest test-my-package-get-blank-lines-below/empty nil
  (should (equal 0 (my-ert-temp-buffer
                    ""
                    'my-package-get-blank-lines-below))))


(ert-deftest test-my-package-get-blank-lines-below/not-empty nil
  (should (equal 0 (my-ert-temp-buffer
                    "foobar"
                    'my-package-get-blank-lines-below))))


(ert-deftest test-my-package-get-blank-lines-below/one-below nil
  (should (equal 1 (my-ert-temp-buffer
                    "foobar
"
                    (lambda nil
                      (my-package-get-blank-lines-below))))))


(ert-deftest test-my-package-get-blank-lines-below/two-lines nil
  (should (equal 2 (my-ert-temp-buffer
                    "
foo

bar

"
                    (lambda nil
                      (forward-line 3)
                      (my-package-get-blank-lines-below))))))


;;;; my-read-only
(ert-deftest test-set-buffer-read-only nil
  (with-temp-buffer
    (should (equal nil buffer-read-only))
    (my-package-set-buffer-read-only)
    (should (equal t buffer-read-only)))

  (with-temp-buffer
    (setq buffer-read-only t)
    (my-package-set-buffer-read-only)
    (should (equal t buffer-read-only))))


;;;; End of tests
(ert t)

(provide 'test-my-package)
;;; test-my-package.el ends here
