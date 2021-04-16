;;; test-my-package.el --- tests for my-package.el           -*- lexical-binding: t; -*-

;; Copyright (C) 2021  kga

;; Author: coffeepenbit <coffeepenbit@gmail.com>
;; Keywords: lisp
;; Package-requires: (buttercup)

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
(require 'buttercup)
(require 'my-package)

(defun temp-buffer-helper (buffer-text &optional func)
  "Useful for testing `org-mode' functions.

BUFFER-TEXT is the initial state of the `org-mode' buffer.

FUNC is what is ran after creating the buffer."
  (with-temp-buffer
    (insert buffer-text)
    (goto-char (point-min))
    (if func
        (funcall func)
      (buffer-string))))

(describe "my-copy-line"
  (before-each (setq inhibit-message t))
  (it "copies empty lines"
    (temp-buffer-helper
     ""
     (lambda nil
       (make-local-variable 'kill-ring)
       (call-interactively 'my-copy-line)
       (expect (car kill-ring) :to-equal ""))))
  (it "copies single lines"
    (temp-buffer-helper
     "foobar"
     (lambda nil
       (make-local-variable 'kill-ring)
       (call-interactively 'my-copy-line)
       (expect (car kill-ring) :to-equal "foobar"))))
  (it "copies 2 lines"
    (temp-buffer-helper
     "foo
bar"
     (lambda nil
       (make-local-variable 'kill-ring)
       (call-interactively 'my-copy-line)
       (expect (car kill-ring) :to-equal "foo"))))
  (it "copies lines in marked region"
    (temp-buffer-helper
     "foo
bar"
     (lambda nil
       (make-local-variable 'kill-ring)
       (call-interactively 'set-mark-command)
       (end-of-line)
       (call-interactively 'my-copy-line)
       (expect (car kill-ring) :to-equal "foo"))))
  (it "copies first of 2 lines"
    (temp-buffer-helper
     "foo
bar"
     (lambda nil
       (make-local-variable 'kill-ring)
       (call-interactively 'my-copy-line)
       (expect (car kill-ring) :to-equal "foo"))))
  (it "copies remainder of line"
    (temp-buffer-helper
     "foo"
     (lambda nil
       (make-local-variable 'kill-ring)
       (goto-char 2)
       (call-interactively 'my-copy-line)
       (expect (car kill-ring) :to-equal "oo"))))
  (it "moves point to next line if region is inactive"
    (temp-buffer-helper
     "foo
" ; Point should be here (character number 5)
     (lambda nil
       (make-local-variable 'kill-ring)
       (goto-char 2)
       (call-interactively 'my-copy-line)
       (expect `(,(car kill-ring) ,(point)) :to-equal '("oo" 5)))))
  (it "moves point next line if region is active"
    (temp-buffer-helper
     "foo
"
     (lambda nil
       (make-local-variable 'kill-ring)
       (call-interactively 'set-mark-command)
       (end-of-line)
       (call-interactively 'my-copy-line)
       (expect `(,(car kill-ring) ,(point)) :to-equal '("foo" 5)))))
  (it "copies nlines when called with prefix"
    (temp-buffer-helper
     "foo
bar
eggs
spam"
     (lambda nil
       (make-local-variable 'kill-ring)
       (let ((current-prefix-arg 2))
         (call-interactively 'my-copy-line))
       (expect (car kill-ring) :to-equal "foo
bar")))
    (temp-buffer-helper
     "foo
bar
eggs
spam"
     (lambda nil
       (make-local-variable 'kill-ring)
       (goto-char (- (point-max) 1)) ; start at spa|m
       (let ((current-prefix-arg -1))
         (call-interactively 'my-copy-line))
       (expect (car kill-ring) :to-equal "spa")))
    (temp-buffer-helper
     "foo
bar
eggs
spam"
     (lambda nil
       (make-local-variable 'kill-ring)
       (goto-char (- (point-max) 1)) ; start at spa|m
       (let ((current-prefix-arg -2))
         (call-interactively 'my-copy-line))
       (expect (car kill-ring) :to-equal "eggs
spa")))))

;; BUG not sure how to simulate command for appending purposes
;; (describe "my-copy-line/reverse-prepend"
;;   (it "description" (expect "eggs :to-equal
;; spa"
;;                  (temp-buffer-helper
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
;; (describe "my-copy-line/copy-append-inactive-region"
;;   (it "description" (expect "foo :to-equal
;; bar"
;;                  (temp-buffer-helper
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

(describe "my-package-get-blank-lines-above"
  (it "counts empty buffer as 0 blank lines"
    (temp-buffer-helper
     ""
     (expect (my-package-get-blank-lines-above) :to-equal 0)))
  (it "counts 1 non-empty line as 0"
    (temp-buffer-helper
     "foobar"
     (expect (my-package-get-blank-lines-above) :to-equal 0)))
  (it "counts 1 blank line above as 1"
    (temp-buffer-helper
     "
foobar"
     (lambda nil
       (forward-line)
       (expect (my-package-get-blank-lines-above) :to-equal 1)))
    (temp-buffer-helper
     "
foo

bar
"
     (lambda nil
       (forward-line 3)
       (expect (my-package-get-blank-lines-above) :to-equal 1))))
  (it "counts 2 blank lines above as 2"
    (temp-buffer-helper
     "** asdkpjasdlkjsad
:PROPERTIES:
:ID:       3a3b2554-15bb-40aa-9eae-ee97dbd23779
:END:


[[id:5967c236-64d2-45ad-8a05-f1301919466b][other]]
* alksdjasdlk
"  ; Point at EOL of link
     (lambda nil
       (org-mode)
       (forward-line 6)
       (end-of-line)
       (expect (my-package-get-blank-lines-above) :to-equal 2)))))

(describe "my-package-get-blank-lines-below"
  (it "counts empty as 0 blank lines"
    (temp-buffer-helper
     ""
     (expect (my-package-get-blank-lines-below) :to-equal 0)))
  (it "counts 1 non-empty line as 0"
    (temp-buffer-helper
     "foobar"
     (expect (my-package-get-blank-lines-below) :to-equal 0)))
  (it "counts 1 blank line below as 1"
    (temp-buffer-helper
     "foobar
"
     (lambda nil
       (expect (my-package-get-blank-lines-below) :to-equal 1))))
  (temp-buffer-helper
   "
foo

bar

"
   (lambda nil
     (forward-line 3)
     (expect (my-package-get-blank-lines-below) :to-equal 2))))

(describe "my-package-corresponding-file"
  (it "returns the test filename of the given source filename"
    (expect "test-main.el" :to-equal (my-package-corresponding-file "main.el")))
  (it "returns the source filename of the given test filename"
    (expect "main.el" :to-equal (my-package-corresponding-file "test-main.el"))))

(describe "my-package-corresponding-test-file"
  (it "returns the test filename of the given source filename"
    (expect "test-main.el" :to-equal (my-package-corresponding-test-file "main.el"))))

(describe "my-package-corresponding-source-file"
  (it "returns the source filename of the given test filename"
    (expect "main.el" :to-equal (my-package-corresponding-source-file "test-main.el"))))

(describe "my-package-toggle-dedicated-buffer"
  (it "makes non-dedicated buffer dedicated"
    (expect (window-dedicated-p) :to-be nil)
    (my-package-toggle-window-dedicated)
    (expect (window-dedicated-p)))
  (it "makes dedicated buffer non-dedicate"
    (set-window-dedicated-p (get-buffer-window (current-buffer))
                            'dedicated)
    (expect (window-dedicated-p))
    (my-package-toggle-window-dedicated)
    (expect (window-dedicated-p) :to-be nil)))

(provide 'test-my-package)
;;; test-my-package.el ends here
