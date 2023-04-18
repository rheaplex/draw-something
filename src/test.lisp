;; test.lisp - very simple test.
;; Copyright (C) 2023 Myers Studio Ltd.
;;
;; This file is part of draw-something.
;;
;; draw-something is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; draw-something is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:draw-something.test
  (:use #:cl #:log)
  (:nicknames #:test)
  (:export #:deftests
           #:expect
           #:run-tests))

(in-package #:draw-something.test)

(defvar *tests* '())

(defmacro expect ((value &key (test #'eql)) &body code)
  (let ((code-value (gensym))
        (result (gensym)))
    `(let* ((,code-value ,@code)
            (,result (funcall ,test ,value ,code-value)))
       (unless ,result
         (log:info "  FAILED: ~a" ',code)
         (log:info "  EXPECTED: ~a" ,value)
         (log:info "  GOT: ~a" ,code-value))
       ,result)))

(defmacro deftest (description &body tests)
  (let ((result (gensym))
        (early (gensym)))
    `(push (cons ,description
                 (lambda ()
                   (let ((,result t))
                     (block ,early
                       ,@(loop for test in tests
                               collect `(unless ,test
                                          (setf ,result nil)
                                          (return-from ,early)))
                       ,result))))
           *tests*)))

(defun run-tests ()
  (let ((tests-count (length *tests*))
        (total 0)
        (passed 0)
        (failed 0))
    (log:info "Running tests.")
    (dolist (test (reverse *tests*))
      (incf total)
      (log:info "~d/~d: ~a." total tests-count (car test))
      (if (funcall (cdr test))
          (incf passed)
          (incf failed)))
    (log:info "Ran ~d tests: ~d passed, ~d failed." total passed failed)))
