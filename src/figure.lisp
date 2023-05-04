;; figure.lisp - A drawn figure.
;; Copyright (C) 2006, 2016 Rhea Myers.
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

(in-package :draw-something)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A figure on a plane.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +min-forms+ 5)
(defconstant +max-forms+ 10)

(defclass <figure> (<tagged>)
  ((forms :accessor forms
          :type vector
          :initform (make-array 1 :adjustable t :fill-pointer 0)
          :initarg :forms
          :documentation "The forms of the figure.")
   (composition-bounds :accessor composition-bounds
                       :type <rectangle>
                       :initarg composition-bounds
                       :documentation "The assigned region for the form.")
   (bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The bounds of the figure."))
  (:documentation "A figure drawn in the drawing."))

(defun make-figure ()
  "Constructor function."
  (make-instance '<figure>))

(defun add-form-to-figure (form figure)
  (append form (forms figure))
  (setf (bounds figure)
        (include-rectangle (bounds figure)
                           (bounds form))))

#|(defun make-figure-from-points (points)
  "Make a figure with a single polyline from the provided points."
  (log-info "Making figure.")
  (let ((fig (make-figure>)))
    (vector-push-extend (make-form-from-points points)
                        (forms fig))
    fig))

(defun draw-figure (fig)
  "Draw the forms of a figure."
  (loop for form across (forms fig)
        do (log-info "Drawing figure.")
        do (draw-form form (choose-one-of *plane-pen-parameters*))))
|#
