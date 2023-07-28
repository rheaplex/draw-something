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

(defclass <figure> ()
  ((forms :accessor forms
          :type vector
          :initform (make-array 1 :adjustable t :fill-pointer 0)
          :initarg :forms
          :documentation "The forms of the figure.")
   (composition-bounds :accessor composition-bounds
                       :type (or <rectangle> null)
                       :initform nil
                       :initarg :composition-bounds
                       :documentation "The assigned region for the form.")
   (bounds :accessor bounds
           :type (or <rectangle> null)
           :initarg :bounds
           :initform nil
           :documentation "The bounds of the figure."))
  (:documentation "A figure drawn in the drawing."))

(defun make-figure (composition-bounds &key (form nil))
  "Constructor function."
  (let ((fig (make-instance '<figure>
                            :composition-bounds composition-bounds)))
    (when form
      (add-form-to-figure form fig))
    fig))

(defmethod print-object ((object <figure>) stream)
  "Make a human readable string describing the figure."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(BOUNDS: ~a COMPOSITION-BOUNDS: ~a)" ;;  FORMS: ~a
          (bounds object)
          (composition-bounds object)
          ;;(forms object)
          )))

(defun form-count (figure)
  "Determine how many forms are in the figure."
  (length (forms figure)))

(defun add-form-to-figure (form figure)
  (vector-push-extend form (forms figure))
  (setf (bounds figure)
        (include-rectangle (bounds figure)
                           (bounds form))))

(defun make-figure-from-points (composition-bounds points)
  "Make a figure with a single polyline from the provided points."
  (log-info "Making figure.")
  (make-figure composition-bounds
               :form (make-form-from-points points)))

(defun choose-figure-form (figure)
  "Randomly choose a form of the figure."
  (choose-one-of (forms figure)))

(defun draw-figure (fig pen-params)
  "Draw the forms of a figure."
  (dotimes (i (form-count fig))
    (let ((form (aref (forms fig) i)))
      (log-info "Drawing figure.")
      (draw-form form pen-params)
      ;; If drawing failed, remove form.
      (unless (outline form)
        (log-err "Couldn't draw outline, removing form.")
        (remove-aref (forms fig) i)))))
