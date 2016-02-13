;;  form.lisp - A form of a figure.
;;  Copyright (C) 2006, 2016 Rob Myers rob@robmyers.org
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

(in-package "DRAW-SOMETHING")


(defconstant min-form-points 1)
(defconstant max-form-points 12)

(defconstant form-step-limit 5000)

(defconstant pen-width 1.0)

(defclass form ()
  ((skeleton :accessor skeleton
             :type vector
             :initarg :skeleton
             :initform (make-vector 5)
             :documentation "The guide shape for the outline.")
   (outline :accessor outline
            :type polyline
            :initform (make-instance 'polyline)
            :documentation "The outlines for the skeleton. Will be outline_s_.")
   (bounds :accessor bounds
           :type rectangle
           :initform (make-instance 'rectangle)
           :initarg :bounds
           :documentation "The bounds of the form.")
   (fill-colour :accessor fill-colour
                :type colour
                :initarg :colour
                :initform nil ;;(random-colour)
                :documentation "The flat body colour of the form."))
  (:documentation "A form drawn in the drawing."))

;; Skeleton will ultimately be generated from a list of objects, kept separately
;; Forms will be able to have no fill or no outline independently

(defun form-first-point (the-form)
  "Get the first point in the outline of the form."
  (first-point (outline the-form)))

(defun form-point-count (the-form)
  "The number of points in the outline of the form."
  (point-count (outline the-form)))

(defun most-recent-point (the-form)
  "The most recent point added to the outline of the form."
  (last-point (outline the-form)))

(defun make-form-start-point (form-skeleton pen-params)
  "Get the point to start drawing at."
  (let ((start-point nil))
    (dovector (skel form-skeleton)
      (let* ((hp (highest-leftmost-point skel))
             (candidate (make-instance 'point
                                       :x (x hp)
                                       :y (+ (y hp)
                                             (pen-distance pen-params)))))
        (when (or (not start-point)
                  (> (y candidate) (y start-point))
                  (and (= (y candidate) (y start-point))
                       (< (x candidate) (x start-point))))
          (setf start-point candidate))))
    start-point))

(defun make-form-turtle (the-form pen-params)
  "Make the turtle to draw around the form."
  (make-instance 'turtle
                 :location (make-form-start-point (skeleton the-form)
                                                  pen-params)
                 :direction (- (/ pi 2.0))))

(defun make-form-from-points (points)
  "Make a form, ready to be started."
  (advisory-message (format nil "Making form.~%"))
  (let* ((skel (make-polyline-from-points points))
         (the-form (make-instance 'form
                                    :skeleton (vector skel)
                                    :bounds (bounds skel))))
    ;;(draw-form the-form) ;; Remove for codelets
    the-form))

(defun path-ready-to-close (the-form pen-params)
  (and (> (form-point-count the-form) 2) ;; Ignore very first point
       (< (distance (most-recent-point the-form)
                    (form-first-point the-form))
          (move-step pen-params))))

(defun path-timeout (the-form)
  "Make sure that a failure of the form algorithm hasn't resulted in a loop."
  (> (form-point-count the-form)
     form-step-limit))

(defun should-finish-form (the-form pen-params)
  "Decide whether the form should finish."
  (or (path-ready-to-close the-form pen-params)
      (path-timeout the-form)))

(defun draw-form (the-form)
  "Find the next point forward along the drawn outline of the shape."
  (let* ((form-bounds (bounds the-form))
         (the-outline (outline the-form))
         (the-pen (choose-one-of plane-pen-parameters))
	 (the-turtle (make-form-turtle the-form the-pen)))
    (advisory-message "Drawing form.~%")
    (append-point the-outline (location the-turtle))
    (loop until (should-finish-form the-form the-pen)
       do (progn 
	    (adjust-next-pen (skeleton the-form) the-pen the-turtle)
	    (forward the-turtle (move-step the-pen))
	    (let ((new-location (location the-turtle)))
	      (append-point the-outline new-location)
	      (include-point form-bounds new-location))))
    (append-point the-outline (form-first-point the-form))))
