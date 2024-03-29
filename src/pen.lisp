;; pen.lisp - The pen to draw around skeletal forms with.
;; Copyright (C) 2007, 2016, 2021 Rhea Myers.
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
;; A pen configuration for drawing a form with a turtle.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <pen-parameters> ()
  ((turn-step          :accessor turn-step
                       :initform 1.0
                       :initarg :turn-step
                       :documentation "How many radians the turtle turns at a time.")
   (move-step          :accessor move-step
                       :initform 1.0
                       :initarg :move-step
                       :documentation "How far the turtle moves each step.")
   (distance           :accessor pen-distance
                       :initarg :distance
                       :documentation "How far from the skeleton to draw.")
   (distance-tolerance :accessor distance-tolerance
                       :initarg :distance-tolerance
                       :documentation "How far from the distance is tolerable.")
   (drift-probability  :accessor drift-probability
                       :initarg :drift-probability
                       :documentation "How likely it is the pen will wobble.")
   (drift-range        :accessor drift-range
                       :initarg :drift-range
                       :documentation "The +/- pen wobble range."))
  (:documentation "A set of parameters for a pen to use."))

(defun make-pen-parameters (&key turn-step move-step
                              distance distance-tolerance
                              drift-probability drift-range)
  "Constuctor function."
  (make-instance '<pen-parameters> :turn-step turn-step
                                   :move-step move-step
                                   :distance distance
                                   :distance-tolerance distance-tolerance
                                   :drift-probability drift-probability
                                   :drift-range drift-range))

(defmethod print-object ((object <pen-parameters>) stream)
  "Make a human readable string describing the params."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(TURN-STEP: ~,2f MOVE-STEP: ~,2f DISTANCE: ~,2f DIST-TOLERANCE: ~,2f DRIFT-PROB: ~,2f DRIFT-RANGE: ~,2f)"
          (turn-step object)
          (move-step object)
          (pen-distance object)
          (distance-tolerance object)
          (drift-probability object)
          (drift-range object))))

(defun generate-pen-parameters (step-min step-max
                                distance-min distance-max
                                tolerance-min tolerance-max
                                count index)
  "Generate pen parameters within a range."
  (make-pen-parameters :move-step          (lstep step-min
                                                  step-max
                                                  count
                                                  index)
                       :distance           (lstep distance-min
                                                  distance-max
                                                  count
                                                  index)
                       :distance-tolerance (lstep tolerance-min
                                                  tolerance-max
                                                  count
                                                  index)
                       :turn-step          0.1
                       :drift-probability  0.0
                       :drift-range        0.1))

(defun next-pen-distance (skeleton-forms pen-params the-turtle)
  "How far the pen will be from the guide shape when it next moves forwards."
  (let ((dist most-positive-single-float)
        (p (next-point the-turtle (move-step pen-params) )))
    (loop for skel across skeleton-forms
          do (let ((new-dist (distance p skel)))
               (when (< new-dist dist)
                 (setf dist new-dist))))
    dist))

(defun next-pen-too-close (skeleton-forms pen-params the-turtle)
  "Will the pen move to be too close from the guide shape next time?"
  (< (random-number (distance-tolerance pen-params))
     (- (next-pen-distance skeleton-forms pen-params the-turtle)
        (pen-distance pen-params))))

(defun next-pen-too-far (skeleton-forms pen-params the-turtle)
  "Will the pen move to be too far from the guide shape next time?"
  (< (random-number (distance-tolerance pen-params))
     (- (pen-distance pen-params)
        (next-pen-distance skeleton-forms pen-params the-turtle))))

(defun ensure-next-pen-far-enough (skeleton-forms pen-params the-turtle)
  "If the pen would move too close next time, turn it left until it wouldn't."
  (loop while (next-pen-too-close skeleton-forms pen-params the-turtle)
        do (left the-turtle (random-number (turn-step pen-params)))))

(defun ensure-next-pen-close-enough (skeleton-forms pen-params the-turtle)
  "If the pen would move too far next time, turn it right until it wouldn't."
  (loop while (next-pen-too-far skeleton-forms pen-params the-turtle)
        do (right the-turtle (random-number (turn-step pen-params)))))

(defun drift-pen-direction (pen-params the-turtle)
  "Adjust the pen's direction to simulate human neurophysiological noise."
  (if (< (random-number 1.0) (drift-probability pen-params))
      (turn the-turtle
            (random-range (- (drift-range pen-params))
                          (drift-range pen-params)))))

(defun adjust-next-pen (skeleton-forms pen-params the-turtle)
  "Drift or correct the pen's heading around the shape."
  (drift-pen-direction pen-params the-turtle)
  (ensure-next-pen-far-enough skeleton-forms pen-params the-turtle)
  (ensure-next-pen-close-enough skeleton-forms pen-params the-turtle))
