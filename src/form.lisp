;; form.lisp - A form within a figure.
;; Copyright (C) 2006, 2016, 2021 Rhea Myers.
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
;; A form in a figure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +form-step-limit+ 10000)

(defclass <form> ()
  ((skeleton :accessor skeleton
             :type vector
             :initarg :skeleton
             :initform (make-array 1 :adjustable t :fill-pointer 0)
             :documentation "The guide shapes for the outline.")
   (outline :accessor outline
            :type (or <polyline> nil)
            :initform (make-polyline)
            :documentation "The outlines for the skeleton.")
   (bounds :accessor bounds
           :type (or <rectangle> null)
           :initarg :bounds
           :initform nil
           :documentation "The bounds of the form.")
   (fill-colour :accessor fill-colour
                :initarg :fill-colour
                :initform nil
                :documentation "The flat body colour of the form.")
   (stroke-colour :accessor stroke-colour
                  :initarg :stroke-colour
                  :initform nil
                  :documentation "The outline colour of the form.")
   (stroke-width :accessor stroke-width
                 :initarg :stroke-width
                 :initform 2.0 ;;FIXME
                 :documentation "The outline colour of the form."))
  (:documentation "A form drawn in the drawing."))

(defun make-form (&key skeleton)
  "Constructor for single-shape skeleton."
  (let ((fm (make-instance '<form> :skeleton (vector skeleton))))
    (setf (bounds fm) (bounds skeleton))
    fm))

(defun make-form-from-points (points)
  "Make a form, ready to be started."
  (log-info "Making form.")
  (let* ((skel (make-polyline-from-points points))
         (the-form (make-form :skeleton skel)))
    the-form))

(defun add-skeleton-geometry (form skeleton)
  (vector-push-extend skeleton (skeleton form))
  (setf (bounds form) (include-rectangle (bounds form) (bounds skeleton))))

(defmethod print-object ((object <form>) stream)
  "Make a human readable string describing the form."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(BOUNDS: ~a FILL: ~a STROKE: ~a STROKE-WIDTH ~a SKELETON: ~a OUTLINE: ~a)"
            (bounds object)
            (fill-colour object)
            (stroke-colour object)
            (stroke-width object)
            (skeleton object)
            (outline object))))

;; Skeleton will ultimately be generated from a list of objects, kept separately
;; Forms will be able to have no fill or no outline independently

(defun outline-first-point (the-form)
  "Get the first point in the outline of the form."
  (first-point (outline the-form)))

(defun outline-point-count (the-form)
  "The number of points in the outline of the form."
  (point-count (outline the-form)))

(defun most-recent-outline-point (the-form)
  "The most recent point added to the outline of the form."
  (last-point (outline the-form)))

(defun make-outline-start-point (form-skeleton pen-params)
  "Get the point to start drawing at."
  (let ((start-point nil))
    (loop for skel across form-skeleton
          do (let* ((hp (highest-leftmost-point skel))
                    (candidate (make-point :x (x hp)
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
  (make-turtle :location (make-outline-start-point (skeleton the-form)
                                                   pen-params)
               :direction 0.0))

(defun outline-ready-to-close (the-form pen-params)
  (and (> (outline-point-count the-form) 2) ;; Ignore very first point
       (< (distance (most-recent-outline-point the-form)
                    (outline-first-point the-form))
          (move-step pen-params))))

(defun outline-timed-out (the-form)
  "Make sure that outlining hasn't failed and resulted in an endless loop."
  (> (outline-point-count the-form) +form-step-limit+))

(defun draw-form (the-form pen-params)
  "Find the next point forward along the drawn outline of the shape."
  (let* ((the-outline (outline the-form))
         (the-turtle (make-form-turtle the-form pen-params)))
    (log-info "Drawing form.")
    (append-point the-outline (location the-turtle))
    (loop until (outline-ready-to-close the-form pen-params)
          do (adjust-next-pen (skeleton the-form) pen-params the-turtle)
             (forward the-turtle (move-step pen-params))
             (let ((new-location (location the-turtle)))
               (append-point the-outline new-location)
               (setf (bounds the-form)
                     (include-point (bounds the-form) new-location)))
             (when (outline-timed-out the-form)
               (setf (outline the-form) nil)
               (return)))
    (when (outline the-form)
      (append-point the-outline (outline-first-point the-form)))))
