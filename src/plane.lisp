;; plane.lisp - A plane (layer, level, plane) in the drawing.
;; Copyright (C) 2006, 2010, 2016 Rhea Myers.
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
;; A picture plane
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <plane> ()
  ((figures :accessor figures
            :initform (make-array 1 :adjustable t :fill-pointer 0)
            :initarg :figures
            :documentation "The figures of the plane.")
   (pen-params :accessor pen-params
               :initarg :pen-params
               :documentation "The pen configuration for the plane."))
  (:documentation "A plane of the drawing."))

(defun make-plane (&key pen-params)
  "Constuctor function."
  (make-instance '<plane> :pen-params pen-params))

(defun plane-forms-bounds (the-plane)
  "Get the bounding rectangles for every form of every figure on the plane"
  (let ((results (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for figure across (figures the-plane)
          do (loop for form across (forms figure)
                   do (vector-push-extend (bounds form) results)))
    results))

(defun find-space-on-plane (the-drawing the-plane required-size-rect)
  "Find empty space on the plane of given size, or nil if fail"
  ;; Efficiency decreases with number of figures. Cells would be constant.
  ;; FIXME: Uses bounds rather than outline of actual figure
  ;; TODO: Try figure bounds first. No intersection? proceed
  ;;       Next try form bounds.
  ;;       Next try form skeletons.
  ;;       Finally try form outlines.
  ;;  (assert (contains (bounds d) required-size))
  (let ((candidate (make-rectangle :x 0
                                   :y 0
                                   :width (width required-size-rect)
                                   :height (height required-size-rect)))
        (plane-rects (plane-forms-bounds the-plane))
        ;; The resulting rect must fit in within the drawing bounds
        (width-to-search (- (width (bounds the-drawing))
                            (width required-size-rect)))
        (height-to-search (- (height (bounds the-drawing))
                             (height required-size-rect)))
        (result nil))
    (block outside-the-loops
      ;; Use dotimesloop to ensure we don't find the top left space each time
      (dotimesloop (v 0 (random-range 0 height-to-search) height-to-search)
                   (setf (y candidate) v)
                   (dotimesloop (h 0 (random-range 0 width-to-search) width-to-search)
                                (setf (x candidate) h)
                                (when (intersects-none candidate plane-rects)
                                  (setf result candidate)
                                  (return-from outside-the-loops)))))
    result))

(defun find-space-on-plane-range (the-drawing the-plane
                                  min-size-rect max-size-rect steps)
  "Find empty space on the plane larger than min-size up to max-size, or nil"
  (let ((width-step-size (/ (- (width max-size-rect) (width min-size-rect))
                            steps))
        (height-step-size (/ (- (height max-size-rect) (height min-size-rect))
                             steps))
        (result nil))
    (dotimes (step steps)
      (let* ((required-size (make-rectangle :x 0
                                            :y 0
                                            :width (- (width max-size-rect)
                                                      (* width-step-size
                                                         step))
                                            :height (- (height max-size-rect)
                                                       (* height-step-size
                                                          step))))
             (candidate (find-space-on-plane the-drawing
                                             the-plane
                                             required-size)))
        (when candidate
          (setf result candidate)
          (return))))
    result))
