;; composition.lisp - Generating an image with some kind of intent.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Choosing Points and Lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun existing-point-in-rectangle (bounds existing)
  "Find an existing point in bounds that is not in exclude,
   or nil if none can be found."
  (let ((result nil))
    (dotimes (i (length existing))
      (let ((candidate (aref existing i)))
        (when (intersects candidate bounds)
          (setf result candidate)
          (remove-aref existing i))))
    result))

(defun pick-point-in-rectangle (bounds existing existing-percent)
  "Try to find an existing point within bounds existing-percent of the time.
   If that fails, and the rest of the time, generate a new random point
   within bounds."
  (or (and (< (random-number 100) existing-percent)
           (existing-point-in-rectangle bounds existing))
      (random-point-in-rectangle bounds)))

(defun random-polyline-in-rectangle-sep-existing (rect
                                                  count
                                                  sep
                                                  existing
                                                  existing-percent)
  "Create a polyline with the given number of points in the given bounds,
   with each point separated from the others and their lines by at least
   sep. This is to avoid the pen getting trapped by small gaps between
   points or between points and lines."
  (assert (>= count 1))
  (let ((poly (make-polyline))
        (tries 0))
    (append-point poly (pick-point-in-rectangle rect
                                                existing
                                                existing-percent))
    (loop while (< (point-count poly) count)
          do (let ((p (pick-point-in-rectangle rect
                                               existing
                                               existing-percent)))
               (unless p
                 (setf poly nil)
                 (return))
               (if (and (>= (distance p poly) sep)
                        (or (< (point-count poly) 2)
                            (new-segment-far-enough-p p poly sep)))
                   (append-point poly p))
               (incf tries)
               ;; Avoid getting stuck in a corner.
               (when (> tries 100)
                 (return))))
    poly))

(defun random-polyline-in-rectangle-sep-nosect-existing (rect
                                                         count
                                                         sep
                                                         existing
                                                         existing-percent)
  "Create a polyline with the given number of points in the given bounds,
   with each point separated from the others and their lines by at least
   sep. This is to avoid the pen getting trapped by small gaps between
   points or between points and lines."
  (assert (>= count 1))
  (let ((poly (make-polyline))
        (tries 0))
    (append-point poly (pick-point-in-rectangle rect
                                                existing
                                                existing-percent))
    (loop while (< (point-count poly) count)
          do (let ((p (pick-point-in-rectangle rect
                                               existing
                                               existing-percent)))
               (unless p
                 (setf poly nil)
                 (return))
               (if (and (>= (distance p poly) sep)
                        (or (< (point-count poly) 2)
                            (and (new-segment-far-enough-p p poly sep)
                                 ;; First intersection is last point of poly
                                 (= (length (intersections (make-line :from (last-point poly)
                                                                      :to p)
                                                           poly)) 1))))
                   (append-point poly p))
               (incf tries)
               ;; Avoid getting stuck in a corner
               (when (> tries 100)
                 (return))))
    poly))

(defun random-point-on-existing-outline (drawing)
  "Choose a random existing line."
  ;;TODO: Exclude points that match existing lines on plane.
  (choose-random-polyline-point
   (outline (choose-drawing-form drawing))))

(defun random-line-on-existing-outline (drawing)
  "Choose a random existing line."
  ;;TODO: Exclude points that match existing lines on plane.
  (let ((poly (outline (choose-drawing-form drawing))))
    (choose-random-polyline-points-subsequence
     poly
     (random-range (floor (point-count poly) 20)
                   (floor (point-count poly) 4)))))

(defun random-line-on-existing-skeleton (drawing)
  "Choose a random existing line."
  ;;TODO: Exclude points that match existing lines on plane.
  (let ((l (choose-random-polyline-line
            (choose-one-of
             (skeleton (choose-drawing-form drawing))))))
    (vector (from l) (to l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convex Hull
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hull-figure (drawing plane existing-points size-min size-max)
  "Make a hull figure."
  (declare (ignore existing-points))
  ;;FIXME: get random from plane?
  (let ((count (random-range 3 32))
        (room (find-space-on-plane-range drawing plane
                                         size-min size-min
                                         size-max size-max)))
    (when room
      ;; We ignore min-sep because the convex hull doesn't need it.
      ;; Just use random-points-in-rectangle here because we're first.
      (make-figure-from-points (points (convex-hull (random-points-in-rectangle
                                                     room
                                                     count)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-polyline-figure (drawing plane existing-points size-min size-max)
  (let* ((count (random-range 3 16))
         (room (find-space-on-plane-range drawing plane
                                          size-min size-min
                                          size-max size-max)))
    (when room
      (let ((poly (random-polyline-in-rectangle-sep-nosect-existing room
                                                                    count
                                                                    (min-sep drawing)
                                                                    existing-points
                                                                    80)))
        (when poly
          (make-figure-from-points (points poly)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-polygon-figure (drawing plane existing-points size-min size-max)
  (let* ((count (random-range 4 16))
         (room (find-space-on-plane-range drawing plane
                                          size-min size-min
                                          size-max size-max)))
    (when room
      (let* ((poly (random-polyline-in-rectangle-sep-existing room
                                                              count
                                                              (min-sep drawing)
                                                              existing-points
                                                              80)))
        (when poly
          (make-figure-from-points (points poly)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lineset
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-lineset-figure (drawing plane existing-points size-min size-max)
  (let* ((count (random-range 2 8))
         (room (find-space-on-plane-range drawing plane
                                          size-min size-min
                                          size-max size-max)))
    (when room
      (let ((form (make-instance '<form>)))
        (dotimes (i count)
          (add-skeleton-geometry
           form
           (make-line :from (pick-point-in-rectangle room
                                                     existing-points
                                                     80)
                      :to (pick-point-in-rectangle room
                                                   existing-points
                                                   80))))
        (make-figure :form form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Star
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-star-figure (drawing plane existing-points size-min size-max)
  (declare (ignore existing-points))
  (let* ((room (find-space-on-plane-range drawing plane
                                          size-min size-min
                                          size-max size-max)))
    (when room
      (let* ((count (random-range 3 12))
             (step (/ 360 count))
             (pts (make-vector 1))
             (centre (make-point :x (+ (x room) (/ (width room) 2.0))
                                 :y (+ (y room) (/ (height room) 2.0))))
             (distance (* (min (width room) (height room)) 0.475)))
        (dotimes (i count)
          (vector-push-extend centre pts)
          (vector-push-extend (polar-to-cartesian centre
                                                  distance
                                                  (* i step))
                              pts))
        (make-figure-from-points pts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-outline-figure (drawing plane existing-points size-min size-max)
  "Choose a section of an existing outline and draw around it."
  (declare (ignore plane existing-points size-min size-max))
  (let* ((pts (random-line-on-existing-outline drawing)))
  (make-figure-from-points pts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeleton-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-skeleton-line-figure (drawing plane existing-points size-min size-max)
  "Choose a section of an existing outline and draw around it."
    (declare (ignore plane existing-points size-min size-max))
  (let* ((pts (random-line-on-existing-skeleton drawing)))
    (make-figure-from-points pts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-line-figure (drawing plane existing-points size-min size-max)
  "Make a line figure using two of the points."
  (let ((room (find-space-on-plane-range drawing plane
                                          size-min size-min
                                          size-max size-max)))
    (when room
      (let* ((p1 (pick-point-in-rectangle room existing-points 50))
             (p2 (pick-point-in-rectangle room existing-points 50)))
        ;;FIXME: Should be a line not a poly.
        (make-figure-from-points (vector p1 p2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point-figure (drawing plane existing-points size-min size-max)
  "Make a point figure."
  (declare (ignore plane size-min size-max))
  (let* ((p (pick-point-in-rectangle (bounds drawing)
                                     existing-points
                                     90)))
    ;;FIXME: should be a point not a poly.
    (make-figure-from-points (vector p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existing Point On Outline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-outline-point-figure (drawing plane existing-points size-min size-max)
  "Make a point figure."
  (declare (ignore plane existing-points size-min size-max))
  (let* ((poly (outline (choose-drawing-form drawing)))
         (p (random-point-in-polyline poly)))
    ;;FIXME: should be a point not a poly.
    (make-figure-from-points (vector p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Existing Point On Skeleton
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-skeleton-point-figure (drawing plane existing-points size-min size-max)
  "Make a point figure."
  (declare (ignore plane existing-points size-min size-max))
  (let* ((poly (outline (choose-drawing-form drawing)))
         (p (random-point-in-polyline poly)))
    ;;FIXME: should be a point not a poly.
    (make-figure-from-points (vector p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figure generation method selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sorted in rough order of visual density/area coverage.

(defparameter *figure-generation-method-list*
  '(make-hull-figure
    make-polygon-figure
    make-polyline-figure
    make-lineset-figure
    ;;make-star-figure
    make-line-figure
    make-outline-figure
    ;;make-point-figure
    make-skeleton-point-figure
    make-outline-point-figure))

(defun figure-generation-methods (count)
  (choose-n-of-ordered count *figure-generation-method-list*))
