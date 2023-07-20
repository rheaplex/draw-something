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
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun existing-point-in-rectangle (drawing bounds exclude)
  "Find an existing point in bounds that is not in exclude,
   or nil if none can be found."
    (block all
      (do-drawing-forms (drawing form)
        (loop for skel across (skeleton form)
              do (loop for point across (points skel)
                       do (when (and (intersects point bounds)
                                     (not (find-if #'(lambda (p)
                                                       (point= p point))
                                                   exclude)))
                            (return-from all (copy-point point))))))
      nil))

(defun existing-points-in-rectangle (drawing bounds exclude count)
  "Pick up to count existing points in the already added planes
   that fall within bounds and are not listed in exclude.
   The returned vector may be shorter than count, or empty, if not enough
   points can be found that are not in exclude."
  (let ((points (make-vector 1)))
    (block all
      (do-drawing-forms (drawing form)
        (dolist (skel (skeletons form))
          (loop for point across (points skel)
                do (when (and (intersects point bounds)
                              (not (find-if #'(lambda (p)
                                                (point= p point))
                                            exclude)))
                     (vector-push-extend points (copy-point point)))
                   (when (= (length points) count)
                     (return-from all))))))
    points))

(defun pick-point-in-rectangle (drawing bounds exclude old-prob-percent)
  "Try to find an existing point within bounds old-prob-percent of the time.
   If that fails, and the rest of the time, generate a new random point
   within bounds."
  (or (and (< (random-number 100) old-prob-percent)
           (existing-point-in-rectangle drawing bounds exclude))
      (random-point-in-rectangle bounds)))

(defun random-polyline-in-rectangle-sep-existing (drawing
                                                  rect
                                                  count
                                                  sep
                                                  exclude
                                                  old-prob-percent)
  "Create a polyline with the given number of points in the given bounds,
   with each point separated from the others and their lines by at least
   sep. This is to avoid the pen getting trapped by small gaps between
   points or between points and lines."
  (assert (>= count 1))
  (let ((poly (make-polyline)))
    (append-point poly (pick-point-in-rectangle drawing
                                                rect
                                                exclude
                                                old-prob-percent))
    (loop while (< (point-count poly) count)
          do (let ((p (pick-point-in-rectangle drawing
                                               rect
                                               (concatenate 'vector
                                                            exclude
                                                            (points poly))
                                               old-prob-percent)))
               (when (>= (distance p poly) sep)
                 (when (or (< (point-count poly) 2)
                           (new-segment-far-enough-p p poly sep))
                   (append-point poly p)))))
    poly))

(defun random-point-on-existing-outline (drawing plane)
  "Choose a random existing line."
  ;;TODO: Exclude points that match existing lines on plane.
   (choose-random-polyline-point
    (outline (choose-drawing-form drawing))))

(defun random-line-on-existing-outline (drawing plane)
  "Choose a random existing line."
  ;;TODO: Exclude points that match existing lines on plane.
  (let ((poly (outline (choose-drawing-form drawing))))
    (choose-random-polyline-points-subsequence
     poly
     (random-range (floor (point-count poly) 20)
                   (floor (point-count poly) 4)))))

(defun random-line-on-existing-skeleton (drawing plane)
  "Choose a random existing line."
  ;;TODO: Exclude points that match existing lines on plane.
  (let ((l (choose-random-polyline-line
            (choose-one-of
             (skeleton (choose-drawing-form drawing))))))
    (vector (from l) (to l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Convex Hull
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (defun make-hull-figure (drawing plane size-min size-max)
      "Make a hull figure."
      ;;FIXME: get random from plane?
      (let ((count (random-range 5 32))
            (space (find-space-on-plane-range drawing
                                              plane
                                              (make-rectangle :x 0
                                                              :y 0
                                                              :width size-min
                                                              :height size-min)
                                              (make-rectangle :x 0
                                                              :y 0
                                                              :width size-max
                                                              :height size-max))))
        (when space
          ;; We ignore min-sep because the convex hull doesn't need it.
          ;; Just use random-points-in-rectangle here because we're first.
          (let ((fig (make-figure-from-points (points (convex-hull (random-points-in-rectangle space
                                                                                               count))))))
            (vector-push-extend fig (figures plane))
            (draw-figure fig (pen-params plane))
            fig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-polygon-figure (drawing plane size-min size-max)
  (let* ((count (random-range 4 16))
         (space (find-space-on-plane-range drawing
                                           plane
                                           (make-rectangle :x 0
                                                           :y 0
                                                           :width size-min
                                                           :height size-min)
                                           (make-rectangle :x 0
                                                           :y 0
                                                           :width size-max
                                                           :height size-max))))
    (when space
      (let* ((poly (random-polyline-in-rectangle-sep-existing drawing
                                                              space
                                                              count
                                                              (min-sep drawing)
                                                              #()
                                                              100))

             (fig (make-figure-from-points (points poly))))
        (vector-push-extend fig (figures plane))
        (draw-figure fig (pen-params plane))
        fig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-line-figure (drawing plane size-min size-max)
  "Make a line figure using two of the points."
  ;; (let ((space (find-space-on-plane-range drawing
  ;;                                         plane
  ;;                                         (make-rectangle :x 0
  ;;                                                         :y 0
  ;;                                                         :width size-min
  ;;                                                         :height size-min)
  ;;                                         (make-rectangle :x 0
  ;;                                                         :y 0
  ;;                                                         :width size-max
  ;;                                                         :height size-max))))
;;  (when space
    (let* (;;(p1 (pick-point-in-rectangle drawing space #() 50))
           ;;(p2 (pick-point-in-rectangle drawing space #() 50)
           (pts (random-line-on-existing-outline drawing plane))
           ;;FIXME: Should be a line not a poly.
           (fig (make-figure-from-points pts)))
      (vector-push-extend fig (figures plane))
      (draw-figure fig (pen-params plane))
      fig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point-figure (drawing plane size-min size-max)
  "Make a point figure."
  (let* (;;(p (pick-point-in-rectangle drawing space #() 90))
         (poly (outline (choose-drawing-form drawing)))
         (p (random-point-in-polyline poly))
         ;;FIXME: should be a point not a poly.
         (fig (make-figure-from-points (vector p))))
    (vector-push-extend fig (figures plane))
    (draw-figure fig (pen-params plane))
    fig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figure generation method selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *figure-generation-method-list*
  '(make-hull-figure
    make-polygon-figure
    make-line-figure
    make-point-figure))

(defun figure-generation-methods (count)
  (choose-n-of-ordered count *figure-generation-method-list*))
