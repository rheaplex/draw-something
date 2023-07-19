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
;; Convex Hull
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hull-figure (drawing plane size-min size-max)
  "Make a hull figure."
  ;;FIXME: get random from plane?
  (let ((count 12)
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
      (let ((fig (make-figure-from-points (points (convex-hull (random-points-in-rectangle space
                                                                                           count))))))
        (vector-push-extend fig (figures plane))
        (draw-figure fig (pen-params plane))
        fig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-polygon-figure (drawing plane size-min size-max)
  (let* ((count 12)
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
      (let ((fig (make-figure-from-points (points (make-random-polyline-in-rectangle-sep space
                                                                                         count
                                                                                         (min-sep drawing))))))
        (vector-push-extend fig (figures plane))
        (draw-figure fig (pen-params plane))
        fig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-line-figure (drawing plane size-min size-max)
  "Make a line figure using two of the points."
  (let ((space (find-space-on-plane-range drawing
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
      (let* ((p1 (random-point-in-rectangle space))
             (p2 (random-point-in-rectangle space))
             ;;FIXME: Should be a line.
             (fig (make-figure-from-points (vector p1 p2))))
        (vector-push-extend fig (figures plane))
        (draw-figure fig (pen-params plane))
        fig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point-figure (drawing plane size-min size-max)
  "Make a point figure."
  (let* ((space (find-space-on-plane drawing
                                     plane
                                     (make-rectangle :x 0
                                                     :y 0
                                                     :width 1
                                                     :height 1))))
    (when space
      (let* ((p (random-point-in-rectangle space))
             ;;FIXME: should be a point.
             (fig (make-figure-from-points (vector p))))
        (vector-push-extend fig (figures plane))
        (draw-figure fig (pen-params plane))
        fig))))

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
