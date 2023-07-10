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
;; Generating the point population for the composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-composition-points (the-drawing count)
  "Generate the points on the image plane that the composition will use."
  (log-info "Making ~d composition points." count)
  ;;FIXME
  (let* ((b (bounds the-drawing))
         (corner-count (random-number 4))
         (interior-count (- count corner-count))
         (edge-count (- count
                        interior-count
                        corner-count))
         )
    (setf (composition-points the-drawing)
          (concatenate 'vector
                       ;;(random-points-at-rectangle-corners b corner-count)
                       ;;(random-points-on-rectangle b edge-count)
                       (random-points-in-rectangle b interior-count)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convex Hull
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hull-figure (drawing plane)
  "Make a hull figure."
  ;;FIXME: get random from plane?
  (let* ((count 12)
         (pts (points (convex-hull (random-points-in-rectangle (bounds drawing)
                                                               count))))
         ;; We ignore min-sep because the convex hull doesn't need it.
         (fig (make-figure-from-points pts)))
    (vector-push-extend fig (figures plane))
    (draw-figure fig (pen-params plane))
    fig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polygon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-polygon-figure (drawing plane)
  ;;FIXME: get random from plane?
  (let* ((count 12)
         (skel (make-random-polyline-in-rectangle-sep (bounds drawing)
                                                      count
                                                      (min-sep drawing)))
         ;;FIXME: this is a pointless copy
         (fig (make-figure-from-points (points skel))))
    (vector-push-extend fig (figures plane))
    (draw-figure fig (pen-params plane))
    fig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-line-figure (drawing plane)
  "Make a line figure using two of the points."
  (let* ((p1 (random-point-in-rectangle (bounds drawing)))
         (p2 (random-point-in-rectangle (bounds drawing)))
         ;;FIXME: Should be a line.
         (fig (make-figure-from-points (vector p1 p2))))
    (vector-push-extend fig (figures plane))
    (draw-figure fig (pen-params plane))
    fig))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-point-figure (drawing plane)
  "Make a point figure."
  (let* ((p (random-point-in-rectangle (bounds drawing)))
         ;;FIXME: should be a point.
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
