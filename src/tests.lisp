;;  tests.lisp - Tests for draw-something.
;;  Copyright (C) 2010  Rhea Myers rhea@myers.studio
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (load "lisp-unit")
  (require 'draw-something)
  
  (defpackage draw-something-tests
    (:use :common-lisp
	  :lisp-unit
	  :draw-something)))

(in-package draw-something-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-points-in-rect (rect points)
  "Assert that every point in points is within the bounds of rect"
  (let ((p (gensym)))
    `(assert-true (every (lambda (,p)
			   (draw-something::contains ,rect ,p))
			 ,points))))

(defmacro assert-pointses-in-rect (test rect point-sets)
  "Assert test on every point in every set of points being within rect"
  (let ((p (gensym))
	(points (gensym)))
    `(,test (every (lambda (,points)
			   (every (lambda (,p) 
				    (draw-something::contains ,rect ,p))
				  ,points))
			 ,point-sets))))

(defmacro assert-pointses-in-rect-true (rect point-sets)
  "Assert that every point in every set of points is within rect"
  `(assert-pointses-in-rect assert-true ,rect ,point-sets))

(defmacro assert-pointses-in-rect-false (rect point-sets)
  "Assert that not every point in every set of points is within rect"
  `(assert-pointses-in-rect assert-false ,rect ,point-sets))

(defun figure-skeletons (figures)
  "Get the skeleton point vectors for each form of each figure"
  (loop for fig across figures
     do (loop for form across (draw-something::forms fig)
	   collect (draw-something::skeleton form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +from+ (make-instance 'draw-something::point :x 100 :y 100))
(defparameter +mid+ (make-instance 'draw-something::point :x 550 :y 550))
(defparameter +to+ (make-instance 'draw-something::point :x 1000 :y 1000))

(define-test point
    ;; Point equality
    (assert-true (draw-something::point= +from+ +from+))
  ;; Point inequality
    (assert-false (draw-something::point= +from+ +to+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +line+ (make-instance 'draw-something::line
				    :from +from+
				    :to +to+))

(define-test line
    ;; line-at-t for 0 is line start 
    (assert-true (draw-something::point= (draw-something::line-at-t +line+ 0.0)
					 +from+))
  ;; line-at-t for 0.5 is line midpoint
    (assert-true (draw-something::point= (draw-something::line-at-t +line+ 0.5)
					 +mid+))
  ;; line-at-t for 1 is line end
  (assert-true (draw-something::point= (draw-something::line-at-t +line+ 1.0)
					 +to+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +rect+ (make-instance 'draw-something::rectangle 
				    :x 0 :y 0 :width 500 :height 1000))

(define-test rectangle
    ;; Random points inside a rectangle fall within that rectangle
    (assert-points-in-rect +rect+ 
			   (draw-something::random-points-in-rectangle +rect+
								       10000))
    ;; Random points on rectangle bounds fall on that rectangle's bounds
    (assert-points-in-rect +rect+
			   (draw-something::random-points-on-rectangle +rect+
								       10000))
    ;; Random points at rectangle corners fall on that rectangle's bounds
    (assert-points-in-rect +rect+	
			   (draw-something::random-points-at-rectangle-corners
			    +rect+ 4))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +polylines+
  (map-into (make-array 100) 
	    (lambda ()
	      (draw-something::make-random-polyline-in-rectangle +rect+ 10))))

(define-test polyline
    ;; Every created polyline's points fall within its source bounds
    (assert-true
     (every (lambda (poly) 
	      (every (lambda (p)
		       (draw-something::contains +rect+ p))
		     (draw-something::points poly)))
	    +polylines+))
  ;; Every polyline's bounds fits into the source bounds
  #|(assert-true (every 
		(lambda (poly)
		  (draw-something::contains +rect+
					    (draw-something::bounds poly)))
		+polylines+))|#
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +drawing+ (draw-something::make-drawing))
(defparameter +drawing-bounds+ (draw-something::bounds +drawing+))

(define-test drawing
    ;; Drawing origin is 0,0
    (assert-true (= 0
		    (draw-something::x +drawing-bounds+)
		    (draw-something::y +drawing-bounds+)))
    ;; Drawing width is withing specified limits
    (assert-true (<= draw-something::+min-drawing-size+
		     (draw-something::width +drawing-bounds+)
		     draw-something::+max-drawing-size+))
    ;; Drawing height is withing specified limits
    (assert-true (<= draw-something::+min-drawing-size+
		     (draw-something::height +drawing-bounds+)
		     draw-something::+max-drawing-size+))
) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +composition-points+ 
  (draw-something::make-composition-points +drawing+ 10000))

(defparameter +bad-skeletons+ 
  (vector (vector (make-instance 'draw-something::point :x 1000000 :y 0))))

(defparameter +polygon-figures+
  (draw-something::make-polygon-figures +composition-points+ 10 5 20))

(define-test composition
    ;; All composition points are within the drawing bounds
    (assert-points-in-rect (draw-something::bounds +drawing+)
			   +composition-points+)
    ;; All polyline figure points are within the drawing bounds
    (assert-pointses-in-rect-true (draw-something::bounds +drawing+)
				  (figure-skeletons +polygon-figures+))
    ;; These tests will catch skeletons with points outside the drawing bounds
    (assert-pointses-in-rect-false (draw-something::bounds +drawing+)
				   +bad-skeletons+)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +planes+ 
  (draw-something::make-planes +drawing+
			       (draw-something::number-of-planes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(draw-something::make-planes-skeletons +drawing+)

(defparameter +layers-forms+
  (loop for plane across (draw-something::planes +drawing+)
     do (loop for figure across (draw-something::figures plane)
	   do (loop for form across (draw-something::forms figure)
		 collect form))))

(defparameter +layers-forms-skeletons+
  (map 'vector #'draw-something::skeleton +layers-forms+))

(define-test skeletons
    ;; All layer figure forms' skeleton points are within the drawing's bounds
    (assert-pointses-in-rect-true (draw-something::bounds +drawing+)
				  +layers-forms-skeletons+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +max-pen-distance+
  (apply #'max (map 'list #'draw-something::pen-distance 
		    draw-something::plane-pen-parameters)))

(defparameter +max-pen-tolerance+
  (apply #'max (map 'list #'draw-something::pen-distance 
		    draw-something::plane-pen-parameters)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(draw-something::draw-planes-figures +drawing+)

(defparameter +drawing-overflow-bounds+ 
  (draw-something::inset-rectangle +drawing-bounds+
				   (- (+ +max-pen-distance+ 
					 +max-pen-tolerance+)))
  "Outlines go over the edge of the drawing but shouldn't go outside of this")

(defparameter +layers-forms-outlines+
  (map 'vector #'draw-something::outline +layers-forms+))

(define-test figures
    ;; All layer figure forms' outlines are within pen parameter bounds
    (assert-pointses-in-rect-true (draw-something::bounds +drawing+)
				  +layers-forms-outlines+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colouring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(draw-something::colour-objects +drawing+ draw-something::all-object-symbols)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completed drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main flow of control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
