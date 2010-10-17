;; tests.lisp - Tests for draw-something.
;; Copyright (C) 2010  Rhea Myers rhea@myers.studio
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

;; Since many of the functions called use random numbers, coverage isn't perfect

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (load "lisp-unit")
  (load "load")
  
  (defpackage draw-something-tests
    (:documentation
     "Tests for the draw-something package.")
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

(defun layers-forms (drawing)
  (loop for plane across (draw-something::planes drawing)
     do (loop for figure across (draw-something::figures plane)
	   do (loop for form across (draw-something::forms figure)
		 collect form))))

(defun max-pen-distance ()
  (apply #'max (map 'list #'draw-something::pen-distance 
		    draw-something::plane-pen-parameters)))

(defun max-pen-tolerance ()
  (apply #'max (map 'list #'draw-something::pen-distance 
		    draw-something::plane-pen-parameters)))

(defun drawing-overflow-bounds (drawing)
  "Outlines go over the edge of the drawing but shouldn't go outside of this"
  (draw-something::inset-rectangle (draw-something::bounds drawing)
				   (- (+ (max-pen-distance) 
					( max-pen-tolerance)))))

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

(define-test polyline
    (let ((polylines
	   (map-into (make-array 100) 
		     (lambda ()
		       (draw-something::make-random-polyline-in-rectangle 
			+rect+ 10)))))
      ;; Every created polyline's points fall within its source bounds
      (assert-true
       (every (lambda (poly) 
		(every (lambda (p)
			 (draw-something::contains +rect+ p))
		       (draw-something::points poly)))
	      polylines))
      ;; Every polyline's bounds fits into the source bounds
      #|(assert-true (every 
      (lambda (poly)
      (draw-something::contains +rect+
      (draw-something::bounds poly)))
      polylines))|#
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +red+
  (make-instance 'draw-something::colour 
		 :hue 0.0 
		 :saturation 1.0
		 :brightness 1.0))

(define-test colour
    ;; HSV colours convert to RGB correctly
    (multiple-value-bind (r g b ) (draw-something::hsb-to-rgb +red+)
      (assert-equal (list r g b) '(1.0 0.0 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test drawing
    (let* ((drawing (draw-something::make-drawing))
	   (drawing-bounds (draw-something::bounds drawing)))
      ;; Drawing origin is 0,0
      (assert-true (= 0
		      (draw-something::x drawing-bounds)
		      (draw-something::y drawing-bounds)))
      ;; Drawing width is withing specified limits
      (assert-true (<= draw-something::+min-drawing-size+
		       (draw-something::width drawing-bounds)
		       draw-something::+max-drawing-size+))
      ;; Drawing height is withing specified limits
      (assert-true (<= draw-something::+min-drawing-size+
		       (draw-something::height drawing-bounds)
		       draw-something::+max-drawing-size+))
))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test composition
    (let* ((drawing (draw-something::make-drawing))
	   (composition-points
	    (draw-something::make-composition-points drawing 10000))
	   (bad-skeletons
	    (vector (vector (make-instance 'draw-something::point 
					   :x 1000000 :y 0))))
	   (polygon-figures
	    (draw-something::make-polygon-figures composition-points 10 5 20)))
      ;; All composition points are within the drawing bounds
      (assert-points-in-rect (draw-something::bounds drawing)
			     composition-points)
      ;; All polyline figure points are within the drawing bounds
      (assert-pointses-in-rect-true (draw-something::bounds drawing)
				    (figure-skeletons polygon-figures))
      ;; These tests will catch skeletons with points outside the drawing bounds
      (assert-pointses-in-rect-false (draw-something::bounds drawing)
				     bad-skeletons)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test skeletons
    (let* ((drawing (draw-something::draw-something))
	   (forms (layers-forms drawing))
	   (layers-forms-skeletons
	    (map 'vector #'draw-something::skeleton forms)))
      ;; All layer figure forms' skeleton points are within the drawing's bounds
      (assert-pointses-in-rect-true (draw-something::bounds drawing)
				    layers-forms-skeletons)))

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

(define-test figures
    (let* ((drawing (draw-something::draw-something))
	   (forms (layers-forms drawing))
	   (layers-forms-outlines
	    (map 'vector #'draw-something::outline forms)))
      ;; All layer figure forms' outlines are within pen parameter bounds
      (assert-pointses-in-rect-true (drawing-overflowbounds drawing)
				    layers-forms-outlines)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colouring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test colouring
    (let* ((drawing (draw-something::draw-something))
	   (forms (layers-forms drawing)))
      ;; Every form has been coloured
      (assert-true (every (lambda (form) (draw-something::fill-colour form))
			  forms))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-test svg
    (assert-equal (draw-something::svg-rgb +red+) "#FF0000"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completed drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ideally we'd parse the xml and make sure it accurately reflects the drawing
;; But we don't want to have to have an xml parser as a dependency

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main flow of control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
