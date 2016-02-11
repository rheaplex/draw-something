;; tests.lisp - Tests for draw-something.
;; Copyright (C) 2010, 2016 Rob Myers rob@robmyers.org
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

(in-package draw-something/test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:def-suite draw-something-test-suite
    :description "Tests for draw-something.")

(5am:in-suite draw-something-test-suite)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro assert-points-in-rect (rect points)
  "Assert that every point in points is within the bounds of rect"
  (let ((p (gensym)))
    `(5am:is-true (every (lambda (,p)
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
  `(assert-pointses-in-rect is-true ,rect ,point-sets))

(defmacro assert-pointses-in-rect-false (rect point-sets)
  "Assert that not every point in every set of points is within rect"
  `(assert-pointses-in-rect is-false ,rect ,point-sets))

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

(test point
      ;; Point equality
      (5am:is-true (draw-something::point= +from+ +from+))
      ;; Point inequality
      (5am:is-false (draw-something::point= +from+ +to+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +line+ (make-instance 'draw-something::line
                                    :from +from+
                                    :to +to+))

(test line
      ;; line-at-t for 0 is line start 
      (5am:is-true (draw-something::point= (draw-something::line-at-t +line+
                                                                      0.0)
                                           +from+))
      ;; line-at-t for 0.5 is line midpoint
      (5am:is-true (draw-something::point= (draw-something::line-at-t +line+
                                                                      0.5)
                                           +mid+))
      ;; line-at-t for 1 is line end
      (5am:is-true (draw-something::point= (draw-something::line-at-t +line+
                                                                      1.0)
                                           +to+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +rect+ (make-instance 'draw-something::rectangle 
                                    :x 0 :y 0 :width 500 :height 1000))

(defparameter +intersecting-rects+ 
  (list (make-instance 'draw-something::rectangle 
                       :x -100 :y -100 :width 1000 :height 1000)
        (make-instance 'draw-something::rectangle 
                       :x 100 :y 100 :width 1000 :height 1000)))

(defparameter +non-intersecting-rects+ 
  (list (make-instance 'draw-something::rectangle 
                       :x -10000 :y -10000 :width 50 :height 10)
        (make-instance 'draw-something::rectangle 
                       :x 10000 :y 10000 :width 50 :height 10)))

(test rectangle
      ;; Rectangle intersection
      (5am:is-true (draw-something::intersects-any +rect+
                                                   +intersecting-rects+))
      (5am:is-false (draw-something::intersects-any +rect+
                                                    +non-intersecting-rects+))
      ;; Rectangle non-intersection
      (5am:is-true (draw-something::intersects-none +rect+
                                                    +non-intersecting-rects+))
      (5am:is-false (draw-something::intersects-none +rect+
                                                     +intersecting-rects+))
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
                              +rect+ 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test polyline
      (let ((polylines
             (map-into (make-array 100) 
                       (lambda ()
                         (draw-something::make-random-polyline-in-rectangle 
                          +rect+ 10))))
            (polyrecta
             (draw-something::as-polyline (make-instance
                                           'draw-something::rectangle
                                           :x 0 :y 0 :width 100
                                           :height 100)))
            (polyrectb
             (draw-something::as-polyline (make-instance
                                           'draw-something::rectangle
                                           :x 50 :y 50 :width 100
                                           :height 100)))
            (polyrectc
             (draw-something::as-polyline (make-instance
                                           'draw-something::rectangle
                                           :x 500 :y 500 :width 100
                                           :height 100)))
            (polyrectd
             (draw-something::as-polyline (make-instance
                                           'draw-something::rectangle
                                           :x 10 :y 10 :width 10
                                           :height 10))))
        ;; Every created polyline's points fall within its source bounds
        (5am:is-true
         (every (lambda (poly) 
                  (every (lambda (p)
                           (draw-something::contains +rect+ p))
                         (draw-something::points poly)))
                polylines))
        ;; Overlapping polyrects intersect
        (5am:is-true (draw-something::intersects polyrecta polyrectb))
        (5am:is-true (draw-something::intersects polyrectb polyrecta))
        ;; Contained polyrects intersect
        (5am:is-true (draw-something::intersects polyrecta polyrectd))
        (5am:is-true (draw-something::intersects polyrectd polyrecta))
        ;; Non-overlapping polyrects don't intersect
        (5am:is-false (draw-something::intersects polyrecta polyrectc))
        (5am:is-false (draw-something::intersects polyrectc polyrecta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +red+
  (make-instance 'draw-something::colour 
                 :hue 0.0 
                 :saturation 1.0
                 :brightness 1.0))

(5am:test colour
          ;; HSV colours convert to RGB correctly
          (multiple-value-bind (r g b ) (draw-something::hsb-to-rgb +red+)
            (5am:is (equal (list r g b) '(1.0 0.0 0.0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test drawing
          (let* ((drawing (draw-something::make-drawing))
                 (drawing-bounds (draw-something::bounds drawing)))
            ;; Drawing origin is 0,0
            (5am:is-true (= 0
                            (draw-something::x drawing-bounds)
                            (draw-something::y drawing-bounds)))
            ;; Drawing width is withing specified limits
            (5am:is-true (<= draw-something::+min-drawing-size+
                             (draw-something::width drawing-bounds)
                             draw-something::+max-drawing-size+))
            ;; Drawing height is withing specified limits
            (5am:is-true (<= draw-something::+min-drawing-size+
                             (draw-something::height drawing-bounds)
                             draw-something::+max-drawing-size+))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test composition
          (let* ((drawing (draw-something::make-drawing))
                 (composition-points
                  (draw-something::make-composition-points drawing 10000))
                 (bad-skeletons
                  (vector (vector (make-instance 'draw-something::point 
                                                 :x 1000000 :y 0))))
                 (polygon-figures
                  (draw-something::make-polygon-figures composition-points
                                                        10 5 20)))
            ;; All composition points are within the drawing bounds
            (assert-points-in-rect (draw-something::bounds drawing)
                                   composition-points)
            ;; All polyline figure points are within the drawing bounds
            (assert-pointses-in-rect-true (draw-something::bounds drawing)
                                          (figure-skeletons polygon-figures))
            ;; These tests will catch skeletons with points outside drawing rect
            (assert-pointses-in-rect-false (draw-something::bounds drawing)
                                           bad-skeletons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +figure-bounds+  
  (make-instance 'draw-something::rectangle :x 0 :y 0 :width 200 :height 400))

(defparameter +search-size-good+  
  (make-instance 'draw-something::rectangle :x 0 :y 0 :width 20 :height 40))

(defparameter +search-size-bad+  
  (make-instance 'draw-something::rectangle :x 0 :y 0 :width 300 :height 300))

(5am:test planes
          (let* ((the-form 
                  (make-instance 'draw-something::form
                                 :bounds +figure-bounds+))
                 (the-figure 
                  (make-instance 'draw-something::figure
                                 :bounds +figure-bounds+
                                 :forms (vector the-form)))
                 (the-plane 
                  (make-instance 'draw-something::plane
                                 :figures (vector the-figure)))
                 (drawing 
                  (make-instance 'draw-something::drawing 
                                 :bounds (make-instance
                                          'draw-something::rectangle 
                                          :x 0 :y 0 
                                          :width 400 :height 400)
                                 :planes (vector the-plane))))
            ;; It's possible to find space for a new form that fits on the layer
            (5am:is-true (draw-something::find-space-on-plane
                          drawing the-plane +search-size-good+))
            ;; It is not possible to find space for a new form that doesn't fit
            (5am:is-false (draw-something::find-space-on-plane
                           drawing the-plane +search-size-bad+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test skeletons
          (let* ((drawing (draw-something::draw-something))
                 (forms (layers-forms drawing))
                 (layers-forms-skeletons
                  (map 'vector #'draw-something::skeleton forms)))
            ;; All layer figure forms' skeleton points are within drawing bounds
            (assert-pointses-in-rect-true (draw-something::bounds drawing)
                                          layers-forms-skeletons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test figures
          (let* ((drawing (draw-something::draw-something))
                 (forms (layers-forms drawing))
                 (layers-forms-outlines
                  (map 'vector #'draw-something::outline forms)))
            ;; All layer figure forms' outlines are within pen parameter bounds
            (assert-pointses-in-rect-true (drawing-overflow-bounds drawing)
                                          layers-forms-outlines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colouring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test colouring
          (let* ((drawing (draw-something::draw-something))
                 (forms (layers-forms drawing)))
            ;; Every form has been coloured
            (5am:is-true (every (lambda (form)
                                  (draw-something::fill-colour form))
                                forms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(5am:test svg
          (5am:is (string= (draw-something::svg-rgb +red+) "#FF0000")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completed drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ideally we'd parse the xml and make sure it accurately reflects the drawing
;; But we don't want to have to have an xml parser as a dependency
