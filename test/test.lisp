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
;; Initialize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf draw-something:*print-advisories* nil)

(plan 56)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun all-points-in-rect (rect points)
  "Every point in points is within the bounds of rect"
  (every (lambda (p)
           (draw-something::contains rect p))
         points))

(defun all-pointses-in-rect (rect point-sets)
  "Assert test on every point in every set of points being within rect"
  (every (lambda (points)
           (every (lambda (p)
                    (draw-something::contains rect p))
                  points))
         point-sets))

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
                    draw-something::*plane-pen-parameters*)))

(defun max-pen-tolerance ()
  (apply #'max (map 'list #'draw-something::pen-distance
                    draw-something::*plane-pen-parameters*)))

(defun drawing-overflow-bounds (drawing)
  "Outlines go over the edge of the drawing but shouldn't go outside of this"
  (draw-something::inset-rectangle (draw-something::bounds drawing)
                                   (- (+ (max-pen-distance)
                                         ( max-pen-tolerance)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(is (draw-something::turn-positive-p 1 2) t)
(is (draw-something::turn-positive-p 2 1) nil)
(is (draw-something::turn-positive-p 1 6) nil)
(is (draw-something::turn-positive-p 6 1) t)
(is (draw-something::turn-positive-p 0.01 6.2) nil)
(is (draw-something::turn-positive-p 6.2 0.01) t)

;; Check different angle scenarios
;; zero and 2pi are the same, so zero angle between the,
(is (draw-something::shortest-angle-difference 0 draw-something::+radian+)
    0.0d0)
(is (draw-something::shortest-angle-difference draw-something::+radian+ 0)
    0.0d0)
;; Positive differences are positive
(is (draw-something::shortest-angle-difference 0 1) 1.0d0)
(is (draw-something::shortest-angle-difference 1 4) 3.0d0)
;; Negative differences are negative
(is (draw-something::shortest-angle-difference 1 0) -1.0d0)
(is (draw-something::shortest-angle-difference 4 1) -3.0d0)
;; Angles that would be greater than pi are changed to be less than
(is (draw-something::shortest-angle-difference 1 6) -1.2831853071795862d0)
(is (draw-something::shortest-angle-difference 1 5) -2.2831853071795862d0)
;; Angles that cross zero/2pi are less than pi
(is (draw-something::shortest-angle-difference 6 1)
    1.2831853071795862d0)
(is (draw-something::shortest-angle-difference (- draw-something::+radian+ 0.1)
                                               3)
    3.100000001490116d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +from+ (make-instance 'draw-something::<point> :x 100 :y 100))
(defparameter +mid+ (make-instance 'draw-something::<point> :x 550 :y 550))
(defparameter +to+ (make-instance 'draw-something::<point> :x 1000 :y 1000))

;; Point equality
(ok (draw-something::point= +from+ +from+))

;; Point inequality
(ok (not (draw-something::point= +from+ +to+)))

;; Point angles counterclockwise from x-axis
;; 0 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 1 0)
    0.0)
;; 45 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 1 1)
    0.7853982)
;; 90 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 0 1)
    1.5707964)
;; 135 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 -1 1)
    2.3561945)
;; 180 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 -1 0)
    3.1415927)
;; 225 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 -1 -1)
    3.926990811024801d0)
;; 270 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 0 -1)
    4.7123889366733d0)
;; 315 degrees
(is (draw-something::angle-between-two-points-co-ordinates 0 0 1 -1)
    5.497787121926443d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +line+ (make-instance 'draw-something::<line>
                                    :from +from+
                                    :to +to+))

;; line-at-t for 0 is line start
(ok (draw-something::point= (draw-something::line-at-t +line+
                                                       0.0)
                            +from+))
;; line-at-t for 0.5 is line midpoint
(ok (draw-something::point= (draw-something::line-at-t +line+
                                                       0.5)
                            +mid+))
;; line-at-t for 1 is line end
(ok (draw-something::point= (draw-something::line-at-t +line+
                                                       1.0)
                            +to+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +rect+ (make-instance 'draw-something::<rectangle>
                                    :x 0 :y 0 :width 500 :height 1000))

(defparameter +intersecting-rects+
  (list (make-instance 'draw-something::<rectangle>
                       :x -100 :y -100 :width 1000 :height 1000)
        (make-instance 'draw-something::<rectangle>
                       :x 100 :y 100 :width 1000 :height 1000)))

(defparameter +non-intersecting-rects+
  (list (make-instance 'draw-something::<rectangle>
                       :x -10000 :y -10000 :width 50 :height 10)
        (make-instance 'draw-something::<rectangle>
                       :x 10000 :y 10000 :width 50 :height 10)))

;; Rectangle intersection
(ok (draw-something::intersects-any +rect+
                                    +intersecting-rects+))
(ok (not (draw-something::intersects-any +rect+
                                         +non-intersecting-rects+)))
;; Rectangle non-intersection
(ok (draw-something::intersects-none +rect+
                                     +non-intersecting-rects+))
(ok (not (draw-something::intersects-none +rect+
                                          +intersecting-rects+)))

;; Random points inside a rectangle fall within that rectangle
(ok (all-points-in-rect +rect+
                        (draw-something::random-points-in-rectangle +rect+
                                                                    10000)))
;; Random points on rectangle bounds fall on that rectangle's bounds
(ok (all-points-in-rect +rect+
                        (draw-something::random-points-on-rectangle +rect+
                                                                    10000)))
;; Random points at rectangle corners fall on that rectangle's bounds
(ok (all-points-in-rect +rect+
                        (draw-something::random-points-at-rectangle-corners
                         +rect+ 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((polylines
       (map-into (make-array 100)
                 (lambda ()
                   (draw-something::make-random-polyline-in-rectangle
                    +rect+ 10))))
      (polyrecta
       (draw-something::as-polyline (make-instance
                                     'draw-something::<rectangle>
                                     :x 0 :y 0 :width 100
                                     :height 100)))
      (polyrectb
       (draw-something::as-polyline (make-instance
                                     'draw-something::<rectangle>
                                     :x 50 :y 50 :width 100
                                     :height 100)))
      (polyrectc
       (draw-something::as-polyline (make-instance
                                     'draw-something::<rectangle>
                                     :x 500 :y 500 :width 100
                                     :height 100)))
      (polyrectd
       (draw-something::as-polyline (make-instance
                                     'draw-something::<rectangle>
                                     :x 10 :y 10 :width 10
                                     :height 10))))
  ;; Every created polyline's points fall within its source bounds
  (ok
   (every (lambda (poly)
            (every (lambda (p)
                     (draw-something::contains +rect+ p))
                   (draw-something::points poly)))
          polylines))
  ;; Overlapping polyrects intersect
  (ok (draw-something::intersects polyrecta polyrectb))
  (ok (draw-something::intersects polyrectb polyrecta))
  ;; Contained polyrects intersect
  (ok (draw-something::intersects polyrecta polyrectd))
  (ok (draw-something::intersects polyrectd polyrecta))
  ;; Non-overlapping polyrects don't intersect
  (ok (not (draw-something::intersects polyrecta polyrectc)))
  (ok (not (draw-something::intersects polyrectc polyrecta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +red+
  (make-instance 'draw-something::<colour>
                 :hue 0.0
                 :saturation 1.0
                 :brightness 1.0))

;; HSV colours convert to RGB correctly
(multiple-value-bind (r g b ) (draw-something::hsb-to-rgb +red+)
  (ok (equal (list r g b) (list 1.0 0.0 0.0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((drawing (draw-something::make-drawing))
       (drawing-bounds (draw-something::bounds drawing)))
  ;; Drawing origin is 0,0
  (ok (= 0
         (draw-something::x drawing-bounds)
         (draw-something::y drawing-bounds)))
  ;; Drawing width is withing specified limits
  (ok (<= draw-something::+min-drawing-size+
          (draw-something::width drawing-bounds)
          draw-something::+max-drawing-size+))
  ;; Drawing height is withing specified limits
  (ok (<= draw-something::+min-drawing-size+
          (draw-something::height drawing-bounds)
          draw-something::+max-drawing-size+)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(let* ((drawing (draw-something::make-drawing))
       (composition-points
        (draw-something::make-composition-points drawing 10000))
       (bad-skeletons
        (vector (vector (make-instance 'draw-something::<point>
                                       :x 1000000 :y 0))))
       (polygon-figures
        (draw-something::make-polygon-figures composition-points
                                              10 5 20)))
  ;; All composition points are within the drawing bounds
  (ok (all-points-in-rect (draw-something::bounds drawing)
                          composition-points))
  ;; All polyline figure points are within the drawing bounds
  (ok (all-pointses-in-rect (draw-something::bounds drawing)
                            (figure-skeletons polygon-figures)))
  ;; These tests will catch skeletons with points outside drawing rect
  (ok (not (all-pointses-in-rect (draw-something::bounds drawing)
                                 bad-skeletons))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Planes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +figure-bounds+
  (make-instance 'draw-something::<rectangle> :x 0 :y 0 :width 200 :height 400))

(defparameter +search-size-good+
  (make-instance 'draw-something::<rectangle> :x 0 :y 0 :width 20 :height 40))

(defparameter +search-size-bad+
  (make-instance 'draw-something::<rectangle> :x 0 :y 0 :width 300 :height 300))

(let* ((the-form
        (make-instance 'draw-something::<form>
                       :bounds +figure-bounds+))
       (the-figure
        (make-instance 'draw-something::<figure>
                       :bounds +figure-bounds+
                       :forms (vector the-form)))
       (the-plane
        (make-instance 'draw-something::<plane>
                       :figures (vector the-figure)))
       (drawing
        (make-instance 'draw-something::<drawing>
                       :bounds (make-instance
                                'draw-something::<rectangle>
                                :x 0 :y 0
                                :width 400 :height 400)
                       :planes (vector the-plane))))
  ;; It's possible to find space for a new form that fits on the layer
  (ok (not (null (draw-something::find-space-on-plane
                  drawing the-plane +search-size-good+))))
  ;; It is not possible to find space for a new form that doesn't fit
  (ok (not (draw-something::find-space-on-plane
            drawing the-plane +search-size-bad+))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((drawing (draw-something::generate-drawing))
       (forms (layers-forms drawing))
       (layers-forms-skeletons
        (map 'vector #'draw-something::skeleton forms)))
  ;; All layer figure forms' skeleton points are within drawing bounds
  (ok (all-pointses-in-rect (draw-something::bounds drawing)
                            layers-forms-skeletons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Figures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((drawing (draw-something::generate-drawing))
       (forms (layers-forms drawing))
       (layers-forms-outlines
        (map 'vector #'draw-something::outline forms)))
  ;; All layer figure forms' outlines are within pen parameter bounds
  (ok (all-pointses-in-rect (drawing-overflow-bounds drawing)
                            layers-forms-outlines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colouring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((drawing (draw-something::generate-drawing))
       (forms (layers-forms drawing)))
  ;; Every form has been coloured
  (ok (every (lambda (form)
               (draw-something::fill-colour form))
             forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SVG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ok (string= (draw-something::svg-rgb +red+) "#FF0000"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completed drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ideally we'd parse the xml and make sure it accurately reflects the drawing
;; But we don't want to have to have an xml parser as a dependency

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finalize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(finalize)
