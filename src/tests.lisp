
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
  (load "lisp-unit")

  (defpackage draw-something-tests
    (:use :common-lisp
	  :lisp-unit
	  :draw-something)))

(in-package draw-something-tests)


(defmacro assert-points-in-rect (rect points)
  (let ((p (gensym)))
    `(assert-true (every (lambda (,p)
			   (draw-something::contains ,rect ,p))
			 ,points))))


(defmacro assert-pointses-in-rect (test rect point-sets)
  (let ((p (gensym))
	(points (gensym)))
    `(,test (every (lambda (,points)
			   (every (lambda (,p) 
				    (draw-something::contains ,rect ,p))
				  ,points))
			 ,point-sets))))

(defmacro assert-pointses-in-rect-true (rect point-sets)
  `(assert-pointses-in-rect assert-true ,rect ,point-sets))

(defmacro assert-pointses-in-rect-false (rect point-sets)
  `(assert-pointses-in-rect assert-false ,rect ,point-sets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter +from+ (make-instance 'draw-something::point :x 100 :y 10))
(defparameter +to+ (make-instance 'draw-something::point :x 100 :y 1000))

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
					 (draw-something::from +line+)))
    ;; line-at-t for 1 is line end
    (assert-true (draw-something::point= (draw-something::line-at-t +line+ 1.0)
					 (draw-something::to +line+))))


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
    (assert-true (every (lambda (p) (draw-something::contains +rect+ p))
			(draw-something::random-points-on-rectangle +rect+
								    10000)))
    ;; Random points at rectangle corners fall on that rectangle's bounds
    (assert-true (every (lambda (p) (draw-something::contains +rect+ p))
			(draw-something::random-points-at-rectangle-corners
			 +rect+ 4)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun figure-skeletons (figures)
  "Get the skeleton point vectors for each form of each figure"
    (loop for fig across figures
	 do (loop for form across (draw-something::forms fig)
		 collect (draw-something::skeleton form))))

(defparameter +composition-points+ 
  (draw-something::make-composition-points +drawing+ 10000))

(defparameter +bad-skeletons+ 
  (vector (vector (make-instance 'draw-something::point :x 1000000 :y 0))))

(defparameter +polygon-figures+
  (draw-something::make-polygon-figures +composition-points+ 1000 5 20))

(define-test composition
    ;; All composition points are within the drawing bounds
    (assert-points-in-rect (draw-something::bounds +drawing+)
			   +composition-points+)
    ;; All polyline figure points are within the drawing bounds
  (assert-pointses-in-rect-true (draw-something::bounds +drawing+)
				(polygon-skeletons +polygon-figures+))
    (assert-pointses-in-rect-false (draw-something::bounds +drawing+)
				   +bad-skeletons+)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main flow of control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests)
