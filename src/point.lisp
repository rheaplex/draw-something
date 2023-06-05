;; point.lisp - A 2D point.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Point 2D
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <point> (<geometry>)
  ((x :accessor x
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The x co-ordinate of the point.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The y co-ordinate of the point."))
  (:documentation "A simple cartesian point on the picture plane (or page).
                   y goes up"))

(defun make-point (&key x y)
  "Constructor function."
  (make-instance '<point> :x x :y y))

(defun point= (left right)
  "The eql method for points"
  (and (= (x left) (x right))
       (= (y left) (y right))))

(defmethod print-object ((object <point>) stream)
  "Make a human readable string describing the point."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(X: ~,2f, Y: ~,2f)"
          (x object)
          (y object))))

(defun distance-co-ordinates (x1 y1 x2 y2)
  "The distance between two points."
  (sqrt (+ (expt (- x2 x1) 2)
           (expt (- y2 y1) 2))))

(defmethod distance ((left <point>) (right <point>))
  "The distance between two points."
  (distance-co-ordinates (x left) (y left) (x right) (y right)))

(defun distance-point-co-ordinates (point x2 y2)
  "The distance between two points."
  (distance-co-ordinates (x point) (y point) x2 y2))

(defun random-co-ordinates (x-range y-range)
  "Make random co-ordinates avoiding bell-curve point distribution."
  ;; Yay bignum!
  (floor (random-number (+ x-range y-range))
         x-range))

(defun random-point-in-bounds (x y width height)
  "Make a point placed randomly within the given bounds."
  (multiple-value-bind
        (x-dist y-dist) (random-co-ordinates width height)
    (make-point :x (+ x x-dist) :y (+ y y-dist))))

(defun translate-point (p by-x by-y)
  "Make a translated copy of the point."
  (make-point :x (+ (x p) by-x) :y (+ (y p) by-y)))

(defun co-ordinates-at-angle-around-point-co-ordinates (a b r theta)
  "Get the point on the circumference of the circle at theta."
  (values (+ a (* r (cos theta)))
            (+ b (* r (sin theta)))))

(defun co-ordinates-at-angle (x y radius theta)
  "Get the point on the circumference of the arc/circle at theta.
   Doesn't check limits of arc."
  (co-ordinates-at-angle-around-point-co-ordinates x y radius theta))

(defun angle-between-two-points-co-ordinates (x0 y0 x1 y1)
  "Angle ccw from 3 o'clock in radians."
  (let ((theta (atan (- y1 y0) (- x1 x0))))
    (if (>= theta 0)
        theta
        (+ +radian+ theta))))

(defun angle-between-two-points (p1 p2)
  "Calculate the angle of the second point around the first."
  (angle-between-two-points-co-ordinates (x p1) (y p1) (x p2) (y p2)))

(defun least-clockwise-point (p pts)
  (let ((max-angle 0)
        (least nil))
    (dolist (p2 pts)
      ;; Convert the angle to 12 o'clock == 0
      ;;FIXME:  Simplify
      (let ((angle (mod (- (angle-between-two-points p p2)
                           +half-pi+)
                        +radian+)))
        (when (> angle max-angle)
          (setf max-angle angle)
          (setf least p2))))
    least))

(defun highest-leftmost-of (p1 p2)
  "Compare and return the highest leftmost point."
  (if (or (> (y p1) (y p2))
          (and (= (y p1) (y p2))
               (< (x p1) (x p2))))
      p1
      p2))

(defun highest-leftmost-point-in-list (the-points)
  "The highest point, or highest and leftmost point (if several are highest)."
  (let ((highest (aref the-points 0)))
    (dotimes (i (length the-points))
      (let ((pt (aref the-points i)))
        (setf highest (highest-leftmost-of pt highest))))
    highest))

(defun furthest-point (p points &key ((:exclude exclude) '()))
  "Return the point that is furthest from p"
  (let ((candidate nil)
        (candidate-distance -1.0))
    (loop for pp across points
          do (let ((ppd (distance p pp)))
               (when (and (> ppd candidate-distance)
                          (not (member pp exclude)))
                 (setf candidate pp)
                 (setf candidate-distance ppd))))
    candidate))

(defun closest-point (p points &key ((:exclude exclude) '()))
  "Return the point that is closest to p."
  (let ((candidate nil)
        (candidate-distance 99999999.0))
    (dolist (pp points)
      (let ((ppd (distance p pp)))
        (when (and (< ppd candidate-distance)
                   (not (member pp exclude)))
          (setf candidate pp)
          (setf candidate-distance ppd))))
    candidate))

(defun points-closer-than (p points dist &key ((:exclude exclude) '()))
  "Return any points closer to p than dist."
  (let ((results '()))
    (dolist (pp points)
      (when (and (< (distance p pp) dist)
                 (not (member pp exclude)))
        (push pp results)))
    results))

(defun sort-points-by-distance (p points)
  "Return a copy of points sorted by distance from p."
  (sort (copy-seq points)
        (lambda (a b) (< (distance p a)
                         (distance p b)))))

(defun n-closest-points (p points num &key ((:exclude exclude) '()))
  "Return num points closest to p (or all if less than num)."
  (subseq (sort-points-by-distance p (set-difference points exclude))
          0
          (min num (length points))))

(defun point-line-side (p0 p2 p1)
  "Find out which side of an infinite line through p1 and p2 that p0 lies on.
   < 0 = left, > 0 = right, == 0 = exactly on."
 (- (* (- (x p1) (x p0)) (- (y p2) (y p0)))
    (* (- (x p2) (x p0)) (- (y p1) (y p0)))))

(defun all-points-leftp (p q the-points)
  "Are all points to the left of or colinear with pq?"
  (loop for pp across the-points
        when (> (point-line-side pp p q) 0)
        do (return nil)
        finally (return t)))

(defun point-with-all-left (p points)
  "Return the point q that all other points lie to the left of the line pq.
   In the case of colinear points, returns the furthest point."
  (let ((candidates (make-array 1 :adjustable t :fill-pointer 0)))
    (loop for candidate across points
          when (all-points-leftp p candidate points)
          do (vector-push-extend candidate candidates))
    (furthest-point p candidates)))

;; These should be in a "vector.lisp" file.

(defun normalize-vector (v)
  (let ((dist (sqrt (+ (* (x v) (x v)) (* (y v) (y v))))))
    (make-point :x (/ (x v) dist) :y (/ (y v) dist))))

(defun scale-vector (v scale)
  (make-point :x (* (x v) scale) :y (* (y v) scale)))

(defun add-vector-to-point (p v)
  (make-point :x (+ (x p) (x v)) :y (+ (y p) (y v))))
