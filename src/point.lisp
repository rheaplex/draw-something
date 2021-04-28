;;  point.lisp - A 2D point.
;;  Copyright (C) 2006, 2016 Rhea Myers
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

(in-package "DRAW-SOMETHING")

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

(defun point= (left right)
  "The eql method for points"
  (and (= (x left) (x right))
       (= (y left) (y right))))

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
    (make-instance '<point>
                   :x (+ x x-dist)
                   :y (+ y y-dist))))

(defun translate-point (p by-x by-y)
  "Make a translated copy of the point."
  (make-instance '<point>
                 :x (+ (x p) by-x)
                 :y (+ (y p) by-y)))

(defun co-ordinates-at-angle-around-point-co-ordinates (a b r theta)
  "Get the point on the circumference of the circle at theta."
  (values (+ a (* r (cos theta)))
            (+ b (* r (sin theta)))))

(defun co-ordinates-at-angle (x y radius theta)
  "Get the point on the circumference of the arc/circle at theta.
   Doesn't check limits of arc."
  (co-ordinates-at-angle-around-point-co-ordinates x y radius theta))

;; Angle between two points, in radians

(defun angle-between-two-points-co-ordinates (x0 y0 x1 y1)
  (let ((angle (- (atan y1 x1)
                  (atan y0 x0))))
    (if (< angle 0)
        (+ angle +radian+)
        angle)))

(defun angle-between-two-points (p1 p2)
  "Calculate the angle of the second point around the first."
  (angle-between-two-points-co-ordinates (x p1) (y p1) (x p2) (y p2)))

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
    (dovector (pp points)
      (let ((ppd (distance p pp)))
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
  (let ((candidates (make-vector 10)))
    (loop for candidate across points
          when (all-points-leftp p candidate points)
          do (vector-push-extend candidate candidates))
    (furthest-point p candidates)))

(defun convex-hull (the-points)
  "Get the convex hull of an array of points."
  (let* ((first-point (highest-leftmost-point-in-list the-points))
         (current-point first-point)
         (next-point nil)
         (hull (make-vector 10)))
    (vector-push-extend first-point hull)
    (loop until (and (not (eq next-point nil))
                     (eq next-point first-point))
          do (setf next-point
                   (point-with-all-left current-point the-points))
          (vector-push-extend next-point hull)
          (setf current-point next-point))
    (make-instance '<polyline> :points hull)))
