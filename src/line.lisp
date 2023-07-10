;; line.lisp - A 2D line Segment, and utilities on points and lines.
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
;; Line segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <line> (<geometry>)
  ((from :accessor from
     :type point
     :initarg :from
     :documentation "The start of the line.")
   (to :accessor to
       :type point
       :initarg :to
       :documentation "The end of the line."))
   (:documentation "A simple line (segment) between two points."))

(defun make-line (&key from to)
  "Constructor function."
  (make-instance '<line> :from from :to to))

(defmethod bounds ((line <line>))
  (let* ((xmin (min (x (from line)) (x (to line))))
         (ymin (min (y (from line)) (y (to line))))
         (xmax (max (x (from line)) (x (to line))))
         (ymax (max (y (from line)) (y (to line)))))
    (make-rectangle :x xmin
                    :y ymin
                    :width (- xmax xmin)
                    :height (- ymax ymin))))

;;        nearest_point_on_line
;;        From "Crashing Into the New Year",
;;        Jeff Lander, GD Magazine, Jan 1999.
;;        From point t to line A = p1->p2, B is t -> p1, C is t ->p2,
;;        n is nearest point on A to t
;;                 (p2 - p1) * (B o A)
;;        n = p1 + -------------------
;;                  (B o A) + (C o A)
;;        [o is the dot product sign]
;;        Note: Cull out-of-range points on the bounding circle of the
;;        line for testing groups of lines to find closest.
;; This needs decomposing into smaller, more manageable and meaningful units

(defun nearest-point-on-line-coordinates (xp yp xla yla xlb ylb)
  "Get the nearest point on a line"
  ;; Optimised to avoid point accessors
  (let ((dot-ta (+ (* (- xp xla) (- xlb xla))
                   (* (- yp yla) (- ylb yla)))))
    ;;(format t "~F%~%" dot-ta)
    (if (<= dot-ta 0.0)
        (make-instance '<point> :x xla :y yla)
        ;; else
        (let ((dot-tb (+ (* (- xp xlb) (- xla xlb))
                         (* (- yp ylb) (- yla ylb)))))
          ;;(format t "~F%~%" dot-tb)
          (if (<= dot-tb 0.0)
              (make-point :x xlb :y ylb)
              ;; else
              (make-point :x (+ xla
                                (/ (* (- xlb xla) dot-ta)
                                   (+ dot-ta dot-tb)))
                          :y (+ yla
                                (/ (* (- ylb yla) dot-ta)
                                   (+ dot-ta dot-tb)))))))))

(defun nearest-point-on-line-points (p la lb)
  (nearest-point-on-line-coordinates (x p) (y p) (x la) (y la) (x lb) (y lb)))

(defun nearest-point-on-line (p l) ;;la lb)
  (nearest-point-on-line-points p (from l) (to l)))

(defmethod distance ((p <point>) (l <line>))
  "The distance between a point and a line."
  (distance p (nearest-point-on-line p l)))

(defun distance-point-line (p from to)
  "The distance between a point and a line."
  (distance p (nearest-point-on-line-points p from to)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line-line intersection
;; http://astronomy.swin.edu.au/~pbourke/geometry/lineline2d/
;; Returns the time where the second line intersects the first line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lines-intersects-co-ordinates (p1x p1y p2x p2y ;; First line
                                      p3x p3y p4x p4y);; Second line
  "Find whether the two lines, expressed as 8 co-ordinates, intersect."
  (let ((denominator (- (* (- p4y p3y)
               (- p2x p1x))
            (* (- p4x p3x)
               (- p2y p1y)))))
    (if (= denominator 0.0)
    nil ;; Parallel lines
    (let ((ua (/ (- (* (- p4x p3x)
               (- p1y p3y))
            (* (- p4y p3y)
               (- p1x p3x)))
             denominator))
          (ub (/ (- (* (- p2x p1x)
               (- p1y p3y))
            (* (- p2y p1y)
               (- p1x p3x)))
             denominator)))
      (if (and (>= ua 0.0)
           (<= ua 1.0)
           (>= ub 0.0)
           (<= ub 1.0)) ;; Intersection (or not)
          ua
          nil)))))

(defun lines-intersects-points (l1p1 l1p2 l2p1 l2p2)
  "Find whether the two lines, expressed as 4 points intersect."
  (lines-intersects-co-ordinates (x l1p1) (y l1p1) (x l1p2) (y l1p2)
                                 (x l2p1) (y l2p1) (x l2p2) (y l2p2)))


(defun line-intersects-line-points (l1 l2p1 l2p2)
  "Find whether the two lines, the second expressed as 2 points intersect."
  (lines-intersects-points (from l1) (to l1) l2p1 l2p2))

(defmethod intersects ((l1 <line>) (l2 <line>))
  "Find whether the two lines intersect."
  (lines-intersects-points (from l1) (to l1) (from l2) (to l2)))

(defun line-at-t (l time)
  "Evaluate the line at t where 0<=t<=1 ."
  (make-point :x (+ (x (from l))
                    (* time (- (x (to l))
                               (x (from l)))))
              :y (+ (y (from l))
                    (* time (- (y (to l))
                               (y (from l)))))))

(defun random-point-on-line (l)
  "Generate a random point on a line"
  (line-at-t l (random-number 1.0)))

(defun random-points-on-line (l count)
  "Generate count points on line. These will not be in order."
  (loop for i below count
    collect (random-point-on-line l)))

;; https://math.stackexchange.com/a/2813704

(defun point-line-clockwise-p (p l)
  "Determine whether l is clockwise relative to p."
  (let ((p2 (from l))
        (p3 (to l)))
    ;; Positive = ccw, negative = cw
    (< 0.0
       (- (+ (* (x p) (y p2))
             (* (x p2) (y p3))
             (* (x p3) (y p1)))
          (* (x p2) (y p))
          (* (x p3) (y p2))
          (* (x p1) (y p3))))))

(defun line-normal-left (l)
  "Get the left normal vector/9 o'clock vector."
  (let ((dx (- (x (to l)) (x (from l))))
        (dy (- (y (to l)) (y (from l)))))
    (normalize-vector (make-point :x (- dy)
                                  :y dx))))

(defun line-normal-right (l)
  "Get the right normal vector/3 o'clock vector."
  (let ((dx (- (x (to l)) (x (from l))))
        (dy (- (y (to l)) (y (from l)))))
    (normalize-vector (make-point :x dy
                                  :y (- dx)))))
