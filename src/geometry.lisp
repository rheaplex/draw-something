;; geometry.lisp - Point and line to rect.
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

(defpackage #:draw-something.geometry
  (:use #:cl #:choosing)
  (:nicknames #:geometry)
  (:import-from #:choosing
                #:choose-n-of
                #:random-number)
  (:export
   #:<point>
   #:<line>
   #:<rectangle>
   #:<polyline>
   #:x
   #:y
   #:width
   #:height
   #:bounds
   #:append-point
   #:convex-hull
   #:distance
   #:first-point
   #:highest-leftmost-point
   #:include-point
   #:intersects-none
   #:last-point
   #:make-polyline-from-points
   #:point-count
   #:points))

(in-package #:draw-something.geometry)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-gensyms ((&rest names) &body body)
  "From Peter Siebel's Practical Common Lisp"
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Geometric(ish) values and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +radian+ (* pi 2.0)
  "One radian.")

(defconstant +radians-to-degrees+ (/ +radian+ 360.0)
  "The value to multiple radians by to get degrees.")

(defun radians-to-degrees (radians)
  "Convert a value in radians to a huamn-readable value in degrees. :-)"
  (/ radians +radians-to-degrees+))

(defconstant +radians-to-t-ratio+ (/ 1.0 (* pi 2.0)))

(defun radians-to-t (r)
    "Convert the value in radians to a value from 0.0 to 1.0"
    (* r +radians-to-t-ratio+))

(defun norm-radian (value)
  "Clamp a value to 0..2pi"
  (mod value +radian+))

(defun intersects-any (item others)
  "Returns true if item intersects any of the others"
  (some (lambda (o) (intersects item o)) others))

(defun intersects-none (item others)
  "Returns true if item intersects none of the others"
  (not (intersects-any item others)))

(defun turn-positive-p (a1 a2)
  "Is the shortest turn direction positive (t) or negative (nil)?"
  (> (mod (+ (- a1 a2) +radian+) +radian+) pi))

(defun shortest-angle-difference (a1 a2)
  "Find the shortest positive or negative distance between two angles."
  ;; This was slowly assembled from various online sources, none of which worked
  ;; Normalize angles to 0..2pi
  (let ((from (mod a1 +radian+))
        (to (mod a2 +radian+)))
      ;; If from and to are equal (0 = 2pi radians) the angle is zero
      (if (or (= from to)
              (= (+ from to) +radian+))
          0.0d0
          (let ((angle (- to from)))
            (if (> (abs angle) pi)
                ;; Please simplify me
                (* (- (signum angle)) (- +radian+ (abs angle)))
                angle)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The geometry base class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <geometry> ()
  ()
  (:documentation "The abstract base class for geometric(ish) objects."))

(defgeneric bounds (o)
  (:documentation "The object's bounds, if any."))

(defgeneric area (o)
  (:documentation "The object's area, if any."))

(defgeneric contains (a b)
  (:documentation "Whether the first object contains the second."))

(defgeneric distance (a b)
  (:documentation "The distance between the closest points of the objects."))

(defgeneric highest-leftmost-point (o)
  (:documentation "The object's highest leftmost point (may be calculated)."))

(defgeneric intersects (a b)
  (:documentation "Whether and where the objects intersect."))

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

(defun convex-hull (the-points)
  "Get the convex hull of an array of points."
  (let* ((first-point (highest-leftmost-point-in-list the-points))
         (current-point first-point)
         (next-point nil)
         (hull (make-array 1 :adjustable t :fill-pointer 0)))
    (vector-push-extend first-point hull)
    (loop until (and (not (eq next-point nil))
                     (eq next-point first-point))
          do (setf next-point
                   (point-with-all-left current-point the-points))
          (vector-push-extend next-point hull)
          (setf current-point next-point))
    (make-instance '<polyline> :points hull)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line segments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <line> (<geometry>)
  ((from :accessor from
     :type point
     :initform (make-instance '<point>)
     :initarg :from
     :documentation "The start of the line.")
   (to :accessor to
       :type point
       :initform (make-instance '<point>)
       :initarg :to
       :documentation "The end of the line."))
   (:documentation "A simple line (segment) between two points."))

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
          (make-instance '<point> :x xlb :y ylb)
          ;; else
          (make-instance '<point>
                 :x (+ xla
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
  (make-instance '<point>
         :x (+ (x (from l))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <rectangle> (<geometry>)
  ((x :accessor x
      :type float
      :initform  0.0
      :initarg :x
      :documentation "The lower left x co-ordinate of the rectangle.")
   (y :accessor y
      :type float
      :initform 0.0
      :initarg :y
      :documentation "The lower left y co-ordinate of the rectangle.")
   (width :accessor width
      :type float
      :initform  0.0
      :initarg :width
      :documentation "The width of the rectangle.")
   (height :accessor height
      :type float
      :initform 0.0
      :initarg :height
      :documentation "The height of the rectangle."))
  (:documentation "A simple rectangle"))

(defmethod print-object ((object <rectangle>) stream)
  "Make a human readable string describing the rectangle."
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(X: ~,2f, Y: ~,2f WIDTH: ~,2f HEIGHT: ~,2f)"
          (x object)
          (y object)
          (width object)
          (height object))))

(defun copy-rectangle (r)
  "Make a copy of the rectangle."
  (make-instance '<rectangle>
         :x (x r) :y (y r) :width (width r) :height (height r)))

(defun random-point-in-rectangle (bounds-rect)
  "Make a point placed randomly within the given bounding rectangle."
  (make-instance '<point>
         :x (+ (x bounds-rect) (random-number (width bounds-rect)))
         :y (+ (y bounds-rect) (random-number (height bounds-rect)))))

(defun random-points-in-rectangle (bounds-rect count)
  "Create count points placed randomly within bounds-rect."
  (map-into (make-array count)
        (lambda ()(random-point-in-rectangle bounds-rect))))

(defun random-rectangle-in-rectangle (bounds-rect)
  "Make a random rectangle of at least size 1x1 in another rectangle."
  (let* ((new-width (random-number (width bounds-rect)))
     (new-height (random-number (height bounds-rect)))
     (new-x (+ (x bounds-rect)
           (random-number (- (width bounds-rect) new-width))))
     (new-y (+ (y bounds-rect)
           (random-number (- (height bounds-rect) new-height)))))
    (make-instance '<rectangle>
           :x new-x
           :y new-y
           :width new-width
           :height new-height)))

(defun random-rectangle-in-rectangle-size (in new-width new-height)
  "Make a random rectangle of the given size in the given bounds."
  (assert (<= new-width (width in)))
  (assert (<= new-height (height in)))
  (let ((new-x (+ (x in)
          (random-number (- (width in) new-width))))
    (new-y (+ (y in)
          (random-number (- (height in) new-height)))))
    (make-instance '<rectangle>
           :x new-x
           :y new-y
           :width new-width
           :height new-height)))

(defun inset-rectangle (source offset)
  "Trim a rectangle by the given amount."
  (make-instance '<rectangle>
         :x (+ (x source) offset)
         :y (+ (y source) offset)
         :width (- (width source) (* offset 2.0))
         :height (- (height source) (* offset 2.0))))

(defmethod area ((rect <rectangle>))
  "Get the rectangle's area."
  (* (width rect) (height rect)))

(defun rect-as-points (rect)
  "Get the rectangle as an array of four points"
  (vector (make-instance '<point> :x (x rect) :y (y rect))
      (make-instance '<point> :x (+ (x rect) (width rect)) :y (y rect))
      (make-instance '<point> :x (+ (x rect) (width rect))
                     :y (+ (y rect) (height rect)))
      (make-instance '<point> :x (x rect) :y (+ (y rect) (height rect)))))

(defun contains-point-co-ordinates (rect x y)
  "Find whether the rectangle contains the point."
  (and (>= x (y rect))
       (< x (+ (x rect) (width rect)))
       (>= y (y rect))
       (< y (+ (y rect) (height rect)))))

(defmethod contains ((rect <rectangle>) (p <point>))
  "Find whether the rectangle contains the point."
  (contains-point-co-ordinates rect (x p) (y p)))

(defun points-in-rectangle (rect points)
  "Get the vector of points within the rectangle"
  (let ((contained (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for p across points
      when (contains rect p)
      do (vector-push-extend p contained))
    contained))

(defmethod intersects ((rect1 <rectangle>) (rect2 <rectangle>))
  "Find whether the rectangles intersect."
  (and (< (x rect1) (+ (x rect2) (width rect2)))
       (< (y rect1) (+ (y rect2) (height rect2)))
       (> (+ (x rect1) (width rect1)) (x rect2))
       (> (+ (y rect1) (height rect1)) (y rect2))))

(defun include-point (rect p)
  "Destructively expand the rectangle to include the point."
  (let ((right (+ (x rect) (width rect)))
    (top (+ (y rect) (height rect))))
    (cond
      ((< (x p) (x rect))
       (setf (width rect)
         (+ (width rect) (- (x rect) (x p))))
       (setf (x rect) (x p)))
      ((> (x p) right)
       (setf (width rect)
         (+ (width rect) (- (x p) right))))
      ((< (y p) (y rect))
       (setf (height rect)
         (+ (height rect) (- (y rect) (y p))))
       (setf (y rect) (y p)))
      ((> (y p) top)
       (setf (height rect)
         (+ (height rect) (- (y p) top)))))))

(defun include-rectangle (rect include)
  "Expand the first rectangle to include the second."
  (include-point rect (make-instance '<point> :x (x include) :y (y include)))
  (include-point rect (make-instance '<point> :x (x include)
                                     :y (+ (y include) (height include))))
  (include-point rect (make-instance '<point>
                                     :x (+ (x include) (width include))
                                     :y (+ (y include) (height include))))
  (include-point rect (make-instance '<point> :x (+ (x include) (width include))
                     :y (y include))))

(defun rectangle-from-point (p)
  "Make a zero-size rectangle for a point."
  (make-instance '<rectangle> :x (x p) :y (y p) :width 0.0 :height 0.0))

(defun random-point-on-rectangle (r)
  "Make a random point somewhere on the border of a rectangle."
  (case (random-number 4)
    (0 (random-point-on-line
    (make-instance '<line>
               :from (make-instance '<point>
                        :x (x r)
                        :y (y r))
               :to (make-instance '<point>
                      :x (x r)
                      :y (+ (y r)
                        (height r)
                        -1)))))
    (1 (random-point-on-line
    (make-instance '<line>
               :from (make-instance '<point>
                      :x (x r)
                      :y (+ (y r)
                        (height r)
                        -1))
               :to (make-instance '<point>
                      :x (+ (x r)
                        (width r)
                        -1)
                      :y (+ (y r)
                        (height r)
                        -1)))))
    (2 (random-point-on-line
    (make-instance '<line>
               :from (make-instance '<point>
                      :x (+ (x r)
                        (width r)
                        -1)
                      :y (+ (y r)
                        (height r)
                        -1))
               :to (make-instance '<point>
                      :x (+ (x r)
                        (width r)
                        -1)
                      :y (y r)))))
    (3 (random-point-on-line
    (make-instance '<line>
               :from (make-instance '<point>
                        :x (+ (x r)
                        (width r)
                        -1)
                        :y (y r))
               :to (make-instance '<point>
                      :x (x r)
                      :y (y r)))))))

(defun random-points-on-rectangle (r count)
  "Generate count points on a rectangle's outline. These will not be ordered."
  (map-into (make-array count) (lambda () (random-point-on-rectangle r))))

(defun random-points-at-rectangle-corners (r count)
  "Return from 0 to 4 corner points of a rectangle, clamping out-of-range."
  ;; Inefficient but easy to code and easy to read ;-]
  (choose-n-of count
               (vector (make-instance '<point>
                                      :x (x r)
                                      :y (y r))
                       (make-instance '<point>
                                      :x (x r)
                                      :y (+ (y r) (height r) -1))
                       (make-instance '<point>
                                      :x (+ (x r) (width r) -1)
                                      :y (+ (y r) (height r) -1))
                       (make-instance '<point>
                                      :x (+ (x r) (width r) -1)
                                      :y (y r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polyline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass <polyline> (<geometry>)
  ;; Optimised to use arrays not lists to avoid terrible (distance) consing
  ;; For speed set initial dimension to a size unlikely to need increasing
  ((points :accessor points
           :initform (make-array 1000 :adjustable t :fill-pointer 0)
           :initarg :points
           :documentation "The points of the polyline")
   (bounds :accessor bounds
           :type rectangle
           :initarg :bounds
           :documentation "The bounds of the polyline."))
  (:documentation "A polyline or polygon. A series of joined line segments."))

(defun append-point (poly pt)
  "Append a point to the polyline."
  (vector-push-extend pt (points poly))
  (if (slot-boundp poly 'bounds)
      (include-point (bounds poly) pt)
      (setf (bounds poly) (rectangle-from-point pt))))

(defun point-count (poly)
  "Get the number of points in the polyline"
  (length (points poly)))

(defun first-point (poly)
  "Get the first point of the polyline."
  (aref (points poly) 0))

(defun last-point (poly)
  "Get the last point of the polyline."
  (aref (points poly)
        (- (point-count poly) 1)))

(defun make-random-polyline-in-rectangle (rect count)
  "Create a polyline with the given number of points in the given bounds."
  (let ((poly (make-instance '<polyline>)))
    (dotimes (i count)
      (append-point poly (random-point-in-rectangle rect)))
    poly))

(defun make-polyline-from-points (points)
  "Create a polyline with the given points."
  (let ((poly (make-instance '<polyline>)))
    (loop for p across points
      do (append-point poly p))
    poly))

(defmethod distance ((p <point>) (poly <polyline>))
  "The distance from a point to a polyline."
  (cond ((= (length (points poly)) 0)
         nil) ;; Infinite distance? Zero?
        ((= (length (points poly)) 1)
         (distance p (aref (points poly) 0)))
        (t ;; More than 1 point
         (let ((distance-to-poly nil)
               (pts (points poly)))
           (do ((i 1 (+ i 1)))
               ((= i (length pts)))
             (let ((d (distance-point-line p
                                           (aref pts (- i 1))
                                           (aref pts i))))
               (if (or (not distance-to-poly)
                       (< d distance-to-poly))
                   (setf distance-to-poly d))))
           distance-to-poly))))

(defmethod highest-leftmost-point ((poly <polyline>))
  "The highest point, or highest and leftmost point (if several are highest)."
  (highest-leftmost-point-in-list (points poly)))

(defmethod area ((poly <polyline>))
  "Get the area of the POLYGON"
  ;; Cleanme!
  (if (< (length (points poly)) 3)
      0.0
      (let ((pts (points poly))
            (numpts (length (points poly)))
            (area 0.0))
        (dotimes (i (- numpts 1))
          (let ((j (mod (+ i 1)
                        numpts)))
            (setf area (+ area
                          (* (x (aref pts i))
                             (y (aref pts j)))))
            (setf area (- area
                          (* (y (aref pts i))
                             (x (aref pts j)))))))
        (setf area (/ area 2.0))
        (abs area))))

(defmethod contains ((poly <polyline>) (p <point>))
  "Find whether the POLYGON contains the point."
  ;; Count ray-poly-line intersections. Odd = inside, 0 or even = outside.
  (if (> (length (points poly)) 2)
      (let ((pts (points poly))
            (numpts (length (points poly)))
            (ray-line (make-instance '<line>
                                     :from p
                                     :to (translate-point p 10000.0 0.0)))
            (crossings 0))
        (dotimes (i (- numpts 1))
          (let ((j (mod (+ i 1)
                        numpts)))
            (when (line-intersects-line-points ray-line
                                               (aref pts i)
                                               (aref pts j))
              (setf crossings(+ crossings 1)))))
        (oddp crossings))))

(defgeneric as-polyline (geometry)
  (:documentation "Convert the geometry to a polyline approximation."))

(defmethod as-polyline ((rect <rectangle>))
  "Convert a rectangle into a five-point (closed) polyline"
  (make-instance '<polyline>
         :bounds rect
         :points (vector (make-instance '<point>
                        :x (x rect)
                        :y (y rect))
                 (make-instance '<point>
                        :x (+ (x rect)
                              (width rect))
                        :y (y rect))
                 (make-instance '<point>
                        :x (+ (x rect)
                              (width rect))
                        :y (+ (y rect)
                              (height rect)))
                 (make-instance '<point>
                        :x (x rect)
                        :y (+ (y rect)
                              (height rect)))
                 (make-instance '<point>
                        :x (x rect)
                        :y (y rect)))))

(defmacro do-poly-lines ((sym poly) &body body)
  "Apply fun to each line in the polyline, or not if there is only 1 point."
  (with-gensyms (poly-points previous-point current-point i)
   `(let ((,poly-points (points ,poly)))
      (when (> (point-count ,poly) 1)
        (let ((,previous-point (first-point ,poly))
              (,current-point nil)
              (,sym nil))
          (dotimes (,i (- (point-count ,poly) 1))
            (setf ,current-point (aref ,poly-points (+ ,i 1)))
            (setf ,sym (make-instance '<line>
                                      :from ,previous-point
                                      :to ,current-point))
            ,@body
            (setf ,previous-point ,current-point)))))))

(defmethod intersects ((l <line>) (poly <polyline>))
  "Find whether the line intersects or is contained by the polyline."
  ;; Currently only intersects
  (let ((result nil))
    (do-poly-lines (l2 poly)
      (when (intersects l l2)
        (setf result t)
        (return)))
    result))

(defun polyline-contains-points (poly1 poly2)
  "Find whether poly1 contians any points of poly2"
  ;; Currently only intersects
  (dolist (p (points poly2))
    (when (contains poly1 p)
      (return t))))

(defun polyline-lines-intersects (poly1 poly2)
  "Find whether any lines of the polylines intersect"
  (let ((result nil))
    (block outside-loops
      (do-poly-lines (l1 poly1)
    (do-poly-lines (l2 poly2)
      (when (intersects l1 l2)
        (setf result t)
        (return-from outside-loops)))))
    result))

(defmethod intersects ((poly1 <polyline>) (poly2 <polyline>))
  "Find whether the two POLYGONS intersect or contain each other."
  (and (intersects (bounds poly1) (bounds poly2))
       (or
    ;; If any lines intersect
    (polyline-lines-intersects poly1 poly2)
    ;; If one polyline contains any point of the other
    (some (lambda (p) (contains poly1 p)) (points poly1))
    (some (lambda (p) (contains poly2 p)) (points poly2)))))

(defmethod intersects ((poly <polyline>) (rect <rectangle>))
  "Does the polyline overlap the rectangle?"
  ;; Tests in order of computational expense
  ;; See whether the bounds overlap, then continue
  (and (intersects (bounds poly) rect)
       (or
    ;; If the rectangle contains any point of the polyline
    (some (lambda (p) (contains rect p)) (points poly))
    ;; If any line of the rectangle intersects any line of the polyline
    (intersects poly (as-polyline rect))
    ;; If any point of the square is inside the polyline
    (some (lambda (p) (contains poly p)) (rect-as-points rect)))))

(defun adding-point-would-cause-self-intersection (poly p)
  "Check if adding the point to the polyline would make it self-intersect."
  (let ((l (make-instance '<line>
                          :from (last-point poly)
                          :to p))
        (result nil))
    (do-poly-lines (ll poly)
             (when (intersects l ll)
               (setf result t)
               (break)))
    result))
